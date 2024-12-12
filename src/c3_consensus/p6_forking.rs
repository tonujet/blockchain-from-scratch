//! We saw in the previous chapter that blockchain communities sometimes opt to modify the
//! consensus rules from time to time. This process is knows as a fork. Here we implement
//! a higher-order consensus engine that allows such forks to be made.
//!
//! The consensus engine we implement here does not contain the specific consensus rules to
//! be enforced before or after the fork, but rather delegates to existing consensus engines
//! for that. Here we simply write the logic for detecting whether we are before or after the fork.

use super::Hash;
use crate::hash;
use core::fmt::{Debug, Display};

#[derive(Debug)]
struct MyError {
    message: String,
}

impl Display for MyError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "MyError: {}", self.message)
    }
}

impl std::error::Error for MyError {}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Header<Digest> {
    parent: Hash,
    height: u64,
    state_root: Hash,
    extrinsics_root: Hash,
    consensus_digest: Option<Digest>,
}

#[derive(Debug, Clone, Eq, std::hash::Hash, PartialEq)]
enum GeneralDigest {
    PoA(Authority),
    PoW(PowDigest),
}
type PowDigest = u64;

#[derive(PartialEq, Eq, Clone, Debug, Hash)]
enum Authority {
    Anton,
    Oleg,
    Bob,
}

impl From<Authority> for GeneralDigest {
    fn from(digest: Authority) -> Self {
        GeneralDigest::PoA(digest)
    }
}

impl From<PowDigest> for GeneralDigest {
    fn from(digest: PowDigest) -> Self {
        GeneralDigest::PoW(digest)
    }
}

impl TryFrom<GeneralDigest> for PowDigest {
    type Error = ();

    fn try_from(digest: GeneralDigest) -> Result<Self, Self::Error> {
        match digest {
            GeneralDigest::PoA(_) => Err(()),
            GeneralDigest::PoW(digest) => Ok(digest),
        }
    }
}

impl TryFrom<GeneralDigest> for Authority {
    type Error = ();

    fn try_from(digest: GeneralDigest) -> Result<Self, Self::Error> {
        match digest {
            GeneralDigest::PoA(authority) => Ok(authority),
            GeneralDigest::PoW(_) => Err(()),
        }
    }
}

trait Consensus
where
    Self::Digest: Debug + Clone + Eq + std::hash::Hash,
{
    type Digest;
    fn validate(&self, parent_digest: &Self::Digest, header: &Header<Self::Digest>) -> bool;
    fn seal(
        &self,
        parent_digest: &Self::Digest,
        partial_header: Header<Self::Digest>,
    ) -> Option<Header<Self::Digest>>;

    fn human_mane(&self) -> &'static str {
        "Unnamed consensus"
    }
}

#[derive(Clone)]
struct Pow {
    threshold: u64,
}

impl Consensus for Pow {
    type Digest = PowDigest;

    fn validate(&self, _: &Self::Digest, header: &Header<Self::Digest>) -> bool {
        if header.consensus_digest.is_none() {
            return false;
        }
        hash(header) < self.threshold
    }

    fn seal(
        &self,
        parent_digest: &Self::Digest,
        mut partial_header: Header<Self::Digest>,
    ) -> Option<Header<Self::Digest>> {
        while !(self.validate(parent_digest, &partial_header)) {
            let digest = partial_header.consensus_digest.take().map_or(0, |d| d + 1);
            partial_header.consensus_digest = Some(digest);
        }
        Some(partial_header)
    }

    fn human_mane(&self) -> &'static str {
        "Proof of work"
    }
}

#[derive(Clone)]
struct Poa {
    authorities: Vec<Authority>,
}

impl Consensus for Poa {
    type Digest = Authority;

    fn validate(&self, _: &Self::Digest, header: &Header<Self::Digest>) -> bool {
        if let Some(authority) = &header.consensus_digest {
            return self.authorities.contains(authority);
        }
        false
    }

    fn seal(
        &self,
        parent_digest: &Self::Digest,
        mut partial_header: Header<Self::Digest>,
    ) -> Option<Header<Self::Digest>> {
        if self.validate(parent_digest, &partial_header) {
            None?
        }

        if self.authorities.is_empty() {
            None?;
        }

        let now = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap();
        let nanos = now.subsec_nanos();
        let i = (nanos % self.authorities.len() as u32) as usize;
        let _ = partial_header
            .consensus_digest
            .insert(self.authorities[i].clone());
        Some(partial_header)
    }

    fn human_mane(&self) -> &'static str {
        "Proof of state"
    }
}

/// A Higher-order consensus engine that represents a change from one set of consensus rules (Before) to
/// another set (After) at a specific block height
struct ConsensusWithForks<'a, Digest>
where
    Digest: Debug + Clone + Eq + std::hash::Hash,
{
    fork_rules: Vec<ForkRule<'a, Digest>>,
}

struct ForkRule<'a, Digest>
where
    Digest: Debug + PartialEq + Eq + std::hash::Hash,
{
    id: usize,
    fork_height: u64,
    consensus: Box<dyn Consensus<Digest = Digest> + 'a>,
}

impl<'a, Digest> Consensus for ConsensusWithForks<'a, Digest>
where
    Digest: Debug + Clone + Eq + std::hash::Hash,
{
    type Digest = Digest;

    fn validate(&self, parent_digest: &Self::Digest, header: &Header<Self::Digest>) -> bool {
        let consensus = self.get_consensus(header.height);
        consensus.validate(parent_digest, header)
    }

    fn seal(
        &self,
        parent_digest: &Self::Digest,
        partial_header: Header<Self::Digest>,
    ) -> Option<Header<Self::Digest>> {
        let consensus = self.get_consensus(partial_header.height);
        consensus.seal(parent_digest, partial_header)
    }

    fn human_mane(&self) -> &'static str {
        "Consensus with multiple forks"
    }
}

impl<'a, Digest> ConsensusWithForks<'a, Digest>
where
    Digest: Debug + Clone + Eq + std::hash::Hash,
{
    fn get_consensus(&self, height: u64) -> &dyn Consensus<Digest = Digest> {
        let mut consensus = self
            .fork_rules
            .last()
            .expect("At least one rule must be in the vector")
            .consensus
            .as_ref();

        for window in self.fork_rules.windows(2) {
            let [curr, next] = window else { continue };
            if curr.fork_height >= height || next.fork_height < height {
                consensus = curr.consensus.as_ref();
                break;
            }
        }

        consensus
    }
    pub fn push_fork(
        &mut self,
        height: u64,
        consensus: impl Consensus<Digest = Digest> + 'a,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let is_smaller_height = self
            .fork_rules
            .last()
            .map(|r| r.fork_height)
            .filter(|h| height < *h)
            .is_some();

        if is_smaller_height {
            Err(Box::new(MyError {
                message: "Height is not enough".to_string(),
            }))?
        }

        let rule = ForkRule {
            id: self.fork_rules.last().map_or(0, |r| r.id + 1),
            fork_height: height,
            consensus: Box::new(consensus),
        };

        self.fork_rules.push(rule);
        Ok(())
    }

    pub fn new(consensus: impl Consensus<Digest = Digest> + 'a) -> Self {
        let mut instance = Self { fork_rules: vec![] };
        instance
            .push_fork(0, consensus)
            .expect("This first rule push must always not panic");
        instance
    }
}

/// Create a PoA consensus engine that changes authorities part way through the chain's history.
/// Given the initial authorities, the authorities after the fork, and the height at which the fork occurs.
fn change_authorities(
    fork_height: u64,
    initial_authorities: Vec<Authority>,
    final_authorities: Vec<Authority>,
) -> Result<impl Consensus, Box<dyn std::error::Error>> {
    let init_poa = Poa {
        authorities: initial_authorities,
    };
    let changed_poa = Poa {
        authorities: final_authorities,
    };
    let mut change_authorities_consensus = ConsensusWithForks::new(init_poa);
    change_authorities_consensus.push_fork(fork_height, changed_poa)?;
    Ok(change_authorities_consensus)
}

/// Create a PoW consensus engine that changes the difficulty part way through the chain's history.
fn change_difficulty(
    fork_height: u64,
    initial_difficulty: u64,
    final_difficulty: u64,
) -> Result<impl Consensus, Box<dyn std::error::Error>> {
    let init_pow = Pow {
        threshold: initial_difficulty,
    };
    let next_pow = Pow {
        threshold: final_difficulty,
    };
    let mut forked_consensus = ConsensusWithForks::new(init_pow);
    forked_consensus.push_fork(fork_height, next_pow)?;
    Ok(forked_consensus)
}

/// Earlier in this chapter we implemented a consensus rule in which blocks are only considered valid if
/// they contain an even state root. Sometimes a chain will be launched with a more traditional consensus like
/// PoW or PoA and only introduce an additional requirement like even state root after a particular height.
///
/// Create a consensus engine that introduces the even-only logic only after the given fork height.
/// Other than the evenness requirement, the consensus rules should not change at the fork. This function
/// should work with either PoW, PoA, or anything else as the underlying consensus engine.

struct EvenConsensusWrapper<Wrapped>
where
    Wrapped: Consensus,
{
    consensus: Wrapped,
}

impl<Wrapped> Consensus for EvenConsensusWrapper<Wrapped>
where
    Wrapped: Consensus,
{
    type Digest = Wrapped::Digest;

    fn validate(&self, parent_digest: &Self::Digest, header: &Header<Self::Digest>) -> bool {
        self.consensus.validate(parent_digest, header) && header.state_root % 2 == 0
    }

    fn seal(
        &self,
        parent_digest: &Self::Digest,
        partial_header: Header<Self::Digest>,
    ) -> Option<Header<Self::Digest>> {
        if !self.validate(parent_digest, &partial_header) {
            return None;
        }

        self.consensus.seal(parent_digest, partial_header)
    }

    fn human_mane(&self) -> &'static str {
        "Even wrapper "
    }
}

fn even_after_given_height<Original: Consensus>(
    fork_height: u64,
) -> Result<impl Consensus, Box<dyn std::error::Error>> {
    let pow = Pow {
        threshold: u64::MAX / 100,
    };
    let even_pow = EvenConsensusWrapper {
        consensus: pow.clone(),
    };
    let mut forked_consensus = ConsensusWithForks::new(pow);
    forked_consensus.push_fork(fork_height, even_pow)?;
    Ok(forked_consensus)
}

/// In the spirit of Ethereum's recent switch from PoW to PoA, let us model a similar
/// switch in our consensus framework. It should go without saying that the real-world ethereum
/// handoff was considerably more complex than it may appear in our simplified example, although
/// the fundamentals are the same.
/// For this task, you may use the PowOrPoaDigest type from the previous module if you like.
fn pow_to_poa(
    fork_height: u64,
    difficulty: u64,
    authorities: Vec<Authority>,
) -> Result<impl Consensus, Box<dyn std::error::Error>> {
    let pow = Pow {
        threshold: difficulty,
    };

    let general_pow = GeneralizedConsensus::<_, GeneralDigest>::new(pow);

    let poa = Poa { authorities };
    let general_poa = GeneralizedConsensus::<_, GeneralDigest>::new(poa);

    let mut forked_consensus = ConsensusWithForks::new(general_pow);
    forked_consensus
        .push_fork(fork_height, general_poa)
        .map(|_| forked_consensus)
}

trait IntoVagueHeader<T> {
    fn into_vague_header(self) -> Header<T>;
}

impl<T, U> IntoVagueHeader<U> for Header<T>
where
    T: TryInto<U>
{
    fn into_vague_header(self) -> Header<U> {
        Header {
            parent: self.parent,
            height: self.height,
            state_root: self.state_root,
            extrinsics_root: self.extrinsics_root,
            consensus_digest: self.consensus_digest.map(|d| d.try_into().ok()).flatten(),
        }
    }
}

trait IntoHeader<T> {
    fn into_header(self) -> Header<T>;
}

impl<T, U> IntoHeader<U> for Header<T>
where
    T: Into<U>,
{
    fn into_header(self) -> Header<U> {
        Header {
            parent: self.parent,
            height: self.height,
            state_root: self.state_root,
            extrinsics_root: self.extrinsics_root,
            consensus_digest: self.consensus_digest.map(|d| d.into()),
        }
    }
}

#[derive(Clone)]
struct GeneralizedConsensus<WrappedConsensus, GeneralDigest>
where
    WrappedConsensus::Digest: Into<GeneralDigest>,
    WrappedConsensus: Consensus,
    GeneralDigest: TryInto<WrappedConsensus::Digest>,
{
    consensus: WrappedConsensus,
    general_digest: std::marker::PhantomData<GeneralDigest>,
}


impl <WrappedConsensus, GeneralDigest> GeneralizedConsensus<WrappedConsensus, GeneralDigest>
where
    WrappedConsensus::Digest: Into<GeneralDigest>,
    WrappedConsensus: Consensus,
    GeneralDigest: TryInto<WrappedConsensus::Digest> {
    fn new(consensus: WrappedConsensus) -> Self{
        Self {
            consensus,
            general_digest: Default::default(),
        }
    }
}
    

impl<WrappedConsensus, GeneralDigest> Consensus
    for GeneralizedConsensus<WrappedConsensus, GeneralDigest>
where
    WrappedConsensus::Digest: Into<GeneralDigest>,
    WrappedConsensus: Consensus,
    GeneralDigest: TryInto<WrappedConsensus::Digest> + Clone + Eq + std::hash::Hash + Debug,
{
    type Digest = GeneralDigest;

    fn validate(&self, parent_digest: &Self::Digest, header: &Header<Self::Digest>) -> bool {
        let Ok(parent_digest) = parent_digest.clone().try_into() else {
            return false;
        };
        let header = header.clone().into_vague_header();
        self.consensus.validate(&parent_digest, &header)
    }

    fn seal(
        &self,
        parent_digest: &Self::Digest,
        partial_header: Header<Self::Digest>,
    ) -> Option<Header<Self::Digest>> {
        let Ok(parent_digest) = parent_digest.clone().try_into() else {
            return None;
        };
        let header = partial_header.clone().into_vague_header();
        let sealed = self.consensus.seal(&parent_digest, header)?;

        Some(sealed.into_header())
    }

    fn human_mane(&self) -> &'static str {
        self.human_mane()
    }
}
