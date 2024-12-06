//! PoW and PoA each have their own set of strengths and weaknesses. Many chains are happy to choose
//! one of them. But other chains would like consensus properties that fall in between. To achieve this
//! we could consider interleaving PoW blocks with PoA blocks. Some very early designs of Ethereum considered
//! this approach as a way to transition away from PoW.

/// A Consensus engine that alternates back and forth between PoW and PoA sealed blocks.
///
/// Odd blocks are PoW
/// Even blocks are PoA
struct AlternatingPowPoa {
    pow: Pow,
    poa: SimplePoa,
}

use super::{Consensus, ConsensusAuthority, Header, Pow, SimplePoa};

/// In order to implement a consensus that can be sealed with either work or a signature,
/// we will need an enum that wraps the two individual digest types.
#[derive(Hash, Debug, PartialEq, Eq, Copy, Clone)]
enum PowOrPoaDigest {
    Pow(u64),
    Poa(ConsensusAuthority),
}

impl From<u64> for PowOrPoaDigest {
    fn from(nonce: u64) -> Self {
        PowOrPoaDigest::Pow(nonce)
    }
}

impl TryFrom<PowOrPoaDigest> for u64 {
    type Error = ();

    fn try_from(digest: PowOrPoaDigest) -> Result<Self, Self::Error> {
        match digest {
            PowOrPoaDigest::Pow(nonce) => Ok(nonce),
            PowOrPoaDigest::Poa(_) => Err(()),
        }
    }
}

impl From<ConsensusAuthority> for PowOrPoaDigest {
    fn from(authority: ConsensusAuthority) -> Self {
        PowOrPoaDigest::Poa(authority)
    }
}

impl TryFrom<PowOrPoaDigest> for ConsensusAuthority {
    type Error = ();

    fn try_from(digest: PowOrPoaDigest) -> Result<Self, Self::Error> {
        match digest {
            PowOrPoaDigest::Pow(_) => Err(()),
            PowOrPoaDigest::Poa(authority) => Ok(authority),
        }
    }
}

struct PowOrPoaHeader(Header<PowOrPoaDigest>);
impl<T, E> TryFrom<PowOrPoaHeader> for Header<T>
where
    T: TryFrom<PowOrPoaDigest, Error = E>, 
{
    type Error = E;

    fn try_from(header: PowOrPoaHeader) -> Result<Self, Self::Error> {
        let header = header.0;
        Ok(Header {
            parent: header.parent,
            height: header.height,
            state_root: header.state_root,
            extrinsics_root: header.extrinsics_root,
            consensus_digest: header.consensus_digest.try_into()?,
        })
    }
}

impl From<Header<u64>> for PowOrPoaHeader {
    fn from(header: Header<u64>) -> Self {
        PowOrPoaHeader(Header {
            parent: header.parent,
            height: header.height,
            state_root: header.state_root,
            extrinsics_root: header.extrinsics_root,
            consensus_digest: header.consensus_digest.into(),
        })
    }
}

impl From<Header<ConsensusAuthority>> for PowOrPoaHeader {
    fn from(header: Header<ConsensusAuthority>) -> Self {
        PowOrPoaHeader(Header {
            parent: header.parent,
            height: header.height,
            state_root: header.state_root,
            extrinsics_root: header.extrinsics_root,
            consensus_digest: header.consensus_digest.into(),
        })
    }
}

impl Consensus for AlternatingPowPoa {
    type Digest = PowOrPoaDigest;

    fn validate(&self, parent_digest: &Self::Digest, header: &Header<Self::Digest>) -> bool {
        let parent_digest = *parent_digest;
        let header = header.clone();

        if header.state_root % 2 == 0 {
            let poa_digest = parent_digest
                .try_into()
                .expect("Digest here must be for poa");
            let poa_header = PowOrPoaHeader(header).try_into().expect("Header here must be for poa");
            return self.poa.validate(&poa_digest, &poa_header);
        }

        let pow_digest = parent_digest
            .try_into()
            .expect("Digest here must be for pow");

        let pow_header = PowOrPoaHeader(header).try_into().expect("Header here must be for pow");

        self.pow.validate(&pow_digest, &pow_header)
    }

    fn seal(
        &self,
        parent_digest: &Self::Digest,
        partial_header: Header<()>,
    ) -> Option<Header<Self::Digest>> {
        let parent_digest = *parent_digest;
        let header: PowOrPoaHeader;
        if partial_header.state_root % 2 == 0 {
            let parent_poa_digest = parent_digest
                .try_into()
                .expect("Digest here must be for poa");
            header = self.poa.seal(&parent_poa_digest, partial_header)?.into();
        } else {
            let parent_pow_digest = parent_digest
                .try_into()
                .expect("Digest here must be for pow");
            header = self.pow.seal(&parent_pow_digest, partial_header)?.into();
        };
        Some(header.0)
    }
    
}
