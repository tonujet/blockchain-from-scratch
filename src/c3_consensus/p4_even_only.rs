//! In the previous chapter, we considered a hypothetical scenario where blocks must contain an even state root
//! in order to be valid. Now we will express that logic here as a higher-order consensus engine. It is higher-
//! order because it will wrap an inner consensus engine, such as PoW or PoA and work in either case.

use crate::hash;
use super::{Consensus, Header};
use super::p1_pow::{moderate_difficulty_pow, Pow};


/// A Consensus engine that requires the state root to be even for the header to be valid.
/// Wraps an inner consensus engine whose rules will also be enforced.
struct EvenOnly<Inner: Consensus> {
    /// The inner consensus engine that will be used in addition to the even-only requirement.
    inner: Inner,
}

impl<Inner: Consensus> Consensus for EvenOnly<Inner> {
    type Digest = Inner::Digest;

    fn validate(&self, parent_digest: &Self::Digest, header: &Header<Self::Digest>) -> bool {
        if !self.inner.validate(parent_digest, header) {
            return false;
        }
        if header.state_root % 2 != 0 {
            return false;
        }
        true
    }

    fn seal(
        &self,
        parent_digest: &Self::Digest,
        partial_header: Header<()>,
    ) -> Option<Header<Self::Digest>> {
        (partial_header.state_root % 2 == 0).then_some(())?;
        self.inner.seal(parent_digest, partial_header)
    }
}

/// Using the moderate difficulty PoW algorithm you created in section 1 of this chapter as the inner engine,
/// create a PoW chain that is valid according to the inner consensus engine, but is not valid according to
/// this engine because the state roots are not all even.
fn almost_valid_but_not_all_even() -> Vec<Header<u64>> {
    let pow = moderate_difficulty_pow();
    let even_only_pow = EvenOnly {inner: pow};
    
    let g = Header {
        parent: 0,
        height: 0,
        state_root: 1,
        extrinsics_root: hash(&Vec::<u64>::new()),
        consensus_digest: (),
    };
    
    let sealed_g = even_only_pow.seal(&0, g);
    vec![sealed_g.unwrap()]
}


#[test]
#[should_panic]
fn check_valid_but_not_all_even_funtion() {
    let res = almost_valid_but_not_all_even();
    println!("{:?}", res);
}



