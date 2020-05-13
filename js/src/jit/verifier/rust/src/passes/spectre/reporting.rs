use thiserror::Error;

use crate::ast::lir::{LirNodeIndex, LirOperation, LirNode, LirAllocation};

#[derive(Debug)]
pub struct NodeInfo(LirNodeIndex, LirOperation);

impl From<&'_ LirNode> for NodeInfo {
    fn from(node: &LirNode) -> NodeInfo {
        NodeInfo(node.index(), node.operation().clone())
    }
}
/// Describes any reason the SpectrePass may fail
#[derive(Error, Debug)]
pub enum SpectrePassError {
    #[error("Access node {access:?} uses an unmasked index")]
    AccessWithoutMask {
        access: NodeInfo
    },

    #[error("Unable to find definition of length used in {spectre:?} for access {access:?}")]
    LengthDerivationNotFound {
        access: NodeInfo,
        spectre: NodeInfo
    },

    /// Note: This error does not mean the access is unsafe, but we can't prove that it is
    #[error("Length used in {spectre:?} for access {access:?} is not an array length. It is: {length_src:?}")]
    LengthDerivedFromNonArray {
        access: NodeInfo,
        spectre: NodeInfo,
        length_src: NodeInfo,
    },

    #[error("Length used in {spectre:?} for access {access:?} is derived from a different array {array:?} at {length_src:?}")]
    LengthDerivedFromDifferentArray {
        access: NodeInfo,
        spectre: NodeInfo,
        length_src: NodeInfo,
        array: LirAllocation,
    }
}


pub type SpectreResult<T> = Result<T, SpectrePassError>;

