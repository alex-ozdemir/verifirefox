use super::base::Pass;
use crate::{
    ast::lir::{
        typed_ops::{self,*}, LirAllocation, LirNode, LirNodeIndex, LirUseInfo, LirOperation},
    LirGraph,
};
use std::iter::successors;
use crate::match_op;
use std::sync::Arc;
use anyhow::Error;

mod reporting;
pub use reporting::*;

mod array_traits;
pub use array_traits::*;

#[derive(Debug)]
pub struct SpectrePass {
    graph: Arc<LirGraph>,
}

impl SpectrePass {
    pub fn new(graph: Arc<LirGraph>) -> Self {
        Self { graph }
    }
}

impl Pass for SpectrePass {
    /// For each array access, look for the following pattern:
    ///    3. len = ArrayLength(array)
    ///    ...
    ///    ... (no mutation of array length)
    ///    ...
    ///    2. access_idx = SpectreMaskIndex(index, len)
    ///    ...
    ///    ... (no mutation of array length or access_idx) 
    ///    ...
    ///    1. ArrayAccess(array, access_idx)
    /// 
    fn run(&self) -> Result<(), Error> {
        let access_nodes = (*self.graph).iter().filter_map(array_access);
        for access in access_nodes {
            // 
            if access.index() == &LirAllocation::Constant || access.index() == &LirAllocation::Bogus {
                continue;
            }

            let smi = spectre_for_access(&self.graph, access).ok_or_else(||
                SpectrePassError::AccessWithoutMask {
                    access: access.node().into()
                }
            )?;

            let length_def = length_def_loc(&self.graph, smi).ok_or_else(||
                SpectrePassError::LengthDerivationNotFound{
                    access: access.node().into(),
                    spectre: smi.node().into(),
                }
            )?;

            let length_from_array = array_length_access(length_def).ok_or_else(||
                SpectrePassError::LengthDerivedFromNonArray{
                    access: access.node().into(),
                    spectre: smi.node().into(),
                    length_src: length_def.into()
                }
            )?;

            if !same_array(length_from_array.array(), access.array()) {
                return Err(SpectrePassError::LengthDerivedFromDifferentArray {
                    access: access.node().into(),
                    spectre: smi.node().into(),
                    length_src: length_def.into(),
                    array: length_from_array.array().clone(),
                })?
            }


        }
        Ok(())
    }
}


fn same_array(left: &LirAllocation, right: &LirAllocation) -> bool {
    let left_reg = left.use_info().map(LirUseInfo::virtual_reg);
    let right_reg = right.use_info().map(LirUseInfo::virtual_reg);
    match (left_reg, right_reg) {
        (Some(l), Some(r)) => l == r,
        _ => false
    }
}

fn length_def_loc<'a>(graph: &'a LirGraph, smi: &'a SpectreMaskIndex) -> Option<&'a LirNode> {
    def_location(graph, smi.node(), smi.length().use_info()?)
}

fn spectre_for_access<'a>(graph: &'a LirGraph, access: &'a dyn ArrayAccess) -> Option<&'a SpectreMaskIndex> {
    let masked_idx = access.index();
    let masked_idx_def = def_location(graph, access.node(), masked_idx.use_info()?)?;
    SpectreMaskIndex::from_node(masked_idx_def)
}


fn predecessors_in_block<'a>(graph: &'a LirGraph, node: &'a LirNode) -> impl Iterator<Item=&'a LirNode> {
    successors(Some(node), move |node| {
        match &node.predecessors() {
            [idx] => idx.get(graph),
            _ => None
        }
    })
}

fn def_location<'a>(graph: &'a LirGraph, node: &'a LirNode, lir_use: &'a LirUseInfo) -> Option<&'a LirNode> {
    predecessors_in_block(graph, node).find(|pred| {
        pred.defs().iter().filter_map(|id| id.as_ref()).any(|def| 
            def.virtual_reg() == lir_use.virtual_reg()
        )
    })
}

