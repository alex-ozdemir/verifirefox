use super::base::Pass;
use crate::{
    ast::lir::{
        typed_ops::{self}, LirAllocation, LirOperation, LirNode, LirUseInfo},
    LirGraph,
};
use std::iter::successors;
use crate::match_op;
use std::sync::Arc;
use anyhow::Error;
use std::collections::{HashSet, VecDeque};

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
    ///    4.. (no mutation of array length)
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
            
            // Firefox doesn't insert SMI's for constant indices. Is this okay? 
            if access.index() == &LirAllocation::Constant || access.index() == &LirAllocation::Bogus {
                continue;
            }

            // 1. Is there a spectre mitigation at all
            let smi = spectre_for_access(&self.graph, access).ok_or_else(||
                SpectrePassError::AccessWithoutMask {
                    access: access.node().into()
                }
            )?;

            // 2. Can we find the origin of length in the mitigation
            let length_def = length_def_loc(&self.graph, smi).ok_or_else(||
                SpectrePassError::LengthDerivationNotFound{
                    access: access.node().into(),
                    spectre: smi.node().into(),
                }
            )?;

            // 2. Is the length actually derived from an array
            let length_from_array = array_length_access(length_def).ok_or_else(||
                SpectrePassError::LengthDerivedFromNonArray{
                    access: access.node().into(),
                    spectre: smi.node().into(),
                    length_src: length_def.into()
                }
            )?;

            // 3. Is the length derived from the same array as the access?
            if !length_matches_array_access(&self.graph, length_from_array, access) {
                Err(SpectrePassError::LengthDerivedFromDifferentArray {
                    access: access.node().into(),
                    spectre: smi.node().into(),
                    length_src: length_def.into(),
                    array: length_from_array.array().clone(),
                })?
            }

            // 4. Has the array length changed since the length was taken?
            if let Some(mut_node) = intermediary_mutation(&self.graph, access, length_from_array) {
               Err(SpectrePassError::ArrayModifiedBetweenLengthAndAccess {
                   access: access.node().into(),
                   spectre: smi.node().into(),
                   length_src: length_def.into(),
                   array: length_from_array.array().clone(),
                   modified: mut_node.into(),
               })?
            }


        }
        Ok(())
    }
}

/// Finds any mutation of the array after the length is taken and before the array is accessed
fn intermediary_mutation<'a>(graph: &'a LirGraph, access_node: &'a dyn ArrayAccess, length_node: &'a dyn ArrayLengthAccess) -> Option<&'a LirNode> {
    predecessors(graph, access_node.node()).take_while(|node| node.index() != length_node.node().index()).find(|node| mutates_array(node, access_node.array()))
}

fn mutates_array(node: &LirNode, array: &LirAllocation) -> bool {
    let mutated_array = match_op! {
        match node {
            typed_ops::ArrayPopShiftT(it) => it.object(),
            typed_ops::ArrayPopShiftV(it) => it.object(),
            typed_ops::ArrayPushV(it) => it.array(),
            typed_ops::ArrayPushT(it) => it.array(),
            typed_ops::SetInitializedLength(it) => it.array(),
            _ => return false,
        }
    };

    same_array(mutated_array, array)

}

fn same_array(left: &LirAllocation, right: &LirAllocation) -> bool {
    let left_reg = left.use_info().map(LirUseInfo::virtual_reg);
    let right_reg = right.use_info().map(LirUseInfo::virtual_reg);
    match (left_reg, right_reg) {
        (Some(l), Some(r)) => l == r,
        _ => false
    }
}

fn length_def_loc<'a>(graph: &'a LirGraph, smi: &'a typed_ops::SpectreMaskIndex) -> Option<&'a LirNode> {
    def_location(graph, smi.node(), smi.length())
}

fn spectre_for_access<'a>(graph: &'a LirGraph, access: &'a dyn ArrayAccess) -> Option<&'a typed_ops::SpectreMaskIndex> {
    let masked_idx = access.index();
    let masked_idx_def = def_location(graph, access.node(), masked_idx)?;
    typed_ops::SpectreMaskIndex::from_node(masked_idx_def)
}


fn predecessors<'a>(graph: &'a LirGraph, node: &'a LirNode) -> impl Iterator<Item=&'a LirNode> {
    let mut visited = HashSet::new();
    let mut queue = VecDeque::new();

    successors(Some(node), move |node| {
        visited.insert(node.index());
        let new_preds = node.predecessors().into_iter().filter(|idx| !visited.contains(idx));

        for pred in new_preds{
            queue.push_back(*pred);
        }
        
        queue.pop_front().map(|idx| &graph[idx])
    })
}

// Accounts for TypedArray which contains an elements array
fn length_matches_array_access<'a>(graph: &'a LirGraph, length: &'a dyn ArrayLengthAccess, access: &'a dyn ArrayAccess) -> bool {
    // TypedArrayLength operates on a TypedArray object while accesses are
    // performed through an elements object extracted by TypedArrayElements, so
    // if we're dealing with a typed array, we need to match the elements back
    // to the object
    if *length.node().operation() == LirOperation::TypedArrayLength {
        let array_def = match def_location(graph, access.node(), access.array()) {
            Some(it) => it,
            None => return false,
        };
        if let Some(typed_array_elem) = typed_ops::TypedArrayElements::from_node(array_def) {
            same_array(typed_array_elem.obj(), length.array())
        } else {
            false
        }
    } else {
        same_array(length.array(),access.array())
    }
}

fn def_location<'a>(graph: &'a LirGraph, node: &'a LirNode, alloc: &'a LirAllocation) -> Option<&'a LirNode> {
    let use_info = alloc.use_info()?;
    predecessors(graph, node).find(|pred| {
        pred.defs().iter().filter_map(|id| id.as_ref()).any(|def| 
            def.virtual_reg() == use_info.virtual_reg()
        )
    })
}

