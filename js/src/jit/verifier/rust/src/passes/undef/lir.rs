use std::collections::HashSet;

use crate::ast::lir;
use crate::passes::undef::{DefUseGraph, Set};

impl DefUseGraph for lir::LirGraph {
    type Node = lir::LirNodeIndex;
    type Id = lir::VirtualReg;

    fn uses(&self, n: Self::Node) -> Vec<Self::Id> {
        self[n]
            .operands()
            .iter()
            .filter_map(|a| a.use_info().map(|i| i.virtual_reg()))
            .collect()
    }
    fn definitions(&self, n: Self::Node) -> Vec<Self::Id> {
        self[n]
            .defs()
            .iter()
            .filter_map(|a| a.as_ref().map(lir::LirDefinition::virtual_reg))
            .collect()
    }
    fn predecessors(&self, n: Self::Node) -> Vec<Self::Node> {
        self[n].predecessors().to_owned()
    }
    fn successors(&self, n: Self::Node) -> Vec<Self::Node> {
        self[n].successors().to_owned()
    }
    fn nodes(&self) -> Vec<Self::Node> {
        (0..self.len()).map(lir::LirNodeIndex).collect()
    }
}

#[derive(Debug, Default, PartialEq, Eq, Clone)]
pub struct HashIdSet(pub HashSet<lir::VirtualReg>);

impl Set<lir::VirtualReg> for HashIdSet {
    fn union_with(self, other: &Self) -> Self {
        HashIdSet(self.0.union(&other.0).copied().collect())
    }
    fn insersect_with(self, other: &Self) -> Self {
        HashIdSet(self.0.intersection(&other.0).copied().collect())
    }
    fn add(mut self, item: lir::VirtualReg) -> Self {
        self.0.insert(item);
        self
    }
    fn remove(mut self, item: &lir::VirtualReg) -> Self {
        self.0.remove(&item);
        self
    }
    fn contains(&self, item: &lir::VirtualReg) -> bool {
        self.0.contains(item)
    }
}
