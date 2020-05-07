use std::collections::HashSet;

use bit_vec::BitVec;

use crate::ast::lir;
use crate::passes::undef::{DefUseGraph, Set, UndefUsePass};

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
    fn ids(&self) -> HashSet<lir::VirtualReg> {
        self.iter().flat_map(|n| n.regs()).collect()
    }
    fn is_phi(&self, n: Self::Node) -> bool {
        match self[n].operation() {
            &lir::LirOperation::Phi => true,
            _ => false,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
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
    fn all(items: &HashSet<lir::VirtualReg>) -> Self {
        HashIdSet(items.clone())
    }
    fn none(items: &HashSet<lir::VirtualReg>) -> Self {
        HashIdSet(HashSet::with_capacity(items.len()))
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct BvIdSet(BitVec);

impl Set<lir::VirtualReg> for BvIdSet {
    fn union_with(mut self, other: &Self) -> Self {
        self.0.or(&other.0);
        self
    }
    fn insersect_with(mut self, other: &Self) -> Self {
        self.0.and(&other.0);
        self
    }
    fn add(mut self, item: lir::VirtualReg) -> Self {
        self.0.set(item as usize, true);
        self
    }
    fn remove(mut self, item: &lir::VirtualReg) -> Self {
        self.0.set(*item as usize, false);
        self
    }
    fn contains(&self, item: &lir::VirtualReg) -> bool {
        self.0.get(*item as usize).unwrap_or(false)
    }
    fn all(items: &HashSet<lir::VirtualReg>) -> Self {
        BvIdSet(BitVec::from_elem(*items.iter().max().unwrap_or(&0) as usize + 1, true))
    }
    fn none(items: &HashSet<lir::VirtualReg>) -> Self {
        BvIdSet(BitVec::from_elem(*items.iter().max().unwrap_or(&0) as usize + 1, false))
    }
}

#[allow(dead_code)]
pub type LirUndefUsePassHashSet = UndefUsePass<lir::LirGraph, HashIdSet>;

pub type LirUndefUsePass = UndefUsePass<lir::LirGraph, BvIdSet>;

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::lir::*;
    use crate::passes::undef::*;

    fn link(g: &mut LirGraph, from: u32, to: u32) {
        g[from as usize].push_successor(LirNodeId::from(to + 1));
        g[to as usize].push_predecessor(LirNodeId::from(from + 1));
    }

    fn use_(g: &mut LirGraph, node: u32, reg: u32) {
        g[node as usize].push_operand(LirAllocation::Dynamic(LirDynamicAllocation::new(
            LirUseInfo::new(reg, true, LirUsePolicy::Reg),
        )));
    }

    fn def(g: &mut LirGraph, node: u32, reg: u32) {
        g[node as usize].push_def(Some(LirDefinition::new(
            reg,
            LirType::Int32,
            LirDefinitionPolicy::Reg,
        )));
    }

    #[test]
    fn safe() {
        let lir: LirGraph = vec![LirNode::default(); 1].into_boxed_slice();
        let pass = LirUndefUsePass::from(lir);
        assert!(pass.run().is_ok());
    }

    #[test]
    fn safe_linear() {
        let mut lir: LirGraph = vec![LirNode::default(); 3].into_boxed_slice();
        link(&mut lir, 0, 1);
        link(&mut lir, 1, 2);
        def(&mut lir, 0, 0);
        use_(&mut lir, 2, 0);
        let pass = LirUndefUsePass::from(lir);
        let result = pass.run();
        assert!(result.is_ok());
    }

    #[test]
    fn unsafe_linear() {
        let mut lir: LirGraph = vec![LirNode::default(); 3].into_boxed_slice();
        link(&mut lir, 0, 1);
        link(&mut lir, 1, 2);
        def(&mut lir, 0, 0);
        use_(&mut lir, 2, 0);
        use_(&mut lir, 2, 1);
        let pass = LirUndefUsePass::from(lir);
        let result = pass.run();
        assert!(result.is_err());
    }

    #[test]
    fn safe_fork_join() {
        let mut lir: LirGraph = vec![LirNode::default(); 4].into_boxed_slice();
        link(&mut lir, 0, 1);
        link(&mut lir, 0, 2);
        link(&mut lir, 1, 3);
        link(&mut lir, 2, 3);
        def(&mut lir, 1, 0);
        def(&mut lir, 2, 0);
        use_(&mut lir, 3, 0);
        let pass = LirUndefUsePass::from(lir);
        let result = pass.run();
        assert!(result.is_ok());
    }

    #[test]
    fn safe_fork_join_phi() {
        let mut lir: LirGraph = vec![LirNode::default(); 4].into_boxed_slice();
        link(&mut lir, 0, 1);
        link(&mut lir, 0, 2);
        link(&mut lir, 1, 3);
        link(&mut lir, 2, 3);
        def(&mut lir, 1, 0);
        def(&mut lir, 2, 1);
        lir[3].set_operation(LirOperation::Phi);
        use_(&mut lir, 3, 0);
        use_(&mut lir, 3, 1);
        let pass = LirUndefUsePass::from(lir);
        let result = pass.run();
        if result.is_err() {
            eprintln!("{:#?}", result);
        }
        assert!(result.is_ok());
    }

    #[test]
    fn safe_fork_join_multi_phi() {
        let mut lir: LirGraph = vec![LirNode::default(); 7].into_boxed_slice();
        link(&mut lir, 0, 1);
        link(&mut lir, 0, 2);
        link(&mut lir, 1, 3);
        link(&mut lir, 2, 3);
        link(&mut lir, 3, 4);
        link(&mut lir, 4, 5);
        link(&mut lir, 5, 6);
        def(&mut lir, 1, 0);
        def(&mut lir, 2, 1);
        lir[3].set_operation(LirOperation::Phi);
        use_(&mut lir, 3, 0);
        use_(&mut lir, 3, 1);
        def(&mut lir, 3, 2);
        lir[4].set_operation(LirOperation::Phi);
        use_(&mut lir, 4, 0);
        use_(&mut lir, 4, 1);
        def(&mut lir, 4, 3);
        lir[5].set_operation(LirOperation::Phi);
        use_(&mut lir, 5, 0);
        use_(&mut lir, 5, 1);
        def(&mut lir, 5, 4);
        use_(&mut lir, 6, 4);
        let pass = LirUndefUsePass::from(lir);
        let result = pass.run();
        if result.is_err() {
            eprintln!("{:#?}", result);
        }
        assert!(result.is_ok());
    }

    #[test]
    fn unsafe_fork_join() {
        let mut lir: LirGraph = vec![LirNode::default(); 4].into_boxed_slice();
        link(&mut lir, 0, 1);
        link(&mut lir, 0, 2);
        link(&mut lir, 1, 3);
        link(&mut lir, 2, 3);
        def(&mut lir, 1, 1);
        def(&mut lir, 2, 0);
        use_(&mut lir, 3, 0);
        let pass = LirUndefUsePass::from(lir);
        let result = pass.run();
        assert!(result.is_err());
    }

    #[test]
    fn unsafe_fork_join_hash_set() {
        let mut lir: LirGraph = vec![LirNode::default(); 4].into_boxed_slice();
        link(&mut lir, 0, 1);
        link(&mut lir, 0, 2);
        link(&mut lir, 1, 3);
        link(&mut lir, 2, 3);
        def(&mut lir, 1, 1);
        def(&mut lir, 2, 0);
        use_(&mut lir, 3, 0);
        let pass = LirUndefUsePassHashSet::from(lir);
        let result = pass.run();
        assert!(result.is_err());
    }

    #[test]
    fn safe_loop() {
        let mut lir: LirGraph = vec![LirNode::default(); 4].into_boxed_slice();
        link(&mut lir, 0, 1);
        link(&mut lir, 1, 2);
        link(&mut lir, 1, 3);
        link(&mut lir, 2, 1);
        def(&mut lir, 0, 0);
        use_(&mut lir, 2, 0);
        use_(&mut lir, 3, 0);
        let pass = LirUndefUsePass::from(lir);
        let result = pass.run();
        if result.is_err() {
            eprintln!("{:#?}", result);
        }
        assert!(result.is_ok());
    }

    #[test]
    fn unsafe_loop() {
        let mut lir: LirGraph = vec![LirNode::default(); 4].into_boxed_slice();
        link(&mut lir, 0, 1);
        link(&mut lir, 1, 2);
        link(&mut lir, 1, 3);
        link(&mut lir, 2, 1);
        def(&mut lir, 2, 0);
        use_(&mut lir, 2, 0);
        use_(&mut lir, 3, 0);
        let pass = LirUndefUsePass::from(lir);
        let result = pass.run();
        assert!(result.is_err());
    }
}
