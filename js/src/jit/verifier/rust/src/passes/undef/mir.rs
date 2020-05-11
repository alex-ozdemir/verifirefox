use std::collections::HashSet;

use crate::ast::mir;
use crate::passes::undef::lir::BvIdSet;
use crate::passes::undef::{DefUseGraph, UndefUsePass};

impl DefUseGraph for mir::MirGraph {
    // (block idx, is phi, instr/phi idx)
    type Node = (u32, bool, u32);
    type Id = mir::MirDefId;

    fn uses(&self, (bb_idx, is_phi, idx): Self::Node) -> Vec<Self::Id> {
        self[bb_idx as usize]
            .get_instr(is_phi, idx)
            .unwrap()
            .map(|p| p.inputs.iter())
            .unwrap_or_else(|i| i.inputs.iter())
            .copied()
            .collect()
    }
    fn definitions(&self, (bb_idx, is_phi, idx): Self::Node) -> Vec<Self::Id> {
        vec![self[bb_idx as usize]
            .get_instr(is_phi, idx)
            .unwrap()
            .map(|p| p.output)
            .unwrap_or_else(|i| i.output)]
    }
    fn predecessors(&self, (bb_idx, is_phi, idx): Self::Node) -> Vec<Self::Node> {
        let block = &self[bb_idx as usize];
        if idx == 0 && (is_phi || block.phis.len() == 0) {
            block
                .predecessors
                .iter()
                .cloned()
                .map(|bb_idx_p| {
                    let pred = &self[bb_idx_p];
                    if pred.instructions.len() > 0 {
                        (bb_idx_p.0 as u32, false, pred.instructions.len() as u32 - 1)
                    } else {
                        assert!(pred.phis.len() > 0);
                        (bb_idx_p.0 as u32, true, pred.phis.len() as u32 - 1)
                    }
                })
                .collect()
        } else if is_phi {
            vec![(bb_idx, is_phi, idx - 1)]
        } else if block.phis.len() == 0 {
            vec![(bb_idx, is_phi, idx - 1)]
        } else {
            vec![(bb_idx, true, block.phis.len() as u32 - 1)]
        }
    }
    fn successors(&self, (bb_idx, is_phi, idx): Self::Node) -> Vec<Self::Node> {
        let block = &self[bb_idx as usize];
        if (is_phi && idx + 1 == block.phis.len() as u32 && block.instructions.len() == 0)
            || (!is_phi && idx + 1 == block.instructions.len() as u32)
        {
            block
                .successors
                .iter()
                .cloned()
                .map(|bb_idx_p| (bb_idx_p.0 as u32, self[bb_idx_p].phis.len() > 0, 0))
                .collect()
        } else if !is_phi {
            vec![(bb_idx, is_phi, idx + 1)]
        } else if idx as usize + 1 == block.phis.len() {
            vec![(bb_idx, false, 0)]
        } else {
            vec![(bb_idx, is_phi, idx + 1)]
        }
    }
    fn nodes(&self) -> Vec<Self::Node> {
        let mut out = Vec::new();
        for (i, b) in self.iter().enumerate() {
            for j in 0..(b.phis.len()) {
                out.push((i as u32, true, j as u32))
            }
            for j in 0..(b.instructions.len()) {
                out.push((i as u32, false, j as u32))
            }
        }
        out
    }
    fn ids(&self) -> HashSet<mir::MirDefId> {
        let mut out = HashSet::new();
        for b in self.iter() {
            for p in b.phis.iter() {
                out.insert(p.output);
                out.extend(p.inputs.iter());
            }
            for i in b.instructions.iter() {
                out.insert(i.output);
                out.extend(i.inputs.iter());
            }
        }
        out
    }
    fn is_phi(&self, (_, is_phi, _): Self::Node) -> bool {
        is_phi
    }
    fn is_block_start(&self, (bb_idx, is_phi, idx): Self::Node) -> bool {
        let b = &self[bb_idx as usize];
        if b.phis.len() > 0 {
            is_phi && idx == 0
        } else {
            assert!(!is_phi);
            idx == 0
        }
    }
}

pub type MirUndefUsePass = UndefUsePass<mir::MirGraph, BvIdSet>;

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::mir::*;
    use crate::passes::undef::*;

    fn assert_has_error_str(e: &Error, s: &str) {
        assert!(format!("{}", e).contains(s), "No '{}' in error '{}'", s, e)
    }

    fn assert_undef_err(result: &Result<(), Error>) {
        assert!(result.is_err());
        if let Err(e) = result.as_ref() {
            assert_has_error_str(e, "Use of undef")
        }
    }

    fn link(g: &mut MirGraph, from: usize, to: usize) {
        g[from as usize].push_successor(MirBasicBlockIndex::from(to));
        g[to as usize].push_predecessor(MirBasicBlockIndex::from(from));
    }

    #[test]
    fn safe() {
        let mut mir: MirGraph = vec![MirBasicBlock::default(); 1].into_boxed_slice();
        mir[0].phis.push(MirPhi {
            output: 0,
            inputs: vec![],
        });
        let pass = MirUndefUsePass::from(mir);
        let result = pass.run();
        if result.is_err() {
            eprintln!("{:#?}", result);
        }
        assert!(result.is_ok());
    }

    #[test]
    fn safe_linear() {
        let mut mir: MirGraph = vec![MirBasicBlock::default(); 1].into_boxed_slice();
        mir[0].instructions.push(MirInstruction {
            operation: MirOperation::Other,
            output: 0,
            inputs: vec![],
        });
        mir[0].instructions.push(MirInstruction {
            operation: MirOperation::Other,
            output: 0,
            inputs: vec![0],
        });
        let pass = MirUndefUsePass::from(mir);
        let result = pass.run();
        if result.is_err() {
            eprintln!("{:#?}", result);
        }
        assert!(result.is_ok());
    }

    #[test]
    fn unsafe_linear() {
        let mut mir: MirGraph = vec![MirBasicBlock::default(); 1].into_boxed_slice();
        mir[0].instructions.push(MirInstruction {
            operation: MirOperation::Other,
            output: 0,
            inputs: vec![],
        });
        mir[0].instructions.push(MirInstruction {
            operation: MirOperation::Other,
            output: 0,
            inputs: vec![0],
        });
        mir[0].instructions.push(MirInstruction {
            operation: MirOperation::Other,
            output: 0,
            inputs: vec![1],
        });
        let pass = MirUndefUsePass::from(mir);
        let result = pass.run();
        assert_undef_err(&result);
    }

    #[test]
    fn safe_fork_join() {
        let mut mir: MirGraph = vec![MirBasicBlock::default(); 4].into_boxed_slice();
        mir[0].instructions.push(MirInstruction {
            operation: MirOperation::Other,
            output: 0,
            inputs: vec![],
        });
        mir[1].instructions.push(MirInstruction {
            operation: MirOperation::Other,
            output: 1,
            inputs: vec![0],
        });
        mir[2].instructions.push(MirInstruction {
            operation: MirOperation::Other,
            output: 1,
            inputs: vec![0],
        });
        mir[3].instructions.push(MirInstruction {
            operation: MirOperation::Other,
            output: 2,
            inputs: vec![1],
        });
        link(&mut mir, 0, 1);
        link(&mut mir, 0, 2);
        link(&mut mir, 1, 3);
        link(&mut mir, 2, 3);
        let pass = MirUndefUsePass::from(mir);
        let result = pass.run();
        assert!(result.is_ok());
    }

    #[test]
    fn safe_fork_join_phi() {
        let mut mir: MirGraph = vec![MirBasicBlock::default(); 4].into_boxed_slice();
        mir[0].instructions.push(MirInstruction {
            operation: MirOperation::Other,
            output: 0,
            inputs: vec![],
        });
        mir[1].instructions.push(MirInstruction {
            operation: MirOperation::Other,
            output: 1,
            inputs: vec![0],
        });
        mir[2].instructions.push(MirInstruction {
            operation: MirOperation::Other,
            output: 2,
            inputs: vec![0],
        });
        mir[3].phis.push(MirPhi {
            output: 3,
            inputs: vec![1, 2],
        });
        link(&mut mir, 0, 1);
        link(&mut mir, 0, 2);
        link(&mut mir, 1, 3);
        link(&mut mir, 2, 3);
        let pass = MirUndefUsePass::from(mir);
        let result = pass.run();
        assert!(result.is_ok());
    }

    #[test]
    fn safe_fork_join_multi_phi() {
        let mut mir: MirGraph = vec![MirBasicBlock::default(); 4].into_boxed_slice();
        mir[0].instructions.push(MirInstruction {
            operation: MirOperation::Other,
            output: 0,
            inputs: vec![],
        });
        mir[1].instructions.push(MirInstruction {
            operation: MirOperation::Other,
            output: 1,
            inputs: vec![0],
        });
        mir[1].instructions.push(MirInstruction {
            operation: MirOperation::Other,
            output: 3,
            inputs: vec![0],
        });
        mir[2].instructions.push(MirInstruction {
            operation: MirOperation::Other,
            output: 2,
            inputs: vec![0],
        });
        mir[2].instructions.push(MirInstruction {
            operation: MirOperation::Other,
            output: 4,
            inputs: vec![0],
        });
        mir[3].phis.push(MirPhi {
            output: 7,
            inputs: vec![1, 2],
        });
        mir[3].phis.push(MirPhi {
            output: 8,
            inputs: vec![3, 4],
        });
        link(&mut mir, 0, 1);
        link(&mut mir, 0, 2);
        link(&mut mir, 1, 3);
        link(&mut mir, 2, 3);
        let pass = MirUndefUsePass::from(mir);
        let result = pass.run();
        assert!(result.is_ok());
    }

    #[test]
    fn unsafe_fork_join_phi() {
        let mut mir: MirGraph = vec![MirBasicBlock::default(); 4].into_boxed_slice();
        mir[0].instructions.push(MirInstruction {
            operation: MirOperation::Other,
            output: 0,
            inputs: vec![],
        });
        mir[1].instructions.push(MirInstruction {
            operation: MirOperation::Other,
            output: 1,
            inputs: vec![0],
        });
        mir[2].instructions.push(MirInstruction {
            operation: MirOperation::Other,
            output: 2,
            inputs: vec![0],
        });
        mir[3].phis.push(MirPhi {
            output: 3,
            inputs: vec![1, 1],
        });
        link(&mut mir, 0, 1);
        link(&mut mir, 0, 2);
        link(&mut mir, 1, 3);
        link(&mut mir, 2, 3);
        let pass = MirUndefUsePass::from(mir);
        let result = pass.run();
        assert_undef_err(&result);
    }

    #[test]
    fn safe_loop() {
        let mut mir: MirGraph = vec![MirBasicBlock::default(); 4].into_boxed_slice();
        mir[0].instructions.push(MirInstruction {
            operation: MirOperation::Other,
            output: 0,
            inputs: vec![],
        });
        mir[1].phis.push(MirPhi {
            output: 1,
            inputs: vec![0, 2],
        });
        mir[2].instructions.push(MirInstruction {
            operation: MirOperation::Other,
            output: 2,
            inputs: vec![1],
        });
        mir[3].instructions.push(MirInstruction {
            operation: MirOperation::Other,
            output: 3,
            inputs: vec![0, 1],
        });
        link(&mut mir, 0, 1);
        link(&mut mir, 1, 2);
        link(&mut mir, 2, 1);
        link(&mut mir, 1, 3);
        let pass = MirUndefUsePass::from(mir);
        let result = pass.run();
        assert!(result.is_ok());
    }

    #[test]
    fn unsafe_loop() {
        let mut mir: MirGraph = vec![MirBasicBlock::default(); 4].into_boxed_slice();
        mir[0].instructions.push(MirInstruction {
            operation: MirOperation::Other,
            output: 0,
            inputs: vec![],
        });
        mir[1].phis.push(MirPhi {
            output: 1,
            inputs: vec![0, 2],
        });
        mir[2].instructions.push(MirInstruction {
            operation: MirOperation::Other,
            output: 3,
            inputs: vec![1],
        });
        mir[3].instructions.push(MirInstruction {
            operation: MirOperation::Other,
            output: 3,
            inputs: vec![0, 1],
        });
        link(&mut mir, 0, 1);
        link(&mut mir, 1, 2);
        link(&mut mir, 2, 1);
        link(&mut mir, 1, 3);
        let pass = MirUndefUsePass::from(mir);
        let result = pass.run();
        assert_undef_err(&result);
    }
}
