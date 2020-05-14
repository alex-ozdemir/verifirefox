// TODO: Verify allocation policies beyond internal consistency.
// TODO: Verify allocation used-at-start property.
// TODO: Verify stack integrity?
// TODO: Account for inserted nodes in successors equivalence check.

mod ast;

use std::collections::HashMap;
use std::sync::Arc;

use anyhow::{bail, ensure, Context, Error};

use crate::ast::lir::LirGraph;

use crate::passes::base::Pass;

use crate::passes::reg_alloc::ast::{
    Possibility, RegAllocGraph, RegAllocNode, RegAllocPredecessors,
};

pub use crate::passes::reg_alloc::ast::{RegAllocNodeIndex, RegAllocState};

#[derive(Clone, Debug)]
pub struct RegAllocPass {
    before_graph: Arc<LirGraph>,
    after_graph: Arc<LirGraph>,
}

impl RegAllocPass {
    pub fn new(before_graph: Arc<LirGraph>, after_graph: Arc<LirGraph>) -> Self {
        RegAllocPass {
            before_graph,
            after_graph,
        }
    }
}

impl Pass for RegAllocPass {
    fn run(&self) -> Result<(), Error> {
        let mut graph = RegAllocGraph::from_lir(&*self.before_graph, &*self.after_graph)?;

        find_fixpoint(&mut graph, false)?;
        find_fixpoint(&mut graph, true)?;
        check_graph_completeness(&graph)?;

        Ok(())
    }
}

fn find_fixpoint(graph: &mut RegAllocGraph, strict_meet: bool) -> Result<(), Error> {
    /*
    let prefix = format!("{:p}", graph);
    let graph_size = graph.capacity();
    let mut iterations = 0;
    eprintln!("{} find_fixpoint on {}", prefix, graph_size);
    */

    for node_index in (0..graph.nodes.len()).map(RegAllocNodeIndex::from) {
        if let RegAllocPredecessors::AtBlockStart(_) = graph.nodes[node_index].predecessors {
            graph.queue_push(node_index);
        }
    }

    fn handle_node(
        graph: &mut RegAllocGraph,
        node_index: RegAllocNodeIndex,
        strict_meet: bool,
    ) -> Result<(), Error> {
        let mut state = meet(graph, &graph.nodes[node_index].predecessors, strict_meet);

        let node = &mut graph.nodes[node_index];

        node.has_complete_state = match node.operation.pre_judge(&state) {
            Possibility::Known(_) => true,
            Possibility::Unknown => false,
            Possibility::Invalid => {
                bail!("Found inconsistency with pre-transfer state {:?}", state);
            }
        };

        node.operation.transfer(&mut state);

        node.has_complete_state = match node.operation.post_judge(&state) {
            Possibility::Known(_) => true,
            Possibility::Unknown => false,
            Possibility::Invalid => {
                bail!("Found inconsistency with post-transfer state {:?}", state);
            }
        } && node.has_complete_state;

        if node.state.as_ref() != Some(&state) {
            node.state = Some(state);

            for successor in node.successors.to_owned().iter() {
                graph.queue_push(*successor);
            }
        }

        Ok(())
    }

    loop {
        let node_index = match graph.queue_pop() {
            Some(node_index) => node_index,
            None => break,
        };

        handle_node(graph, node_index, strict_meet)
            .with_context(|| format!("Error at node {}", node_index))?;

        /*
        iterations += 1;
        if iterations % 100000 == 0 {
            //eprintln!("{} still going on {} at {}; queue = {} : {:?}", prefix, graph_size, iterations, dirty.len(), dirty);
        }
        */
    }

    //eprintln!("{} end {} on {}", prefix, iterations, graph_size);
    Ok(())
}

fn check_graph_completeness(graph: &RegAllocGraph) -> Result<(), Error> {
    fn handle_node(graph: &RegAllocGraph, node: &RegAllocNode) -> Result<(), Error> {
        ensure!(
            node.has_complete_state,
            "Found incomplete fixpoint state {:?}",
            meet(graph, &node.predecessors, true)
        );
        Ok(())
    }

    for (raw_node_index, node) in graph.nodes.iter().enumerate() {
        handle_node(graph, node).with_context(|| format!("Error at node {}", raw_node_index))?;
    }
    Ok(())
}

fn meet(
    graph: &RegAllocGraph,
    predecessors: &RegAllocPredecessors,
    strict: bool,
) -> RegAllocState {
    match predecessors {
        RegAllocPredecessors::AtBlockStart(all_predecessors) => {
            let mut state = HashMap::new();

            for (predecessor_index, predecessor_node_index) in all_predecessors.iter().enumerate()
            {
                let predecessor_state = match graph.nodes[*predecessor_node_index].state.as_ref() {
                    Some(predecessor_state) => predecessor_state,
                    None => continue,
                };

                for (physical_loc, virtual_regs) in predecessor_state {
                    let mut out_possibly_virtual_reg = Possibility::Unknown;

                    for possibly_virtual_reg in virtual_regs.iter() {
                        let virtual_reg = match possibly_virtual_reg {
                            Possibility::Known(virtual_reg) => *virtual_reg,
                            Possibility::Unknown => {
                                if strict {
                                    out_possibly_virtual_reg = Possibility::Unknown;
                                    break;
                                } else {
                                    continue;
                                }
                            }
                            Possibility::Invalid => {
                                out_possibly_virtual_reg = Possibility::Invalid;
                                break;
                            }
                        };

                        match out_possibly_virtual_reg {
                            Possibility::Known(out_virtual_reg) => {
                                if out_virtual_reg != virtual_reg {
                                    out_possibly_virtual_reg = Possibility::Invalid;
                                    break;
                                }
                            }
                            Possibility::Unknown => {
                                out_possibly_virtual_reg = Possibility::Known(virtual_reg);
                            }
                            Possibility::Invalid => break,
                        }
                    }

                    if let Possibility::Unknown = out_possibly_virtual_reg {
                        continue;
                    }

                    let out_virtual_regs = state.entry(*physical_loc).or_insert_with(|| {
                        vec![Possibility::Unknown; all_predecessors.len()].into_boxed_slice()
                    });
                    out_virtual_regs[predecessor_index] = out_possibly_virtual_reg;
                }
            }

            state
        }
        RegAllocPredecessors::InBlockBody(predecessor_node_index) => {
            match graph.nodes[*predecessor_node_index].state.as_ref() {
                Some(predecessor_state) => predecessor_state.clone(),
                None => HashMap::new(),
            }
        }
    }
}
