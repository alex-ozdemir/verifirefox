// TODO: Verify allocation policies beyond internal consistency.
// TODO: Verify allocation used-at-start property.
// TODO: Verify allocation recovered-input property.
// TODO: Verify definition must-reuse-input property.
// TODO: Verify stack integrity?
// TODO: Account for inserted nodes in successors equivalence check.

mod ast;

use std::collections::{HashMap, VecDeque};
use std::error;
use std::sync::Arc;

use anyhow::Error;
use thiserror::Error;

use crate::ast::lir::LirGraph;

use crate::passes::base::Pass;

use crate::passes::reg_alloc::ast::{
    reg_alloc_graph_from_lir, Possibility, RegAllocGraph, RegAllocPredecessors,
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
        let mut graph = reg_alloc_graph_from_lir(&*self.before_graph, &*self.after_graph)?;

        find_fixpoint(&mut graph, false)?;
        find_fixpoint(&mut graph, true)?;
        check_graph_completeness(&graph)?;

        Ok(())
    }
}

#[derive(Clone, Debug, Error)]
#[error("error at node #{node_index}")]
pub struct RegAllocError<Source>
where
    Source: 'static + error::Error,
{
    node_index: RegAllocNodeIndex,
    source: Source,
}

#[derive(Clone, Debug, Error)]
#[error("found inconsistency with state {state:?}")]
pub struct RegAllocFixpointError {
    state: RegAllocState,
}

fn find_fixpoint(
    graph: &mut RegAllocGraph,
    strict_meet: bool,
) -> Result<(), RegAllocError<RegAllocFixpointError>> {
    /*
    let prefix = format!("{:p}", graph);
    let graph_size = graph.capacity();
    let mut iterations = 0;
    eprintln!("{} find_fixpoint on {}", prefix, graph_size);
    */

    let mut dirty = graph
        .iter()
        .enumerate()
        .filter_map(|(raw_node_index, node)| match node.predecessors {
            RegAllocPredecessors::AtBlockStart(_) => Some(raw_node_index.into()),
            RegAllocPredecessors::InBlockBody(_) => None,
        })
        .collect::<VecDeque<RegAllocNodeIndex>>();

    loop {
        let node_index = match dirty.pop_front() {
            Some(item) => item,
            None => break,
        };

        /*
        iterations += 1;
        if iterations % 100000 == 0 {
            //eprintln!("{} still going on {} at {}; queue = {} : {:?}", prefix, graph_size, iterations, dirty.len(), dirty);
        }
        */

        let mut state = meet(graph, &graph[node_index].predecessors, strict_meet);

        let node = &mut graph[node_index];

        let state_is_complete = match node.operation.judge(&state) {
            Possibility::Known(_) => true,
            Possibility::Unknown => false,
            Possibility::Invalid => {
                return Err(RegAllocError {
                    node_index,
                    source: RegAllocFixpointError {
                        state: state.clone(),
                    },
                });
            }
        };

        node.operation.transfer(&mut state);

        if state != node.state {
            //dirty.extend(node.successors.iter());
            for successor in node.successors.iter() {
                if !dirty.contains(successor) {
                    dirty.push_back(*successor);
                }
            }
            node.state = state;
        }

        node.has_complete_state = state_is_complete;
    }

    //eprintln!("{} end {} on {}", prefix, iterations, graph_size);
    Ok(())
}

#[derive(Clone, Debug, Error)]
#[error("found incomplete fixpoint state {state:?}, graph: {graph:#?}")]
pub struct RegAllocCompletenessError {
    state: RegAllocState,
    graph: RegAllocGraph,
}

fn check_graph_completeness(
    graph: &RegAllocGraph,
) -> Result<(), RegAllocError<RegAllocCompletenessError>> {
    for (raw_node_index, node) in graph.iter().enumerate() {
        if !node.has_complete_state {
            let state = meet(graph, &node.predecessors, true);

            return Err(RegAllocError {
                node_index: raw_node_index.into(),
                source: RegAllocCompletenessError {
                    state,
                    graph: graph.clone(),
                },
            });
        }
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
                for (physical_loc, virtual_regs) in graph[*predecessor_node_index].state.iter() {
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
            graph[*predecessor_node_index].state.clone()
        }
    }
}
