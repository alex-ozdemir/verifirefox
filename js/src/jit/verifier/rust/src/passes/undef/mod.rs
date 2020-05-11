// TODO(aozdemir): Extract basic blocks and find fixpoint over them, not nodes
// (Optimization)

use std::borrow::Cow;
use std::collections::{HashMap, HashSet, VecDeque};
use std::convert::From;
use std::default::Default;
use std::fmt::Debug;
use std::hash::Hash;
use std::iter::FromIterator;
use std::marker::PhantomData;
use std::sync::Arc;

use anyhow::{anyhow, Error};
use thiserror::Error;

use crate::passes::base::Pass;

pub mod lir;

pub trait Set<T>: 'static + Clone + Eq + Debug + Send + Sync {
    fn union(&mut self, other: &Self);
    fn insersect(&mut self, other: &Self);
    fn difference(&mut self, other: &Self);
    fn add(&mut self, item: T);
    fn remove(&mut self, item: &T);
    fn contains(&self, item: &T) -> bool;
    fn all(items: &HashSet<T>) -> Self;
    fn none(items: &HashSet<T>) -> Self;
    fn add_all(&mut self, items: impl IntoIterator<Item = T>) {
        items.into_iter().map(|i| self.add(i)).count();
    }
}

pub trait DefUseGraph: 'static + Debug + Send + Sync {
    /// Unique for each node
    type Node: Eq + Ord + Hash + Copy + Debug + Send + Sync;
    /// Unique for each def/use-able identifier
    type Id: Eq + Copy + Debug + Send + Sync;

    fn predecessors(&self, n: Self::Node) -> Vec<Self::Node>;
    /// Whether this node is a join point.
    /// If true, then the number of predecessors must equal the number of uses.
    fn is_phi(&self, n: Self::Node) -> bool;
    fn is_block_start(&self, n: Self::Node) -> bool;
    fn successors(&self, n: Self::Node) -> Vec<Self::Node>;
    fn definitions(&self, n: Self::Node) -> Vec<Self::Id>;
    fn uses(&self, n: Self::Node) -> Vec<Self::Id>;
    /// All nodes. First is entry.
    fn nodes(&self) -> Vec<Self::Node>;
    /// All ids.
    fn ids(&self) -> HashSet<Self::Id>;
}

type BlockIdx = usize;

#[derive(Clone, Debug)]
struct Block<Node, Id, Ids: Set<Id>> {
    preds: Vec<BlockIdx>,
    succs: Vec<BlockIdx>,
    phis: Vec<Instruction<Node, Id, Ids>>,
    instrs: Vec<Instruction<Node, Id, Ids>>,
    queued: bool,
    pre_undefs: Ids,
    post_undefs: Ids,
    defs: Ids,
}

#[derive(Clone, Debug)]
struct Instruction<Node, Id, Ids: Set<Id>> {
    node: Node,
    defs: Ids,
    uses: Vec<Id>,
}

impl<Node, Id: Copy, Ids: Set<Id>> Instruction<Node, Id, Ids> {
    fn apply_transition(&self, pre: &mut Ids) {
        pre.difference(&self.defs);
    }
}

impl<Node, Id: Copy, Ids: Set<Id>> Block<Node, Id, Ids> {
    fn apply_transition(&mut self) {
        self.post_undefs = self.pre_undefs.clone();
        self.post_undefs.difference(&self.defs);
    }
}

#[derive(Clone, Debug)]
struct UndefUseAnalysisState<G: DefUseGraph, Ids: Set<G::Id>> {
    blocks: Vec<Block<G::Node, G::Id, Ids>>,
}

impl<G: DefUseGraph, Ids: Set<G::Id>> UndefUseAnalysisState<G, Ids> {
    fn from_graph(graph: &G) -> Result<Self, Error> {
        let mut queue = VecDeque::from_iter(graph.nodes().into_iter().filter(|n| graph.is_block_start(*n)));
        let mut queued: HashSet<G::Node> = HashSet::from_iter(queue.iter().cloned());
        let mut blocks = Vec::new();
        let mut node_to_block_id = HashMap::new();
        let ids = graph.ids();
        let none = Ids::none(&ids);

        // Assemble blocks
        while let Some(n) = queue.pop_front() {
            let block_id = if graph.is_block_start(n) {
                let block_id = blocks.len();
                node_to_block_id.insert(n, block_id);
                blocks.push(Block {
                    preds: Vec::new(),
                    succs: Vec::new(),
                    phis: Vec::new(),
                    instrs: Vec::new(),
                    queued: false,
                    pre_undefs: none.clone(),
                    post_undefs: none.clone(),
                    defs: none.clone(),
                });
                block_id
            } else {
                let preds = graph.predecessors(n);
                if preds.len() != 1 {
                    Err(anyhow!("non-start node {:?} with != 1 predecessors", n))?;
                }
                if graph.is_phi(n) && !graph.is_phi(preds[0]) {
                    Err(anyhow!("phi after non-phi! Node: {:?}", n))?;
                }
                *node_to_block_id.get(&preds[0]).unwrap()
            };
            node_to_block_id.insert(n, block_id);
            let block = &mut blocks[block_id];
            let mut defs = none.clone();
            defs.add_all(graph.definitions(n));
            block.defs.union(&defs);
            (if graph.is_phi(n) {
                &mut block.phis
            } else {
                &mut block.instrs
            })
            .push(Instruction {
                defs,
                uses: graph.uses(n),
                node: n,
            });

            for succ in graph.successors(n) {
                if !queued.contains(&succ) {
                    queued.insert(succ);
                    queue.push_back(succ);
                }
            }
        }

        // Set all undef at start
        blocks[0].pre_undefs = Ids::all(&ids);

        // Link blocks and apply transitions.
        for block in &mut blocks {
            block.apply_transition();
            let phis = block.phis.iter();
            let instrs = block.instrs.iter();
            let first = phis.clone().chain(instrs.clone()).next().unwrap().node;
            block.preds.extend(
                graph
                    .predecessors(first)
                    .iter()
                    .map(|p| node_to_block_id.get(p).unwrap()),
            );
            let last = phis.rev().chain(instrs.rev()).next().unwrap().node;
            block.succs.extend(
                graph
                    .successors(last)
                    .iter()
                    .map(|p| node_to_block_id.get(p).unwrap()),
            );
        }
        Ok(UndefUseAnalysisState { blocks })
    }

    fn get_block(&self, i: usize) -> Result<&Block<G::Node, G::Id, Ids>, Error> {
        self.blocks
            .get(i)
            .ok_or_else(|| anyhow!("Missing block: {}", i))
    }

    fn get_block_mut(&mut self, i: usize) -> Result<&mut Block<G::Node, G::Id, Ids>, Error> {
        self.blocks
            .get_mut(i)
            .ok_or_else(|| anyhow!("Missing block: {}", i))
    }

    fn fixpoint_over_blocks(&mut self) -> Result<(), Error> {
        let mut queue = VecDeque::from_iter(0..self.blocks.len());
        for n in &queue {
            self.get_block_mut(*n)?.queued = true;
        }
        while let Some(n) = queue.pop_front() {
            let data_mut = self.get_block_mut(n)?;
            assert!(data_mut.queued);
            data_mut.queued = false;
            let data = self.get_block(n)?;
            let prior_defs: Cow<Ids> = match data.preds.as_slice() {
                [] => continue,
                // Avoid the copy if there is only one.
                [pred] => Cow::Borrowed(&self.get_block(*pred)?.post_undefs),
                preds => Cow::Owned({
                    let mut s = self.get_block(preds[0])?.post_undefs.clone();
                    for p in preds.iter().skip(1) {
                        s.union(&self.get_block(*p)?.post_undefs);
                    }
                    s
                }),
            };
            if prior_defs.as_ref() != &data.pre_undefs {
                let new_prior_defs = prior_defs.into_owned();
                let data_mut = self.get_block_mut(n)?;
                data_mut.pre_undefs = new_prior_defs;
                data_mut.apply_transition();
                for succ in data_mut.succs.clone() {
                    let succ_data = self.get_block_mut(succ)?;
                    if !succ_data.queued {
                        succ_data.queued = true;
                        queue.push_back(succ);
                    }
                }
            }
        }
        Ok(())
    }

    fn find_undef_uses(&self) -> Result<(), Error> {
        for block in &self.blocks {
            let mut undefs = block.pre_undefs.clone();
            for phi in &block.phis {
                // For a phi node, each use must be defined **after** its corresponding block
                // predecpredecessor node
                if block.preds.len() != phi.uses.len() {
                    Err(anyhow!(
                        "For phi nodes, #uses must equal #block predecpredecessors\nNode: {:#?}\nGraph: {:#?}\nBlock preds: {:#?}\nuses:{:#?}",
                        phi.node,
                        self,
                        block.preds,
                        phi.uses
                    ))?;
                }
                for (pred, use_) in block.preds.iter().zip(phi.uses.iter()) {
                    if self.get_block(*pred)?.post_undefs.contains(use_) {
                        Err(UndefUseError {
                            node: phi.node,
                            var: *use_,
                        })?;
                    }
                }
                phi.apply_transition(&mut undefs);
            }
            for instr in &block.instrs {
                // For non-phi node, each use must be defined **before** this node
                for use_ in &instr.uses {
                    if undefs.contains(use_) {
                        Err(UndefUseError {
                            node: instr.node,
                            var: *use_,
                        })?;
                    }
                }
                instr.apply_transition(&mut undefs);
            }
        }
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct UndefUsePass<G: DefUseGraph, Ids: Set<G::Id>> {
    graph: Arc<G>,
    _ids: PhantomData<Ids>,
}

impl<G: DefUseGraph, Ids: Set<G::Id>> From<Arc<G>> for UndefUsePass<G, Ids> {
    fn from(graph: Arc<G>) -> Self {
        Self {
            graph,
            _ids: Default::default(),
        }
    }
}

impl<G: DefUseGraph, Ids: Set<G::Id>> From<G> for UndefUsePass<G, Ids> {
    fn from(graph: G) -> Self {
        Self::from(Arc::new(graph))
    }
}

#[derive(Clone, Debug, Error)]
#[error("Use of undef {var:?} at node {node:?}")]
pub struct UndefUseError<Node, Id>
where
    Node: 'static + Debug,
    Id: 'static + Debug,
{
    node: Node,
    var: Id,
}

impl<'a, G: DefUseGraph, Ids: Set<G::Id>> Pass for UndefUsePass<G, Ids> {
    fn run(&self) -> Result<(), Error> {
        let mut state = UndefUseAnalysisState::<G, Ids>::from_graph(&*self.graph)?;
        state.fixpoint_over_blocks()?;
        state.find_undef_uses()?;
        Ok(())
    }
}
