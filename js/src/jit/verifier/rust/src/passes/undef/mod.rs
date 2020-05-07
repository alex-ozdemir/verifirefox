use std::collections::{HashMap, VecDeque};
use std::convert::From;
use std::default::Default;
use std::fmt::Debug;
use std::hash::Hash;
use std::iter::FromIterator;
use std::marker::PhantomData;
use std::sync::Arc;
use std::borrow::Cow;

use anyhow::Error;
use thiserror::Error;

use crate::passes::base::Pass;

pub mod lir;

pub trait Set<T>: 'static + Clone + Eq + Debug + Send + Sync {
    fn union_with(self, other: &Self) -> Self;
    fn insersect_with(self, other: &Self) -> Self;
    fn add(self, item: T) -> Self;
    fn remove(self, item: &T) -> Self;
    fn contains(&self, item: &T) -> bool;
    fn new(capacity: usize) -> Self;
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
    fn successors(&self, n: Self::Node) -> Vec<Self::Node>;
    fn definitions(&self, n: Self::Node) -> Vec<Self::Id>;
    fn uses(&self, n: Self::Node) -> Vec<Self::Id>;
    fn nodes(&self) -> Vec<Self::Node>;
    fn n_ids(&self) -> usize;
}

#[derive(Clone, Debug)]
struct UndefUseAnalysisNode<Node, Id, Ids: Set<Id>> {
    preds: Vec<Node>,
    succs: Vec<Node>,
    /// identifiers defined here
    defs: Vec<Id>,
    uses: Vec<Id>,
    /// identifiers defined earlier
    prior_defs: Ids,
    /// identifiers defined earlier
    post_defs: Ids,
    is_phi: bool,
    queued: bool,
}

impl<Node, Id: Copy, Ids: Set<Id>> UndefUseAnalysisNode<Node, Id, Ids> {
    fn apply_transition(&mut self) {
        self.post_defs = self
            .defs
            .iter()
            .fold(self.prior_defs.clone(), |s: Ids, d| s.add(*d));
    }
}

#[derive(Clone, Debug)]
struct UndefUseAnalysisState<G: DefUseGraph, Ids: Set<G::Id>> {
    nodes: HashMap<G::Node, UndefUseAnalysisNode<G::Node, G::Id, Ids>>,
    n_ids: usize,
}

impl<G: DefUseGraph, Ids: Set<G::Id>> UndefUseAnalysisState<G, Ids> {
    fn from_graph(graph: &G) -> Self {
        let mut nodes = HashMap::new();
        let n_ids = graph.n_ids();
        for n in graph.nodes() {
            let uses = graph.uses(n);
            let preds = graph.predecessors(n);
            let is_phi = graph.is_phi(n);
            if is_phi {
                assert_eq!(uses.len(), preds.len(), "For phi nodes, #uses must equal #predecessors");
            }
            let mut data = UndefUseAnalysisNode {
                preds,
                succs: graph.successors(n),
                defs: graph.definitions(n),
                uses,
                is_phi,
                prior_defs: Ids::new(n_ids),
                post_defs: Ids::new(n_ids),
                queued: false,
            };
            data.apply_transition();
            nodes.insert(n, data);
        }
        UndefUseAnalysisState { n_ids, nodes }
    }

    fn get_node(&self, n: &G::Node) -> Result<&UndefUseAnalysisNode<G::Node, G::Id, Ids>, MissingNodeError<G::Node>> {
        self.nodes.get(&n).ok_or(MissingNodeError::from(*n))
    }

    fn get_node_mut(&mut self, n: &G::Node) -> Result<&mut UndefUseAnalysisNode<G::Node, G::Id, Ids>, MissingNodeError<G::Node>> {
        self.nodes.get_mut(&n).ok_or(MissingNodeError::from(*n))
    }

    fn compute_defs(&mut self) -> Result<(), MissingNodeError<G::Node>> {
        let mut queue = VecDeque::from_iter(self.nodes.keys().copied());
        for n in &queue {
            self.get_node_mut(n)?.queued = true;
        }
        while let Some(n) = queue.pop_front() {
            let data_mut = self.get_node_mut(&n)?;
            assert!(data_mut.queued);
            data_mut.queued = false;
            let data = self.get_node(&n)?;
            let prior_defs: Cow<Ids> = match data.preds.as_slice() {
                [] => continue,
                // Avoid the copy if there is only one.
                [pred] => Cow::Borrowed(&self.get_node(pred)?.post_defs),
                preds => Cow::Owned(preds.iter().skip(1).fold(Ok(self.get_node(&preds[0])?.post_defs.clone()), |s: Result<Ids, MissingNodeError<G::Node>>, p| {
                    s.and_then(|s| Ok(s.insersect_with(&self.get_node(p)?.post_defs)))
                })?),
            };
            if prior_defs.as_ref() != &data.prior_defs {
                let new_prior_defs = prior_defs.into_owned();
                let data_mut = self.get_node_mut(&n)?;
                data_mut.prior_defs = new_prior_defs;
                data_mut.apply_transition();
                for succ in data_mut.succs.clone() {
                    let succ_data = self.get_node_mut(&succ)?;
                    if !succ_data.queued {
                        succ_data.queued = true;
                        queue.push_back(succ);
                    }
                }
            }
        }
        Ok(())
    }

    fn find_undef_uses(&self) -> Result<(),Error> {
        for (node, data) in &self.nodes {
            if data.is_phi {
                // For a phi node, each use must be defined **after** its corresponding pred. node
                assert_eq!(data.preds.len(), data.uses.len());
                for (pred, use_) in data.preds.iter().zip(data.uses.iter()) {
                    if !self.get_node(pred)?.post_defs.contains(use_) {
                        Err(UndefUseError::from((*node, *use_)))?;
                    }
                }
            } else {
                // For non-phi node, each use must be defined **before** this node
                for use_ in &data.uses {
                    if !data.prior_defs.contains(use_) {
                        Err(UndefUseError::from((*node, *use_)))?;
                    }
                }
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
#[error("Use of undef {id:?} at node {node_index:?}")]
pub struct UndefUseError<Node, Id>
where
    Node: 'static + Debug,
    Id: 'static + Debug,
{
    node_index: Node,
    id: Id,
}

#[derive(Clone, Debug, Error)]
#[error("Missing node: {node_index:?}")]
pub struct MissingNodeError<Node>
where
    Node: 'static + Debug,
{
    node_index: Node,
}

impl<Node: 'static + Debug> From<Node> for MissingNodeError<Node> {
    fn from(node_index: Node) -> Self {
        Self {
            node_index,
        }
    }
}

impl<Node: 'static + Debug, Id: 'static + Debug> From<(Node, Id)> for UndefUseError<Node, Id> {
    fn from(pair: (Node, Id)) -> Self {
        Self {
            node_index: pair.0,
            id: pair.1,
        }
    }
}

impl<'a, G: DefUseGraph, Ids: Set<G::Id>> Pass for UndefUsePass<G, Ids> {
    fn run(&self) -> Result<(), Error> {
        let mut state = UndefUseAnalysisState::<G, Ids>::from_graph(&*self.graph);
        state.compute_defs()?;
        state.find_undef_uses()?;
        Ok(())
    }
}
