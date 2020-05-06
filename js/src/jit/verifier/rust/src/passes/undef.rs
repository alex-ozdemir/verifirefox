use std::collections::{HashMap, HashSet, VecDeque};
use std::default::Default;
use std::hash::Hash;
use std::iter::{once, FromIterator};

trait Set<T>: Default + Clone + Eq {
    fn union_with(self, other: &Self) -> Self;
    fn insersect_with(self, other: &Self) -> Self;
    fn add(self, item: T) -> Self;
    fn remove(self, item: T) -> Self;
    fn contains(&self, item: &T) -> bool;
}

trait DefUseGraph {
    /// Unique for each node
    type Node: Eq + Ord + Hash + Copy;
    /// Unique for each def/use-able identifier
    type Id: Eq + Ord + Hash + Copy;

    fn predecessors(&self, n: Self::Node) -> Vec<Self::Node>;
    fn successors(&self, n: Self::Node) -> Vec<Self::Node>;
    fn definitions(&self, n: Self::Node) -> Vec<Self::Id>;
    fn uses(&self, n: Self::Node) -> Vec<Self::Id>;
    fn nodes(&self) -> Vec<Self::Node>;
    fn entry(&self) -> Self::Node;
}

#[derive(Clone)]
struct DefUseAnalysisNode<Node, Id, Ids: Set<Id>> {
    preds: Vec<Node>,
    succs: Vec<Node>,
    /// identifiers defined here
    defs: Vec<Id>,
    uses: Vec<Id>,
    /// identifiers defined earlier
    prior_defs: Ids,
    /// identifiers defined earlier
    post_defs: Ids,
    queued: bool,
}

impl<Node, Id: Copy, Ids: Set<Id>> DefUseAnalysisNode<Node, Id, Ids> {
    fn apply_transition(&mut self) {
        self.post_defs = self
            .defs
            .iter()
            .fold(self.prior_defs.clone(), |s: Ids, d| s.add(*d));
    }
}

struct DefUseAnalysisState<'a, G: DefUseGraph, Ids: Set<G::Id>> {
    graph: &'a G,
    nodes: HashMap<G::Node, DefUseAnalysisNode<G::Node, G::Id, Ids>>,
}

impl<'a, G: DefUseGraph, Ids: Set<G::Id>> DefUseAnalysisState<'a, G, Ids> {
    fn from_graph(graph: &'a G) -> Self {
        DefUseAnalysisState {
            graph,
            nodes: HashMap::new(),
        }
    }

    fn load_node(&mut self, n: G::Node) -> &mut DefUseAnalysisNode<G::Node, G::Id, Ids> {
        let nodes = &mut self.nodes;
        let graph = &self.graph;
        nodes.entry(n).or_insert_with(|| {
            let mut this = DefUseAnalysisNode {
                preds: graph.predecessors(n),
                succs: graph.successors(n),
                defs: graph.definitions(n),
                uses: graph.uses(n),
                prior_defs: Ids::default(),
                post_defs: Ids::default(),
                queued: false,
            };
            this.apply_transition();
            this
        })
    }

    fn compute_defs(&mut self) {
        let start = self.graph.entry();
        let mut queue = VecDeque::from_iter(once(start));
        self.load_node(start).queued = true;
        while let Some(n) = queue.pop_front() {
            let data = self.load_node(n);
            assert!(data.queued);
            data.queued = false;
            let pred_nodes = data.preds.clone();
            let prior_defs = match data.preds.len() {
                0 => continue,
                _ => pred_nodes.iter().fold(Ids::default(), |s, p| {
                    s.union_with(&self.load_node(*p).post_defs)
                }),
            };
            let data = self.load_node(n);
            if prior_defs != data.prior_defs {
                data.prior_defs = prior_defs;
                data.apply_transition();
                for succ in data.succs.clone() {
                    let data = self.load_node(succ);
                    if !data.queued {
                        data.queued = true;
                        queue.push_back(succ);
                    }
                }
            }
        }
    }

    fn find_undef_uses(&self) -> Vec<(G::Node, G::Id)> {
        let start = self.graph.entry();
        let mut queue = VecDeque::from_iter(once(start));
        let mut queued_already = HashSet::new();
        queued_already.insert(start);
        let mut undef_uses = Vec::new();
        while let Some(n) = queue.pop_front() {
            let data = self.nodes.get(&n).expect("all nodes should be added during compute_defs");
            for use_ in &data.uses {
                if !data.prior_defs.contains(use_) {
                    undef_uses.push((n, *use_));
                }
            }
            for succ in &data.succs {
                if !queued_already.contains(succ) {
                    queued_already.insert(*succ);
                    queue.push_back(*succ);
                }
            }
        }
        undef_uses
    }
}
