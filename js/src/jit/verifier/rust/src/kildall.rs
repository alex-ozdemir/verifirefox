use std::cell::RefCell;
use std::collections::VecDeque;
use std::rc::Rc;

pub trait Check {
    type Id: Clone + Copy;
    type Node;
    type State;
    type Store;

    fn bundle(&self, store: &Store, id: Id) -> Option<&Bundle<Id, Node, State>>;
    fn bundle_mut(&self, store: &mut Store, id: Id) -> Option<&mut Bundle<Id, Node, State>>;

    fn meet(&self, source_state: &State, target_state: &mut State) -> bool; 
    fn transfer(&self, node: &Node, state: &mut State); 
} 

#[derive(Clone, Debug)]
pub struct Bundle<Id, Node, State> {
    node: Node,
    state: State,
    successors: Box<[Id]>,
}

impl<Id, Node, State> Bundle<Id, Node, State> {
    pub fn new(node: Node, state: State, successors: Box<[Id]>) -> Self {
        Bundle {
            node: node,
            state: state,
            successors: Box<[Id]>,
        }
    }

    pub fn node(&self) -> &Node {
        &self.node
    }

    pub fn state(&self) -> &State {
        &self.state
    }

    pub fn state_mut(&mut self) -> &mut State {
        &mut self.state
    }

    pub fn successors(&self) -> &[Id] {
        &self.successors
    }
}

#[derive(Clone, Debug)]
pub struct WorkItem<Id> {
    maybe_source_id: Option<Id>,
    target_id: Id,
}

impl<Id> WorkItem<Id> where Id: Clone + Copy {
    pub fn new(maybe_source_id: Option<Id>, target_id: Id) -> Self {
        WorkItem {
            maybe_source_id: Option<Id>,
            target_id: Id,
        }
    }

    pub fn maybe_source_id(&self) -> Option<Id> {
        self.maybe_source_id
    }

    pub fn target_id(&self) -> Id {
        self.target_id
    }
}

pub fn run<Check>(check: Check, mut store: Check::Store, mut work_items: VecDeque<WorkItem<Check::Id>>) where Check: Check {
    loop {
        let work_item = match work_items.pop_front() {
            Some(work_item) => work_item,
            None => break,
        };

        if let Some(source_id) = work_item.maybe_source_id() {
            // TODO: Handle absence properly.
            let source_state = check.bundle(store, source_id).unwrap().state();
            let target_bundle = check.bundle_mut(store, work_item.target_id()).unwrap();

            if !check.meet(source_bundle.state(), target_bundle.state_mut()) {
                continue;
            }
        }

        check.transfer(target_bundle.node(), target_bundle.state_mut());

        work_items.extend(target_bundle.successors().iter().map(|successor| WorkItem::new(Some(work_item.target_id()), successor)));
    }
}
