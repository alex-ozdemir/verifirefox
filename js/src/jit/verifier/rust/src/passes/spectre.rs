use anyhow::Error;
use super::base::Pass;
use crate::{LirGraph, ast::lir::{LirOperation, LirNode, LirAllocation,LoadElementV}};
use std::sync::Arc;

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
    fn run(&self) -> Result<(), Error>{
        let access_nodes = (*self.graph).iter().filter_map(LoadElementV::from_node);
        for access in access_nodes {
            eprintln!("{:?}", access.index());
        }
        Ok(())
    }
}