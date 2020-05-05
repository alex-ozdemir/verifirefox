use crate::ast::lir::LirGraph;
use crate::execute;
use crate::passes::RegAllocPass;

use crate::bindings::handle::Handle;

fn make_reg_alloc_pass(
    before_graph_handle: Handle<LirGraph>,
    after_graph_handle: Handle<LirGraph>,
) -> RegAllocPass {
    RegAllocPass::new(before_graph_handle.into(), after_graph_handle.into())
}

#[no_mangle]
pub extern "C" fn verifirefox_passes_reg_alloc_sync(
    before_graph_handle: Handle<LirGraph>,
    after_graph_handle: Handle<LirGraph>,
) {
    execute::sync_pass(make_reg_alloc_pass(before_graph_handle, after_graph_handle));
}

#[no_mangle]
pub extern "C" fn verifirefox_passes_reg_alloc_async(
    before_graph_handle: Handle<LirGraph>,
    after_graph_handle: Handle<LirGraph>,
) {
    execute::async_pass(make_reg_alloc_pass(before_graph_handle, after_graph_handle));
}
