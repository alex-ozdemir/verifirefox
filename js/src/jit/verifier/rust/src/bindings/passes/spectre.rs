use crate::ast::lir::LirGraph;
use crate::execute;
use crate::passes::SpectrePass;

use crate::bindings::handle::Handle;

#[no_mangle]
pub extern "C" fn verifirefox_passes_spectre_sync(
    graph_handle: Handle<LirGraph>,
) {
    execute::sync_pass(SpectrePass::new(graph_handle.into()));
}

#[no_mangle]
pub extern "C" fn verifirefox_passes_spectre_async(
    graph_handle: Handle<LirGraph>,
) {
    execute::async_pass(SpectrePass::new(graph_handle.into()));
}
