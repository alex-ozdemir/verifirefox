use std::sync::Arc;

use crate::ast::lir::LirGraph;
use crate::ast::mir::MirGraph;
use crate::execute;
use crate::passes::LirUndefUsePass;
use crate::passes::MirUndefUsePass;

use crate::bindings::handle::Handle;

fn make_lir_undef_use_pass(graph_handle: Handle<LirGraph>) -> LirUndefUsePass {
    <LirUndefUsePass as From<Arc<LirGraph>>>::from(graph_handle.into())
}

#[no_mangle]
pub extern "C" fn verifirefox_passes_lir_undef_use_sync(graph_handle: Handle<LirGraph>) {
    execute::sync_pass(make_lir_undef_use_pass(graph_handle));
}

#[no_mangle]
pub extern "C" fn verifirefox_passes_lir_undef_use_async(graph_handle: Handle<LirGraph>) {
    execute::async_pass(make_lir_undef_use_pass(graph_handle));
}

fn make_mir_undef_use_pass(graph_handle: Handle<MirGraph>) -> MirUndefUsePass {
    <MirUndefUsePass as From<Arc<MirGraph>>>::from(graph_handle.into())
}

#[no_mangle]
pub extern "C" fn verifirefox_passes_mir_undef_use_sync(graph_handle: Handle<MirGraph>) {
    execute::sync_pass(make_mir_undef_use_pass(graph_handle));
}

#[no_mangle]
pub extern "C" fn verifirefox_passes_mir_undef_use_async(graph_handle: Handle<MirGraph>) {
    execute::async_pass(make_mir_undef_use_pass(graph_handle));
}
