// TODO: Use stowaway for pointer-passing?

use std::mem;
use std::os::raw::c_void;

use crate::ast::mir::{
    MirDefId,
    MirBasicBlockIndex,
    MirGraph,
    MirBasicBlock,
    MirInstruction,
    MirPhi,
    MirOperation,
};

use crate::bindings::handle::Handle;

// MirOperation bindings

#[no_mangle]
pub unsafe extern "C" fn verifirefox_ast_mir_operation_new_other() -> *mut c_void {
    let operation = MirOperation::Other;
    Box::into_raw(Box::new(operation)) as *mut _
}


// MirGraph bindings

#[no_mangle]
pub unsafe extern "C" fn verifirefox_ast_mir_graph_new(n_blocks: usize) -> *mut MirGraph {
    let graph = (0..n_blocks)
        .map(|_| MirBasicBlock::default())
        .collect::<Box<[_]>>();
    Box::into_raw(Box::new(graph)) as *mut _
}

#[no_mangle]
pub unsafe extern "C" fn verifirefox_ast_mir_graph_put_block(
    graph_ptr: *mut MirGraph,
    bb_index: usize,
    bb_ptr: *mut c_void,
) {
    let graph = &mut *(graph_ptr as *mut MirGraph);
    let node = *(Box::<MirBasicBlock>::from_raw(bb_ptr as *mut _));
    graph[bb_index] = node;
}

#[no_mangle]
pub unsafe extern "C" fn verifirefox_ast_mir_graph_into_handle(
    graph_ptr: *mut MirGraph,
) -> Handle<MirGraph> {
    Box::<MirGraph>::from_raw(graph_ptr).into()
}

#[no_mangle]
pub unsafe extern "C" fn verifirefox_ast_mir_graph_clone_handle(
    graph_handle: &Handle<MirGraph>,
) -> Handle<MirGraph> {
    graph_handle.clone()
}

#[no_mangle]
pub unsafe extern "C" fn verifirefox_ast_mir_graph_drop_handle(graph_handle: Handle<MirGraph>) {
    mem::drop(graph_handle);
}

// MirInstruction bindings

#[no_mangle]
pub unsafe extern "C" fn verifirefox_ast_mir_instruction_new(
    operation_ptr: *mut c_void,
    input_capacity: usize,
    def_id: MirDefId,
) -> *mut c_void {
    let operation = if operation_ptr.is_null() {
        MirOperation::Other
    } else {
        *(Box::<MirOperation>::from_raw(operation_ptr as *mut _))
    };
    let node = MirInstruction::new(
        operation,
        input_capacity,
        def_id,
    );
    Box::into_raw(Box::new(node)) as *mut _
}

#[no_mangle]
pub unsafe extern "C" fn verifirefox_ast_mir_instruction_push_input(
    instruction_ptr: *mut c_void,
    input: MirDefId,
) {
    let instr = &mut *(instruction_ptr as *mut MirInstruction);
    instr.push_input(input);
}

// MirPhi bindings

#[no_mangle]
pub unsafe extern "C" fn verifirefox_ast_mir_phi_new(
    input_capacity: usize,
    def_id: MirDefId,
) -> *mut c_void {
    let node = MirPhi::new(
        input_capacity,
        def_id,
    );
    Box::into_raw(Box::new(node)) as *mut _
}

#[no_mangle]
pub unsafe extern "C" fn verifirefox_ast_mir_phi_push_input(
    phi_ptr: *mut c_void,
    input: MirDefId,
) {
    let phi = &mut *(phi_ptr as *mut MirPhi);
    phi.push_input(input);
}

// MirBasicBlock bindings

#[no_mangle]
pub unsafe extern "C" fn verifirefox_ast_mir_basic_block_new(
    phi_capacity: usize,
    instruction_capacity: usize,
    predecessor_capacity: usize,
    successor_capacity: usize,
) -> *mut c_void {
    let node = MirBasicBlock::new(
        phi_capacity,
        instruction_capacity,
        predecessor_capacity,
        successor_capacity,
    );
    Box::into_raw(Box::new(node)) as *mut _
}

#[no_mangle]
pub unsafe extern "C" fn verifirefox_ast_mir_basic_block_push_phi(
    bb_ptr: *mut c_void,
    phi_ptr: *mut c_void,
) {
    let bb = &mut *(bb_ptr as *mut MirBasicBlock);
    let phi = *(Box::<MirPhi>::from_raw(phi_ptr as *mut _));
    bb.push_phi(phi);
}

#[no_mangle]
pub unsafe extern "C" fn verifirefox_ast_mir_basic_block_push_instruction(
    bb_ptr: *mut c_void,
    instruction_ptr: *mut c_void,
) {
    let bb = &mut *(bb_ptr as *mut MirBasicBlock);
    let instruction = *(Box::<MirInstruction>::from_raw(instruction_ptr as *mut _));
    bb.push_instruction(instruction);
}

#[no_mangle]
pub unsafe extern "C" fn verifirefox_ast_mir_basic_block_push_predecessor(
    bb_ptr: *mut c_void,
    predecessor_idx: MirBasicBlockIndex,
) {
    let bb = &mut *(bb_ptr as *mut MirBasicBlock);
    bb.push_predecessor(predecessor_idx);
}

#[no_mangle]
pub unsafe extern "C" fn verifirefox_ast_mir_basic_block_push_successor(
    bb_ptr: *mut c_void,
    successor_idx: MirBasicBlockIndex,
) {
    let bb = &mut *(bb_ptr as *mut MirBasicBlock);
    bb.push_successor(successor_idx);
}

