// TODO: Check LirDefinition and LirAllocation types and policies.
// TODO: Check LirUse usedAtStart invariant.

use std::mem;
use std::os::raw::c_void;

mod lir;

pub use lir::{ArgumentIndex, LirBlockId, LirDefinitionPolicy, LirDefinitionType, LirNodeId, LirUsePolicy, PhysicalReg, StackSlotIndex, VirtualReg};

use lir::{LirAllocation, LirBlock, LirDefinition, LirGraph, LirMove, LirMoveGroup, LirNode, LirOperation, LirUse};

// LirGraph bindings

#[no_mangle]
pub unsafe extern "C" fn verifirefox_lir_graph_new(block_count: LirBlockId) -> *mut c_void {
    let graph = LirGraph::new(block_count);
    Box::into_raw(Box::new(graph)) as *mut _
}

#[no_mangle]
pub unsafe extern "C" fn verifirefox_lir_graph_drop(graph_ptr: *mut c_void) {
    mem::drop(Box::<LirGraph>::from_raw(graph_ptr as *mut _))
}

#[no_mangle]
pub unsafe extern "C" fn verifirefox_lir_graph_put_block(graph_ptr: *mut c_void, block_ptr: *mut c_void) -> bool {
    let graph = &mut *(graph_ptr as *mut LirGraph);
    let block = *(Box::<LirBlock>::from_raw(block_ptr as *mut _));
    graph.put_block(block)
}

// LirBlock bindings

#[no_mangle]
pub unsafe extern "C" fn verifirefox_lir_block_new(id: LirBlockId, node_capacity: usize) -> *mut c_void {
    let block = LirBlock::new(id, node_capacity);
    Box::into_raw(Box::new(block)) as *mut _
}

#[no_mangle]
pub unsafe extern "C" fn verifirefox_lir_block_push_node(block_ptr: *mut c_void, node_ptr: *mut c_void) {
    let block = &mut *(block_ptr as *mut LirBlock);
    let node = *(Box::<LirNode>::from_raw(node_ptr as *mut _));
    block.push_node(node);
}

// LirNode bindings

#[no_mangle]
pub unsafe extern "C" fn verifirefox_lir_node_new(id: LirNodeId, operation_ptr: *mut c_void, operand_capacity: usize, def_capacity: usize, temp_capacity: usize, successor_capacity: usize) -> *mut c_void {
    let operation = if operation_ptr.is_null() {
        LirOperation::Other
    } else {
        *(Box::<LirOperation>::from_raw(operation_ptr as *mut _))
    };
    let node = LirNode::new(id, operation, operand_capacity, def_capacity, temp_capacity, successor_capacity);
    Box::into_raw(Box::new(node)) as *mut _
}

#[no_mangle]
pub unsafe extern "C" fn verifirefox_lir_node_push_operand(node_ptr: *mut c_void, operand_ptr: *mut c_void) {
    let node = &mut *(node_ptr as *mut LirNode);
    let operand = *(Box::<LirAllocation>::from_raw(operand_ptr as *mut _));
    node.push_operand(operand);
}

#[no_mangle]
pub unsafe extern "C" fn verifirefox_lir_node_push_def(node_ptr: *mut c_void, def_ptr: *mut c_void) {
    let node = &mut *(node_ptr as *mut LirNode);
    let def = *(Box::<LirDefinition>::from_raw(def_ptr as *mut _));
    node.push_def(def);
}

#[no_mangle]
pub unsafe extern "C" fn verifirefox_lir_node_push_temp(node_ptr: *mut c_void, temp_ptr: *mut c_void) {
    let node = &mut *(node_ptr as *mut LirNode);
    let temp = *(Box::<LirDefinition>::from_raw(temp_ptr as *mut _));
    node.push_temp(temp);
}

#[no_mangle]
pub unsafe extern "C" fn verifirefox_lir_node_push_successor(node_ptr: *mut c_void, successor: LirBlockId) {
    let node = &mut *(node_ptr as *mut LirNode);
    node.push_successor(successor);
}

// LirOperation bindings

#[no_mangle]
pub unsafe extern "C" fn verifirefox_lir_operation_new_move_group(move_group_ptr: *mut c_void) -> *mut c_void {
    let move_group = *(Box::<LirMoveGroup>::from_raw(move_group_ptr as *mut _));
    let operation = LirOperation::LirMoveGroup(move_group);
    Box::into_raw(Box::new(operation)) as *mut _
}

// LirMoveGroup bindings

#[no_mangle]
pub unsafe extern "C" fn verifirefox_lir_move_group_new(move_capacity: usize) -> *mut c_void {
    let move_group = LirMoveGroup::new(move_capacity);
    Box::into_raw(Box::new(move_group)) as *mut _
}

#[no_mangle]
pub unsafe extern "C" fn verifirefox_lir_move_group_push_move(move_group_ptr: *mut c_void, move_ptr: *mut c_void) {
    let move_group = &mut *(move_group_ptr as *mut LirMoveGroup);
    let move_info = *(Box::<LirMove>::from_raw(move_ptr as *mut _));
    move_group.push_move(move_info);
}

// LirMove bindings

#[no_mangle]
pub unsafe extern "C" fn verifirefox_lir_move_new(from_ptr: *mut c_void, to_ptr: *mut c_void, ty: LirDefinitionType) -> *mut c_void {
    let from = *(Box::<LirAllocation>::from_raw(from_ptr as *mut _));
    let to = *(Box::<LirAllocation>::from_raw(to_ptr as *mut _));
    let move_info = LirMove::new(from, to, ty);
    Box::into_raw(Box::new(move_info)) as *mut _
}

// LirDefinition bindings

#[no_mangle]
pub unsafe extern "C" fn verifirefox_lir_definition_new(virtual_reg: VirtualReg, ty: LirDefinitionType, policy: LirDefinitionPolicy, output_ptr: *mut c_void) -> *mut c_void {
    let output = if output_ptr.is_null() {
        None
    } else {
        Some(*(Box::<LirAllocation>::from_raw(output_ptr as *mut _)))
    };
    let definition = LirDefinition::new(virtual_reg, ty, policy, output);
    Box::into_raw(Box::new(definition)) as *mut _
}

// LirAllocation bindings

#[no_mangle]
pub unsafe extern "C" fn verifirefox_lir_allocation_new_use(policy: LirUsePolicy, virtual_reg: VirtualReg, physical_reg: PhysicalReg, used_at_start: bool) -> *mut c_void {
    let use_info = LirUse::new(policy, virtual_reg, if physical_reg == 0 { None } else { Some(physical_reg) }, used_at_start);
    let allocation = LirAllocation::LirUse(use_info);
    Box::into_raw(Box::new(allocation)) as *mut _
}

#[no_mangle]
pub unsafe extern "C" fn verifirefox_lir_allocation_new_general_reg(physical_reg: PhysicalReg) -> *mut c_void {
    let allocation = LirAllocation::LirGeneralReg(physical_reg);
    Box::into_raw(Box::new(allocation)) as *mut _
}

#[no_mangle]
pub unsafe extern "C" fn verifirefox_lir_allocation_new_float_reg(physical_reg: PhysicalReg) -> *mut c_void {
    let allocation = LirAllocation::LirFloatReg(physical_reg);
    Box::into_raw(Box::new(allocation)) as *mut _
}

#[no_mangle]
pub unsafe extern "C" fn verifirefox_lir_allocation_new_stack_slot(stack_slot_index: StackSlotIndex) -> *mut c_void {
    let allocation = LirAllocation::LirStackSlot(stack_slot_index);
    Box::into_raw(Box::new(allocation)) as *mut _
}

#[no_mangle]
pub unsafe extern "C" fn verifirefox_lir_allocation_new_argument(argument_index: ArgumentIndex) -> *mut c_void {
    let allocation = LirAllocation::LirArgument(argument_index);
    Box::into_raw(Box::new(allocation)) as *mut _
}

#[no_mangle]
pub unsafe extern "C" fn verifirefox_lir_allocation_new_other() -> *mut c_void {
    let allocation = LirAllocation::Other;
    Box::into_raw(Box::new(allocation)) as *mut _
}

// Pass verification bindings

#[no_mangle]
pub unsafe extern "C" fn verifirefox_verify_reg_allocation_pass(before_graph_ptr: *const c_void, after_graph_ptr: *const c_void) -> bool {
    let before_graph = &*(before_graph_ptr as *const LirGraph);
    let after_graph = &*(after_graph_ptr as *const LirGraph);
    println!("{:#?}", (before_graph, after_graph));
    true
}
