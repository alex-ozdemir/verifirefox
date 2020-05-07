// TODO: Use stowaway for pointer-passing?

use std::mem;
use std::os::raw::c_void;

use crate::ast::lir::{
    LirAllocation, LirAnyUsePolicy, LirDefinition, LirDefinitionPolicy, LirDynamicAllocation,
    LirFixedDefinitionPolicy, LirMove, LirMoveGroup, LirNode, LirNodeIndex, LirOperation,
    LirReuseInputDefinitionPolicy, LirStaticAllocation, LirUseInfo, LirUsePolicy, PhysicalReg,
};

use crate::bindings::handle::Handle;

pub use crate::ast::lir::{
    ArgumentIndex, LirGraph, LirNodeId, LirType, PhysicalLoc, PhysicalRegCode, StackSlotIndex,
    VirtualReg,
};

// PhysicalLoc bindings

#[no_mangle]
pub unsafe extern "C" fn verifirefox_ast_lir_physical_loc_new_general_reg(
    physical_reg_code: PhysicalRegCode,
) -> *mut PhysicalLoc {
    let physical_loc = PhysicalLoc::Reg(PhysicalReg::General(physical_reg_code));
    Box::into_raw(Box::new(physical_loc))
}

#[no_mangle]
pub unsafe extern "C" fn verifirefox_ast_lir_physical_loc_new_float_reg(
    physical_reg_code: PhysicalRegCode,
) -> *mut PhysicalLoc {
    let physical_loc = PhysicalLoc::Reg(PhysicalReg::Float(physical_reg_code));
    Box::into_raw(Box::new(physical_loc))
}

#[no_mangle]
pub unsafe extern "C" fn verifirefox_ast_lir_physical_loc_new_stack_slot(
    stack_slot_index: StackSlotIndex,
) -> *mut PhysicalLoc {
    let physical_loc = PhysicalLoc::StackSlot(stack_slot_index);
    Box::into_raw(Box::new(physical_loc))
}

#[no_mangle]
pub unsafe extern "C" fn verifirefox_ast_lir_physical_loc_new_argument(
    argument_index: ArgumentIndex,
) -> *mut PhysicalLoc {
    let physical_loc = PhysicalLoc::Argument(argument_index);
    Box::into_raw(Box::new(physical_loc))
}

// LirGraph bindings

#[no_mangle]
pub unsafe extern "C" fn verifirefox_ast_lir_graph_new(next_node_id: LirNodeId) -> *mut LirGraph {
    let node_count: usize = LirNodeIndex::from(next_node_id).into();
    let graph = (0..node_count)
        .map(|_| LirNode::default())
        .collect::<Box<[_]>>();
    Box::into_raw(Box::new(graph)) as *mut _
}

#[no_mangle]
pub unsafe extern "C" fn verifirefox_ast_lir_graph_put_node(
    graph_ptr: *mut LirGraph,
    node_id: LirNodeId,
    node_ptr: *mut c_void,
) {
    let graph = &mut *(graph_ptr as *mut LirGraph);
    let node = *(Box::<LirNode>::from_raw(node_ptr as *mut _));
    let node_index = LirNodeIndex::from(node_id);
    graph[node_index] = node;
}

#[no_mangle]
pub unsafe extern "C" fn verifirefox_ast_lir_graph_into_handle(
    graph_ptr: *mut LirGraph,
) -> Handle<LirGraph> {
    Box::<LirGraph>::from_raw(graph_ptr).into()
}

#[no_mangle]
pub unsafe extern "C" fn verifirefox_ast_lir_graph_clone_handle(
    graph_handle: &Handle<LirGraph>,
) -> Handle<LirGraph> {
    graph_handle.clone()
}

#[no_mangle]
pub unsafe extern "C" fn verifirefox_ast_lir_graph_drop_handle(graph_handle: Handle<LirGraph>) {
    mem::drop(graph_handle);
}

// LirNode bindings

#[no_mangle]
pub unsafe extern "C" fn verifirefox_ast_lir_node_new(
    operation_ptr: *mut c_void,
    operand_capacity: usize,
    def_capacity: usize,
    temp_capacity: usize,
    predecessor_capacity: usize,
    successor_capacity: usize,
    is_at_block_start: bool,
) -> *mut c_void {
    let operation = if operation_ptr.is_null() {
        LirOperation::Other
    } else {
        *(Box::<LirOperation>::from_raw(operation_ptr as *mut _))
    };
    let node = LirNode::new(
        operation,
        operand_capacity,
        def_capacity,
        temp_capacity,
        predecessor_capacity,
        successor_capacity,
        is_at_block_start,
    );
    Box::into_raw(Box::new(node)) as *mut _
}

#[no_mangle]
pub unsafe extern "C" fn verifirefox_ast_lir_node_push_operand(
    node_ptr: *mut c_void,
    operand_ptr: *mut c_void,
) {
    let node = &mut *(node_ptr as *mut LirNode);
    let operand = *(Box::<LirAllocation>::from_raw(operand_ptr as *mut _));
    node.push_operand(operand);
}

#[no_mangle]
pub unsafe extern "C" fn verifirefox_ast_lir_node_push_def(
    node_ptr: *mut c_void,
    def_ptr: *mut c_void,
) {
    let node = &mut *(node_ptr as *mut LirNode);
    let def = if def_ptr.is_null() {
        None
    } else {
        Some(*(Box::<LirDefinition>::from_raw(def_ptr as *mut _)))
    };
    node.push_def(def);
}

#[no_mangle]
pub unsafe extern "C" fn verifirefox_ast_lir_node_push_temp(
    node_ptr: *mut c_void,
    temp_ptr: *mut c_void,
) {
    let node = &mut *(node_ptr as *mut LirNode);
    let temp = if temp_ptr.is_null() {
        None
    } else {
        Some(*(Box::<LirDefinition>::from_raw(temp_ptr as *mut _)))
    };
    node.push_temp(temp);
}

#[no_mangle]
pub unsafe extern "C" fn verifirefox_ast_lir_node_push_predecessor(
    node_ptr: *mut c_void,
    predecessor_node_id: LirNodeId,
) {
    let node = &mut *(node_ptr as *mut LirNode);
    node.push_predecessor(predecessor_node_id);
}

#[no_mangle]
pub unsafe extern "C" fn verifirefox_ast_lir_node_push_successor(
    node_ptr: *mut c_void,
    successor_node_id: LirNodeId,
) {
    let node = &mut *(node_ptr as *mut LirNode);
    node.push_successor(successor_node_id);
}

// LirOperation bindings

#[no_mangle]
pub unsafe extern "C" fn verifirefox_ast_lir_operation_new_call_set_element() -> *mut c_void {
    let operation = LirOperation::CallSetElement;
    Box::into_raw(Box::new(operation)) as *mut _
}

#[no_mangle]
pub unsafe extern "C" fn verifirefox_ast_lir_operation_new_load_element_v() -> *mut c_void {
    let operation = LirOperation::LoadElementV;
    Box::into_raw(Box::new(operation)) as *mut _
}

#[no_mangle]
pub unsafe extern "C" fn verifirefox_ast_lir_operation_new_move_group(
    move_group_ptr: *mut c_void,
) -> *mut c_void {
    let move_group = *(Box::<LirMoveGroup>::from_raw(move_group_ptr as *mut _));
    let operation = LirOperation::MoveGroup(move_group);
    Box::into_raw(Box::new(operation)) as *mut _
}

#[no_mangle]
pub unsafe extern "C" fn verifirefox_ast_lir_operation_new_phi() -> *mut c_void {
    let operation = LirOperation::Phi;
    Box::into_raw(Box::new(operation)) as *mut _
}

#[no_mangle]
pub unsafe extern "C" fn verifirefox_ast_lir_operation_new_spectre_mask_index() -> *mut c_void {
    let operation = LirOperation::SpectreMaskIndex;
    Box::into_raw(Box::new(operation)) as *mut _
}

// LirMoveGroup bindings

#[no_mangle]
pub unsafe extern "C" fn verifirefox_ast_lir_move_group_new(move_capacity: usize) -> *mut c_void {
    let move_group = LirMoveGroup::new(move_capacity);
    Box::into_raw(Box::new(move_group)) as *mut _
}

#[no_mangle]
pub unsafe extern "C" fn verifirefox_ast_lir_move_group_push_move(
    move_group_ptr: *mut c_void,
    move_ptr: *mut c_void,
) {
    let move_group = &mut *(move_group_ptr as *mut LirMoveGroup);
    let move_info = *(Box::<LirMove>::from_raw(move_ptr as *mut _));
    move_group.push_move(move_info);
}

// LirMove bindings

#[no_mangle]
pub unsafe extern "C" fn verifirefox_ast_lir_move_new(
    from_ptr: *mut c_void,
    to_ptr: *mut c_void,
    ty: LirType,
) -> *mut c_void {
    let from = *(Box::<LirAllocation>::from_raw(from_ptr as *mut _));
    let to = *(Box::<LirAllocation>::from_raw(to_ptr as *mut _));
    let move_info = LirMove::new(from, to, ty);
    Box::into_raw(Box::new(move_info)) as *mut _
}

// LirDefinition bindings

#[no_mangle]
pub unsafe extern "C" fn verifirefox_ast_lir_definition_new_with_reg_policy(
    virtual_reg: VirtualReg,
    ty: LirType,
) -> *mut c_void {
    let definition_policy = LirDefinitionPolicy::Reg;
    let definition = LirDefinition::new(virtual_reg, ty, definition_policy);
    Box::into_raw(Box::new(definition)) as *mut _
}

#[no_mangle]
pub unsafe extern "C" fn verifirefox_ast_lir_definition_new_with_reuse_input_policy(
    virtual_reg: VirtualReg,
    ty: LirType,
    input: usize,
) -> *mut c_void {
    let reuse_input_definition_policy = LirReuseInputDefinitionPolicy::new(input);
    let definition_policy = LirDefinitionPolicy::ReuseInput(reuse_input_definition_policy);
    let definition = LirDefinition::new(virtual_reg, ty, definition_policy);
    Box::into_raw(Box::new(definition)) as *mut _
}

#[no_mangle]
pub unsafe extern "C" fn verifirefox_ast_lir_definition_new_with_fixed_policy(
    virtual_reg: VirtualReg,
    ty: LirType,
    physical_loc_ptr: *mut PhysicalLoc,
) -> *mut c_void {
    let physical_loc = *(Box::<PhysicalLoc>::from_raw(physical_loc_ptr));
    let fixed_definition_policy = LirFixedDefinitionPolicy::new(physical_loc);
    let definition_policy = LirDefinitionPolicy::Fixed(fixed_definition_policy);
    let definition = LirDefinition::new(virtual_reg, ty, definition_policy);
    Box::into_raw(Box::new(definition)) as *mut _
}

// LirAllocation bindings

#[no_mangle]
pub unsafe extern "C" fn verifirefox_ast_lir_allocation_new_bogus() -> *mut c_void {
    let allocation = LirAllocation::Bogus;
    Box::into_raw(Box::new(allocation)) as *mut _
}

#[no_mangle]
pub unsafe extern "C" fn verifirefox_ast_lir_allocation_new_constant() -> *mut c_void {
    let allocation = LirAllocation::Constant;
    Box::into_raw(Box::new(allocation)) as *mut _
}

#[no_mangle]
pub unsafe extern "C" fn verifirefox_ast_lir_allocation_new_static(
    physical_loc_ptr: *mut PhysicalLoc,
    use_info_ptr: *mut c_void,
) -> *mut c_void {
    let physical_loc = *(Box::<PhysicalLoc>::from_raw(physical_loc_ptr));

    let use_info = if use_info_ptr.is_null() {
        None
    } else {
        Some(*(Box::<LirUseInfo>::from_raw(use_info_ptr as *mut _)))
    };

    let static_allocation = LirStaticAllocation::new(physical_loc, use_info);
    let allocation = LirAllocation::Static(static_allocation);
    Box::into_raw(Box::new(allocation)) as *mut _
}

#[no_mangle]
pub unsafe extern "C" fn verifirefox_ast_lir_allocation_new_dynamic(
    use_info_ptr: *mut c_void,
) -> *mut c_void {
    let use_info = *(Box::<LirUseInfo>::from_raw(use_info_ptr as *mut _));

    let dynamic_allocation = LirDynamicAllocation::new(use_info);
    let allocation = LirAllocation::Dynamic(dynamic_allocation);
    Box::into_raw(Box::new(allocation)) as *mut _
}

// LirUseInfo bindings

#[no_mangle]
pub unsafe extern "C" fn verifirefox_ast_lir_use_info_new_with_any_policy(
    virtual_reg: VirtualReg,
    is_used_at_start: bool,
    is_recovered_input: bool,
) -> *mut c_void {
    let any_use_policy = LirAnyUsePolicy::new(is_recovered_input);
    let use_policy = LirUsePolicy::Any(any_use_policy);
    let use_info = LirUseInfo::new(virtual_reg, is_used_at_start, use_policy);
    Box::into_raw(Box::new(use_info)) as *mut _
}

#[no_mangle]
pub unsafe extern "C" fn verifirefox_ast_lir_use_info_new_with_reg_policy(
    virtual_reg: VirtualReg,
    is_used_at_start: bool,
) -> *mut c_void {
    let use_policy = LirUsePolicy::Reg;
    let use_info = LirUseInfo::new(virtual_reg, is_used_at_start, use_policy);
    Box::into_raw(Box::new(use_info)) as *mut _
}
