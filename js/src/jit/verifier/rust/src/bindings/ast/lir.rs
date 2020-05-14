// TODO: Use stowaway for pointer-passing?

use std::mem;
use std::os::raw::c_void;
use std::ffi::CStr;
use std::os::raw::c_char;

use crate::ast::lir::{
    LirAllocation, LirAnyUsePolicy, LirDefinition, LirDefinitionPolicy, LirDynamicAllocation,
    LirFixedDefinitionPolicy, LirMove, LirMoveGroup, LirNode, LirNodeIndex, LirOperation,
    LirReuseInputDefinitionPolicy, LirSnapshot, LirStaticAllocation, LirUseInfo, LirUsePolicy,
    PhysicalReg,
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
    id: u32,
    operation_ptr: *mut c_void,
    operand_capacity: usize,
    def_capacity: usize,
    temp_capacity: usize,
    predecessor_capacity: usize,
    successor_capacity: usize,
    snapshot_ptr: *mut c_void,
    is_at_block_start: bool,
) -> *mut c_void {
    let operation = if operation_ptr.is_null() {
        LirOperation::default()
    } else {
        *(Box::<LirOperation>::from_raw(operation_ptr as *mut _))
    };

    let snapshot = if snapshot_ptr.is_null() {
        None
    } else {
        Some(*(Box::<LirSnapshot>::from_raw(snapshot_ptr as *mut _)))
    };

    let node = LirNode::new(
        LirNodeId::from(id).into(),
        operation,
        operand_capacity,
        def_capacity,
        temp_capacity,
        predecessor_capacity,
        successor_capacity,
        snapshot,
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
macro_rules! basic_operation {
    ($rust_name:ident, $c_func:ident) => {
        #[no_mangle]
        pub unsafe extern "C" fn $c_func() -> *mut c_void {
            let operation = LirOperation::$rust_name;
            Box::into_raw(Box::new(operation)) as *mut _
        }
    }
}

basic_operation!(CallSetElement, verifirefox_ast_lir_operation_new_call_set_element);
basic_operation!(Phi, verifirefox_ast_lir_operation_new_phi);
basic_operation!(SpectreMaskIndex, verifirefox_ast_lir_operation_new_spectre_mask_index);
basic_operation!(ArrayPopShiftV, verifirefox_ast_lir_operation_new_array_pop_shift_v);
basic_operation!(ArrayPopShiftT, verifirefox_ast_lir_operation_new_array_pop_shift_t);
basic_operation!(ArrayPushV, verifirefox_ast_lir_operation_new_array_push_v);
basic_operation!(ArrayPushT, verifirefox_ast_lir_operation_new_array_push_t);
basic_operation!(StoreElementV, verifirefox_ast_lir_operation_new_store_element_v);
basic_operation!(StoreElementT, verifirefox_ast_lir_operation_new_store_element_t);
basic_operation!(StoreElementHoleV, verifirefox_ast_lir_operation_new_store_element_hole_v);
basic_operation!(StoreElementHoleT, verifirefox_ast_lir_operation_new_store_element_hole_t);
basic_operation!(FallibleStoreElementT, verifirefox_ast_lir_operation_new_fallible_store_element_t);
basic_operation!(FallibleStoreElementV, verifirefox_ast_lir_operation_new_fallible_store_element_v);
basic_operation!(StoreUnboxedScalar, verifirefox_ast_lir_operation_new_store_unboxed_scalar);
basic_operation!(StoreUnboxedBigInt, verifirefox_ast_lir_operation_new_store_unboxed_big_int);
basic_operation!(StoreTypedArrayElementHole, verifirefox_ast_lir_operation_new_store_typed_array_element_hole);
basic_operation!(StoreTypedArrayElementHoleBigInt, verifirefox_ast_lir_operation_new_store_typed_array_element_hole_big_int);
basic_operation!(LoadElementV, verifirefox_ast_lir_operation_new_load_element_v);
basic_operation!(LoadElementT, verifirefox_ast_lir_operation_new_load_element_t);
basic_operation!(LoadElementHole, verifirefox_ast_lir_operation_new_load_element_hole);
basic_operation!(LoadUnboxedScalar, verifirefox_ast_lir_operation_new_load_unboxed_scalar);
basic_operation!(LoadUnboxedBigInt, verifirefox_ast_lir_operation_new_load_unboxed_big_int);
basic_operation!(LoadTypedArrayElementHole, verifirefox_ast_lir_operation_new_load_typed_array_element_hole);
basic_operation!(LoadTypedArrayElementHoleBigInt, verifirefox_ast_lir_operation_new_load_typed_array_element_hole_big_int);

basic_operation!(ArrayLength, verifirefox_ast_lir_operation_new_array_length);
basic_operation!(TypedArrayLength, verifirefox_ast_lir_operation_new_typed_array_length);
basic_operation!(InitializedLength, verifirefox_ast_lir_operation_new_initialized_length);
basic_operation!(SetInitializedLength, verifirefox_ast_lir_operation_new_set_initialized_length);

#[no_mangle]
pub unsafe extern "C" fn verifirefox_ast_lir_operation_new_move_group(
    move_group_ptr: *mut c_void,
) -> *mut c_void {
    let move_group = *(Box::<LirMoveGroup>::from_raw(move_group_ptr as *mut _));
    let operation = LirOperation::MoveGroup(move_group);
    Box::into_raw(Box::new(operation)) as *mut _
}

#[no_mangle]
pub unsafe extern "C" fn verifirefox_ast_lir_operation_new_other(
    move_group_ptr: *const c_char,
) -> *mut c_void {
    let op_name = CStr::from_ptr(move_group_ptr).to_string_lossy().into_owned();
    let operation = LirOperation::Other(op_name);
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

// LirSnapshot bindings

#[no_mangle]
pub unsafe extern "C" fn verifirefox_ast_lir_snapshot_new(
    entry_capacity: usize,
    stack_slot_count: usize,
) -> *mut c_void {
    let snapshot = LirSnapshot::new(entry_capacity, stack_slot_count);
    Box::into_raw(Box::new(snapshot)) as *mut _
}

#[no_mangle]
pub unsafe extern "C" fn verifirefox_ast_lir_snapshot_push_entry(
    snapshot_ptr: *mut c_void,
    entry_ptr: *mut c_void,
) {
    let snapshot = &mut *(snapshot_ptr as *mut LirSnapshot);
    let entry = *(Box::<LirAllocation>::from_raw(entry_ptr as *mut _));
    snapshot.push_entry(entry);
}
