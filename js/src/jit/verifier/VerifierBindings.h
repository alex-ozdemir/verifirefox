/* -*- Mode: C++; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 2 -*-
 * vim: set ts=8 sts=2 et sw=2 tw=80:
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

// Interface for calling from C++ into Verifirefox.

// The functions declared here are implemented in rust/src/lib.rs.

#ifndef jit_verifier_VerifierBindings_h
#define jit_verifier_VerifierBindings_h

#include <stddef.h>
#include <stdint.h>

namespace js {
namespace jit {
namespace verifier {

typedef void (*FailCallback)();

typedef uint32_t ArgumentIndex;
typedef uint32_t PhysicalRegCode;
typedef uint32_t StackSlotIndex;
typedef uint32_t VirtualReg;

struct PhysicalLoc;

enum class LirType : uint8_t {
  GENERAL = 0,
  INT32 = 1,
  OBJECT = 2,
  SLOTS = 3,
  FLOAT32 = 4,
  DOUBLE = 5,
  SIMD128INT = 6,
  SIMD128FLOAT = 7,
  TYPE = 8,
  PAYLOAD = 9,
  BOX = 10,
};

typedef uint32_t LirNodeId;

struct LirGraph;
struct LirNode;
struct LirOperation;
struct LirMoveGroup;
struct LirMove;
struct LirDefinition;
struct LirAllocation;
struct LirUseInfo;

typedef const LirGraph* LirGraphHandle;

typedef uint32_t MirDefId;
typedef uint32_t MirBasicBlockIndex;

struct MirGraph;
struct MirBasicBlock;
struct MirInstruction;
struct MirPhi;
struct MirOperation;

typedef const MirGraph* MirGraphHandle;

}  // namespace verifier
}  // namespace jit
}  // namespace js

extern "C" {

void verifirefox_execute_init(js::jit::verifier::FailCallback failCb);

// PhysicalLoc bindings

js::jit::verifier::PhysicalLoc*
verifirefox_ast_lir_physical_loc_new_general_reg(
    js::jit::verifier::PhysicalRegCode physicalRegCode);

js::jit::verifier::PhysicalLoc* verifirefox_ast_lir_physical_loc_new_float_reg(
    js::jit::verifier::PhysicalRegCode physicalRegCode);

js::jit::verifier::PhysicalLoc* verifirefox_ast_lir_physical_loc_new_stack_slot(
    js::jit::verifier::StackSlotIndex stackSlotIndex);

js::jit::verifier::PhysicalLoc* verifirefox_ast_lir_physical_loc_new_argument(
    js::jit::verifier::ArgumentIndex argumentIndex);

// LirGraph bindings

js::jit::verifier::LirGraph* verifirefox_ast_lir_graph_new(
    uint32_t nodeCapacity);

void verifirefox_ast_lir_graph_drop(js::jit::verifier::LirGraph* graph);

void verifirefox_ast_lir_graph_put_node(js::jit::verifier::LirGraph* graph,
                                    js::jit::verifier::LirNodeId nodeId,
                                    js::jit::verifier::LirNode* node);

js::jit::verifier::LirGraphHandle verifirefox_ast_lir_graph_into_handle(
    js::jit::verifier::LirGraph* graph);

js::jit::verifier::LirGraphHandle verifirefox_ast_lir_graph_clone_handle(
    const js::jit::verifier::LirGraphHandle* graph_handle);

void verifirefox_ast_lir_graph_drop_handle(
    js::jit::verifier::LirGraphHandle graph_handle);

// LirNode bindings

js::jit::verifier::LirNode* verifirefox_ast_lir_node_new(
    js::jit::verifier::LirOperation* operation, size_t operandCapacity,
    size_t defCapacity, size_t tempCapacity, size_t predecessorCapacity,
    size_t successorCapacity, bool isAtBlockStart, uint32_t id);

void verifirefox_ast_lir_node_push_operand(
    js::jit::verifier::LirNode* node,
    js::jit::verifier::LirAllocation* operand);

void verifirefox_ast_lir_node_push_def(js::jit::verifier::LirNode* node,
                                       js::jit::verifier::LirDefinition* def);

void verifirefox_ast_lir_node_push_temp(js::jit::verifier::LirNode* node,
                                        js::jit::verifier::LirDefinition* temp);

void verifirefox_ast_lir_node_push_predecessor(
    js::jit::verifier::LirNode* node,
    js::jit::verifier::LirNodeId predecessorNodeId);

void verifirefox_ast_lir_node_push_successor(
    js::jit::verifier::LirNode* node,
    js::jit::verifier::LirNodeId successorNodeId);

// LirOperation bindings
js::jit::verifier::LirOperation* verifirefox_ast_lir_operation_new_move_group(
    js::jit::verifier::LirMoveGroup* moveGroup);

#define op(id) js::jit::verifier::LirOperation* verifirefox_ast_lir_operation_new_##id()

op(call_set_element);
op(load_element_v);
op(phi);
op(spectre_mask_index);

op(call_set_element);
op(phi);
op(spectre_mask_index);
op(array_pop_shift_v);
op(array_pop_shift_t);
op(array_push_v);
op(array_push_t);
op(store_element_v);
op(store_element_t);
op(store_element_hole_v);
op(store_element_hole_t);
op(fallible_store_element_t);
op(fallible_store_element_v);
op(store_unboxed_scalar);
op(store_unboxed_big_int);
op(store_typed_array_element_hole);
op(store_typed_array_element_hole_big_int);
op(load_element_v);
op(load_element_t);
op(load_element_hole);
op(load_unboxed_scalar);
op(load_unboxed_big_int);
op(load_typed_array_element_hole);
op(load_typed_array_element_hole_big_int);

op(array_length);
op(typed_array_length);
op(initialized_length);
op(set_initialized_length);
// js::jit::verifier::LirOperation*
// verifirefox_ast_lir_operation_new_call_set_element();

// js::jit::verifier::LirOperation*
// verifirefox_ast_lir_operation_new_load_element_v();

js::jit::verifier::LirOperation* verifirefox_ast_lir_operation_new_move_group(
    js::jit::verifier::LirMoveGroup* moveGroup);

js::jit::verifier::LirOperation* verifirefox_ast_lir_operation_new_other(
    const char*);

// js::jit::verifier::LirOperation* verifirefox_ast_lir_operation_new_phi();

// js::jit::verifier::LirOperation*
// verifirefox_ast_lir_operation_new_spectre_mask_index();

#undef op
// LirMoveGroup bindings

js::jit::verifier::LirMoveGroup* verifirefox_ast_lir_move_group_new(
    size_t moveCapacity);

void verifirefox_ast_lir_move_group_push_move(
    js::jit::verifier::LirMoveGroup* moveGroup,
    js::jit::verifier::LirMove* move);

// LirMove bindings

js::jit::verifier::LirMove* verifirefox_ast_lir_move_new(
    js::jit::verifier::LirAllocation* from,
    js::jit::verifier::LirAllocation* to,
    js::jit::verifier::LirType type);

// LirDefinition bindings

js::jit::verifier::LirDefinition*
verifirefox_ast_lir_definition_new_with_reg_policy(
    js::jit::verifier::VirtualReg virtualReg,
    js::jit::verifier::LirType type);

js::jit::verifier::LirDefinition*
verifirefox_ast_lir_definition_new_with_reuse_input_policy(
    js::jit::verifier::VirtualReg virtualReg,
    js::jit::verifier::LirType type,
    size_t input);

js::jit::verifier::LirDefinition*
verifirefox_ast_lir_definition_new_with_fixed_policy(
    js::jit::verifier::VirtualReg virtualReg,
    js::jit::verifier::LirType type,
    js::jit::verifier::PhysicalLoc* physicalLoc);

// LirAllocation bindings

js::jit::verifier::LirAllocation* verifirefox_ast_lir_allocation_new_bogus();

js::jit::verifier::LirAllocation* verifirefox_ast_lir_allocation_new_constant();

js::jit::verifier::LirAllocation* verifirefox_ast_lir_allocation_new_static(
    js::jit::verifier::PhysicalLoc* physicalLoc,
    js::jit::verifier::LirUseInfo* useInfo);

js::jit::verifier::LirAllocation* verifirefox_ast_lir_allocation_new_dynamic(
    js::jit::verifier::LirUseInfo* useInfo);

// LirUseInfo bindings

js::jit::verifier::LirUseInfo* verifirefox_ast_lir_use_info_new_with_any_policy(
    js::jit::verifier::VirtualReg virtualReg,
    bool isUsedAtStart,
    bool isRecoveredInput);

js::jit::verifier::LirUseInfo* verifirefox_ast_lir_use_info_new_with_reg_policy(
    js::jit::verifier::VirtualReg virtualReg,
    bool isUsedAtStart);

// MirOperation bindings
js::jit::verifier::MirOperation* verifirefox_ast_mir_operation_new_other();

// MirGraph bindings

js::jit::verifier::MirGraph* verifirefox_ast_mir_graph_new(size_t n_blocks);

void verifirefox_ast_mir_graph_put_block(
    js::jit::verifier::MirGraph* graph_ptr,
    size_t bb_index,
    js::jit::verifier::MirBasicBlock* bb_ptr);

js::jit::verifier::MirGraphHandle verifirefox_ast_mir_graph_into_handle(
    js::jit::verifier::MirGraph* graph);

js::jit::verifier::MirGraphHandle verifirefox_ast_mir_graph_clone_handle(
    const js::jit::verifier::MirGraphHandle* graph_handle);

void verifirefox_ast_mir_graph_drop_handle(
    js::jit::verifier::MirGraphHandle graph_handle);

// MirInstruction bindings

js::jit::verifier::MirInstruction* verifirefox_ast_mir_instruction_new(
    js::jit::verifier::MirOperation* operation_ptr,
    size_t input_capacity,
    js::jit::verifier::MirDefId def_id);

void verifirefox_ast_mir_instruction_push_input(
    js::jit::verifier::MirInstruction* instruction_ptr,
    js::jit::verifier::MirDefId input);

// MirPhi bindings

js::jit::verifier::MirPhi* verifirefox_ast_mir_phi_new(
    size_t input_capacity,
    js::jit::verifier::MirDefId input);

void verifirefox_ast_mir_phi_push_input(
    js::jit::verifier::MirPhi* phi_ptr,
    js::jit::verifier::MirDefId input);

// MirBasicBlock bindings

js::jit::verifier::MirBasicBlock* verifirefox_ast_mir_basic_block_new(
    size_t phi_capacity,
    size_t instruction_capacity,
    size_t predecessor_capacity,
    size_t successor_capacity);

void verifirefox_ast_mir_basic_block_push_phi(
    js::jit::verifier::MirBasicBlock* bb_ptr,
    js::jit::verifier::MirPhi* phi_ptr);

void verifirefox_ast_mir_basic_block_push_instruction(
    js::jit::verifier::MirBasicBlock* bb_ptr,
    js::jit::verifier::MirInstruction* instruction_ptr);

void verifirefox_ast_mir_basic_block_push_predecessor(
    js::jit::verifier::MirBasicBlock* bb_ptr,
    js::jit::verifier::MirBasicBlockIndex predecessor_idx);

void verifirefox_ast_mir_basic_block_push_successor(
    js::jit::verifier::MirBasicBlock* bb_ptr,
    js::jit::verifier::MirBasicBlockIndex successor_idx);


// RegAllocPass bindings

void verifirefox_passes_reg_alloc_sync(
    const js::jit::verifier::LirGraphHandle beforeGraph,
    const js::jit::verifier::LirGraphHandle afterGraph);

void verifirefox_passes_reg_alloc_async(
    const js::jit::verifier::LirGraphHandle beforeGraph,
    const js::jit::verifier::LirGraphHandle afterGraph);

void verifirefox_passes_spectre_async(
    const js::jit::verifier::LirGraphHandle graph);
void verifirefox_passes_spectre_sync(
    const js::jit::verifier::LirGraphHandle graph);

// LirUndefUses bindings

void verifirefox_passes_lir_undef_use_sync(
    const js::jit::verifier::LirGraphHandle graph);

void verifirefox_passes_lir_undef_use_async(
    const js::jit::verifier::LirGraphHandle graph);

void verifirefox_passes_mir_undef_use_sync(
    const js::jit::verifier::MirGraphHandle graph);

void verifirefox_passes_mir_undef_use_async(
    const js::jit::verifier::MirGraphHandle graph);

}  // extern "C"

#endif  // jit_verifier_VerifierBindings_h
