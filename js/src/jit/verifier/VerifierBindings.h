/* -*- Mode: C++; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 2 -*-
 * vim: set ts=8 sts=2 et sw=2 tw=80:
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

// Interface for calling from C++ into Verifirefox.

// The functions declared here are implemented in src/lib.rs.

#ifndef jit_verifier_VerifierBindings_h
#define jit_verifier_VerifierBindings_h

#include <stdint.h>

namespace js {
namespace jit {
namespace verifier {

typedef uint32_t ArgumentIndex;
typedef uint32_t PhysicalReg;
typedef uint32_t StackSlotIndex;
typedef uint32_t VirtualReg;

typedef uint32_t LBlockId;
typedef uint32_t LNodeId;

struct LIRGraph;
struct LBlock;
struct LNode;
struct LOperation;
struct LMoveGroup;
struct LMove;
struct LDefinition;
struct LAllocation;
struct LUse;

enum class LDefinitionType : uint8_t {
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

enum class LDefinitionPolicy : uint8_t {
  FIXED = 0,
  REGISTER = 1,
  MUST_REUSE_INPUT = 2,
};

enum class LUsePolicy : uint8_t {
  ANY = 0,
  REGISTER = 1,
  FIXED = 2,
  KEEPALIVE = 3,
  RECOVERED_INPUT = 4,
};

}  // namespace verifier
}  // namespace jit
}  // namespace js

extern "C" {

// LIRGraph bindings

js::jit::verifier::LIRGraph* verifirefox_lir_graph_new(uint32_t blockCount);

void verifirefox_lir_graph_drop(js::jit::verifier::LIRGraph* graph);

bool verifirefox_lir_graph_put_block(js::jit::verifier::LIRGraph* graph,
                                     js::jit::verifier::LBlock* block);

// LBlock bindings

js::jit::verifier::LBlock* verifirefox_lir_block_new(
    js::jit::verifier::LBlockId id, size_t nodeCapacity);

void verifirefox_lir_block_push_node(js::jit::verifier::LBlock* block,
                                     js::jit::verifier::LNode* node);

// LNode bindings

js::jit::verifier::LNode* verifirefox_lir_node_new(
    js::jit::verifier::LNodeId id, js::jit::verifier::LOperation* operation,
    size_t operandCapacity, size_t defCapacity, size_t tempCapacity,
    size_t successorCapacity);

void verifirefox_lir_node_push_operand(js::jit::verifier::LNode* node,
                                       js::jit::verifier::LAllocation* operand);

void verifirefox_lir_node_push_def(js::jit::verifier::LNode* node,
                                   js::jit::verifier::LDefinition* def);

void verifirefox_lir_node_push_temp(js::jit::verifier::LNode* node,
                                    js::jit::verifier::LDefinition* temp);

void verifirefox_lir_node_push_successor(js::jit::verifier::LNode* node,
                                         js::jit::verifier::LBlockId successor);

// LOperation bindings

js::jit::verifier::LOperation* verifirefox_lir_operation_new_move_group(
    js::jit::verifier::LMoveGroup* moveGroup);

// LMoveGroup bindings

js::jit::verifier::LMoveGroup* verifirefox_lir_move_group_new(
    size_t moveCapacity);

void verifirefox_lir_move_group_push_move(
    js::jit::verifier::LMoveGroup* moveGroup, js::jit::verifier::LMove* move);

// LMove bindings

js::jit::verifier::LMove* verifirefox_lir_move_new(
    js::jit::verifier::LAllocation* from,
    js::jit::verifier::LAllocation* to,
    js::jit::verifier::LDefinitionType type);

// LDefinition bindings

js::jit::verifier::LDefinition* verifirefox_lir_definition_new(
    js::jit::verifier::VirtualReg virtualReg,
    js::jit::verifier::LDefinitionType type,
    js::jit::verifier::LDefinitionPolicy policy,
    js::jit::verifier::LAllocation* output);

// LAllocation bindings

js::jit::verifier::LAllocation* verifirefox_lir_allocation_new_use(
    js::jit::verifier::LUsePolicy policy,
    js::jit::verifier::VirtualReg virtualReg,
    js::jit::verifier::PhysicalReg physicalReg,
    bool usedAtStart);

js::jit::verifier::LAllocation* verifirefox_lir_allocation_new_general_reg(
    js::jit::verifier::PhysicalReg physicalReg);

js::jit::verifier::LAllocation* verifirefox_lir_allocation_new_float_reg(
    js::jit::verifier::PhysicalReg physicalReg);

js::jit::verifier::LAllocation* verifirefox_lir_allocation_new_stack_slot(
    js::jit::verifier::StackSlotIndex stackSlotIndex);

js::jit::verifier::LAllocation* verifirefox_lir_allocation_new_argument(
    js::jit::verifier::ArgumentIndex argumentIndex);

js::jit::verifier::LAllocation* verifirefox_lir_allocation_new_other();

// Pass verification bindings

bool verifirefox_verify_reg_allocation_pass(
    const js::jit::verifier::LIRGraph* beforeGraph,
    const js::jit::verifier::LIRGraph* afterGraph);

}  // extern "C"

#endif  // jit_verifier_VerifierBindings_h
