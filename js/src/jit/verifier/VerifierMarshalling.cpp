/* -*- Mode: C++; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 2 -*-
 * vim: set ts=8 sts=2 et sw=2 tw=80:
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#include "jit/verifier/VerifierMarshalling.h"

using namespace js;
using namespace js::jit;

verifier::LUsePolicy MarshallLUsePolicy(const LUse::Policy policy) {
  switch (policy) {
    case LUse::ANY: {
      return verifier::LUsePolicy::ANY;
    }
    case LUse::REGISTER: {
      return verifier::LUsePolicy::REGISTER;
    }
    case LUse::FIXED: {
      return verifier::LUsePolicy::FIXED;
    }
    case LUse::KEEPALIVE: {
      return verifier::LUsePolicy::KEEPALIVE;
    }
    case LUse::RECOVERED_INPUT: {
      return verifier::LUsePolicy::RECOVERED_INPUT;
    }
    default: {
      MOZ_RELEASE_ASSERT(false, "Unrecognized LIR definition policy");
    }
  }
}

verifier::LAllocation* MarshallLAllocation(const LAllocation& allocation) {
  switch (allocation.kind()) {
    case LAllocation::CONSTANT_VALUE: {
      return verifirefox_lir_allocation_new_other();
    }
    case LAllocation::CONSTANT_INDEX: {
      return verifirefox_lir_allocation_new_other();
    }
    case LAllocation::USE: {
      const LUse& use = *allocation.toUse();
      const verifier::LUsePolicy outPolicy = MarshallLUsePolicy(use.policy());
      return verifirefox_lir_allocation_new_use(outPolicy,
                                                use.virtualRegister(),
                                                use.registerCode(),
                                                use.usedAtStart());
    }
    case LAllocation::GPR: {
      const LGeneralReg& generalReg = *allocation.toGeneralReg();
      return verifirefox_lir_allocation_new_general_reg(
          generalReg.reg().code());
    }
    case LAllocation::FPU: {
      const LFloatReg& floatReg = *allocation.toFloatReg();
      return verifirefox_lir_allocation_new_float_reg(floatReg.reg().code());
    }
    case LAllocation::STACK_SLOT: {
      const LStackSlot& stackSlot = *allocation.toStackSlot();
      return verifirefox_lir_allocation_new_stack_slot(stackSlot.slot());
    }
    case LAllocation::ARGUMENT_SLOT: {
      const LArgument& argument = *allocation.toArgument();
      return verifirefox_lir_allocation_new_argument(argument.index());
    }
    default: {
      MOZ_RELEASE_ASSERT(false, "Unrecognized LIR allocation kind");
    }
  }
}

verifier::LDefinitionType MarshallLDefinitionType(
    const LDefinition::Type type) {
  switch (type) {
    case LDefinition::GENERAL: {
      return verifier::LDefinitionType::GENERAL;
    }
    case LDefinition::INT32: {
      return verifier::LDefinitionType::INT32;
    }
    case LDefinition::OBJECT: {
      return verifier::LDefinitionType::OBJECT;
    }
    case LDefinition::SLOTS: {
      return verifier::LDefinitionType::SLOTS;
    }
    case LDefinition::FLOAT32: {
      return verifier::LDefinitionType::FLOAT32;
    }
    case LDefinition::DOUBLE: {
      return verifier::LDefinitionType::DOUBLE;
    }
    case LDefinition::SIMD128INT: {
      return verifier::LDefinitionType::SIMD128INT;
    }
    case LDefinition::SIMD128FLOAT: {
      return verifier::LDefinitionType::SIMD128FLOAT;
    }
#ifdef JS_NUNBOX32
    case LDefinition::TYPE: {
      return verifier::LDefinitionType::TYPE;
    }
    case LDefinition::PAYLOAD: {
      return verifier::LDefinitionType::PAYLOAD;
    }
#elif JS_PUNBOX64
    case LDefinition::BOX: {
      return verifier::LDefinitionType::BOX;
    }
#endif
    default: {
      MOZ_RELEASE_ASSERT(false, "Unrecognized LIR definition type");
    }
  }
}

verifier::LDefinitionPolicy MarshallLDefinitionPolicy(
    const LDefinition::Policy policy) {
  switch (policy) {
    case LDefinition::FIXED: {
      return verifier::LDefinitionPolicy::FIXED;
    }
    case LDefinition::REGISTER: {
      return verifier::LDefinitionPolicy::REGISTER;
    }
    case LDefinition::MUST_REUSE_INPUT: {
      return verifier::LDefinitionPolicy::MUST_REUSE_INPUT;
    }
    default: {
      MOZ_RELEASE_ASSERT(false, "Unrecognized LIR definition policy");
    }
  }
}

verifier::LDefinition* MarshallLDefinition(const LDefinition& definition) {
  const verifier::LDefinitionType outType =
      MarshallLDefinitionType(definition.type());
  const verifier::LDefinitionPolicy outPolicy =
      MarshallLDefinitionPolicy(definition.policy());
  verifier::LAllocation* const outOutput =
      definition.policy() == LDefinition::FIXED || definition.output()->isUse()
          ? MarshallLAllocation(*definition.output())
          : nullptr;
  return verifirefox_lir_definition_new(definition.virtualRegister(), outType,
                                        outPolicy, outOutput);
}

verifier::LMove* MarshallLMove(const LMove& move) {
  verifier::LAllocation* const outFrom = MarshallLAllocation(move.from());
  verifier::LAllocation* const outTo = MarshallLAllocation(move.to());
  const verifier::LDefinitionType outType =
      MarshallLDefinitionType(move.type());
  return verifirefox_lir_move_new(outFrom, outTo, outType);
}

verifier::LMoveGroup* MarshallLMoveGroup(const LMoveGroup& moveGroup) {
  const size_t numMoves = moveGroup.numMoves();

  verifier::LMoveGroup* outMoveGroup = verifirefox_lir_move_group_new(numMoves);

  for (size_t moveIndex = 0; moveIndex < numMoves; ++moveIndex) {
    const LMove& move = moveGroup.getMove(moveIndex);
    verifier::LMove* const outMove = MarshallLMove(move);
    verifirefox_lir_move_group_push_move(outMoveGroup, outMove);
  }

  return outMoveGroup;
}

verifier::LNode* MarshallLNode(const LNode& node) {
  verifier::LOperation* outOperation;

  switch (node.op()) {
    case LNode::Opcode::MoveGroup: {
      const LMoveGroup& moveGroup = *node.toMoveGroup();
      verifier::LMoveGroup* const outMoveGroup = MarshallLMoveGroup(moveGroup);
      outOperation = verifirefox_lir_operation_new_move_group(outMoveGroup);
      break;
    }
    default: {
      outOperation = nullptr;
      break;
    }
  }

  const size_t numOperands =
      node.isPhi()
          ? node.toPhi()->numOperands()
          : node.toInstruction()->numOperands();

  const size_t numDefs = node.numDefs();

  const size_t numTemps =
      node.isPhi()
          ? 0
          : node.toInstruction()->numTemps();

  const size_t numSuccessors =
      node.isPhi()
          ? 0
          : node.toInstruction()->numSuccessors();

  verifier::LNode* const outNode = verifirefox_lir_node_new(node.id(),
                                                            outOperation,
                                                            numOperands,
                                                            numDefs, numTemps,
                                                            numSuccessors);

  for (size_t operandIndex = 0; operandIndex < numOperands; operandIndex++) {
    const LAllocation& operand =
        node.isPhi()
            ? *node.toPhi()->getOperand(operandIndex)
            : *node.toInstruction()->getOperand(operandIndex);

    verifier::LAllocation* const outOperand = MarshallLAllocation(operand);
    verifirefox_lir_node_push_operand(outNode, outOperand);
  }

  for (size_t defIndex = 0; defIndex < numDefs; ++defIndex) {
    const LDefinition& def =
        node.isPhi()
            ? *node.toPhi()->getDef(defIndex)
            : *node.toInstruction()->getDef(defIndex);

    verifier::LDefinition* const outDef = MarshallLDefinition(def);
    verifirefox_lir_node_push_def(outNode, outDef);
  }

  if (node.isInstruction()) {
    const LInstruction& instruction = *node.toInstruction();

    for (size_t tempIndex = 0; tempIndex < numTemps; ++tempIndex) {
      const LDefinition& temp = *instruction.getTemp(tempIndex);
      verifier::LDefinition* const outTemp = MarshallLDefinition(temp);
      verifirefox_lir_node_push_temp(outNode, outTemp);
    }

    for (size_t successorIndex = 0;
         successorIndex < numSuccessors;
         ++successorIndex) {
      const MBasicBlock& successor = *instruction.getSuccessor(successorIndex);
      verifirefox_lir_node_push_successor(outNode, successor.id());
    }
  }

  return outNode;
}

verifier::LBlock* MarshallLBlock(const LBlock& block) {
  const MBasicBlock& mirBlock = *block.mir();

  const size_t numPhis = block.numPhis();

  verifier::LBlock* const outBlock = verifirefox_lir_block_new(mirBlock.id(),
                                                               numPhis);

  for (size_t phiIndex = 0; phiIndex < numPhis; ++phiIndex) {
    const LPhi& node = *block.getPhi(phiIndex);
    verifier::LNode* const outNode = MarshallLNode(node);
    verifirefox_lir_block_push_node(outBlock, outNode);
  }

  for (LInstructionIterator instructionIterator(block.begin());
       instructionIterator != block.end();
       instructionIterator++) {
    const LInstruction& node = **instructionIterator;
    verifier::LNode* const outNode = MarshallLNode(node);
    verifirefox_lir_block_push_node(outBlock, outNode);
  }

  return outBlock;
}

verifier::LIRGraph* verifier::MarshallLIRGraph(const jit::LIRGraph& graph) {
  LIRGraph* const outGraph = verifirefox_lir_graph_new(graph.numBlocks());

  for (size_t blockIndex = 0; blockIndex < graph.numBlocks(); ++blockIndex) {
    const jit::LBlock& block = *graph.getBlock(blockIndex);
    LBlock* const outBlock = MarshallLBlock(block);
    // TODO: Check result.
    verifirefox_lir_graph_put_block(outGraph, outBlock);
  }

  return outGraph;
}
