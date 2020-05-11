/* -*- Mode: C++; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 2 -*-
 * vim: set ts=8 sts=2 et sw=2 tw=80:
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */


#include "jit/verifier/VerifierMarshalling.h"

#include <unordered_map>
#include <vector>

#include "jit/MIRGraph.h"
#include "jit/verifier/VerifierBindings.h"

using namespace js;
using namespace js::jit;

verifier::LirType MarshallLirType(
    const LDefinition::Type type) {
  switch (type) {
    case LDefinition::GENERAL: {
      return verifier::LirType::GENERAL;
    }
    case LDefinition::INT32: {
      return verifier::LirType::INT32;
    }
    case LDefinition::OBJECT: {
      return verifier::LirType::OBJECT;
    }
    case LDefinition::SLOTS: {
      return verifier::LirType::SLOTS;
    }
    case LDefinition::FLOAT32: {
      return verifier::LirType::FLOAT32;
    }
    case LDefinition::DOUBLE: {
      return verifier::LirType::DOUBLE;
    }
    case LDefinition::SIMD128INT: {
      return verifier::LirType::SIMD128INT;
    }
    case LDefinition::SIMD128FLOAT: {
      return verifier::LirType::SIMD128FLOAT;
    }
#ifdef JS_NUNBOX32
    case LDefinition::TYPE: {
      return verifier::LirType::TYPE;
    }
    case LDefinition::PAYLOAD: {
      return verifier::LirType::PAYLOAD;
    }
#elif JS_PUNBOX64
    case LDefinition::BOX: {
      return verifier::LirType::BOX;
    }
#endif
    default: {
      MOZ_RELEASE_ASSERT(false, "Unrecognized LIR type");
    }
  }
}

verifier::LirUseInfo* MarshallLirUseInfo(const LUse& use) {
  const verifier::VirtualReg outVirtualReg = use.virtualRegister();
  MOZ_ASSERT(outVirtualReg);

  const bool outIsUsedAtStart = use.usedAtStart();

  switch (use.policy()) {
    case LUse::ANY:
    case LUse::KEEPALIVE:
    case LUse::RECOVERED_INPUT: {
      bool outIsRecoveredInput = use.policy() == LUse::RECOVERED_INPUT;
      return verifirefox_ast_lir_use_info_new_with_any_policy(outVirtualReg,
                                                          outIsUsedAtStart,
                                                          outIsRecoveredInput);
    }
    case LUse::REGISTER:
    case LUse::FIXED: {
      return verifirefox_ast_lir_use_info_new_with_reg_policy(outVirtualReg,
                                                          outIsUsedAtStart);
    }
    default: {
      MOZ_RELEASE_ASSERT(false, "Unrecognized LIR use policy");
    }
  }
}

verifier::PhysicalLoc* MarshallPhysicalLoc(const LAllocation& allocation) {
  switch (allocation.kind()) {
    case LAllocation::GPR: {
      const LGeneralReg& generalReg = *allocation.toGeneralReg();
      return verifirefox_ast_lir_physical_loc_new_general_reg(
          generalReg.reg().code());
    }
    case LAllocation::FPU: {
      const LFloatReg& floatReg = *allocation.toFloatReg();
      return verifirefox_ast_lir_physical_loc_new_float_reg(
          floatReg.reg().code());
    }
    case LAllocation::STACK_SLOT: {
      const LStackSlot& stackSlot = *allocation.toStackSlot();
      return verifirefox_ast_lir_physical_loc_new_stack_slot(stackSlot.slot());
    }
    case LAllocation::ARGUMENT_SLOT: {
      const LArgument& argument = *allocation.toArgument();
      return verifirefox_ast_lir_physical_loc_new_argument(argument.index());
    }
    default: {
      MOZ_RELEASE_ASSERT(false,
                         "Unrecognized or unsupported LIR allocation kind");
    }
  }
}

verifier::LirAllocation* MarshallLirAllocation(const LAllocation& allocation) {
  if (allocation.isBogus()) {
    return verifirefox_ast_lir_allocation_new_bogus();
  }

  switch (allocation.kind()) {
    case LAllocation::CONSTANT_VALUE:
    case LAllocation::CONSTANT_INDEX: {
      return verifirefox_ast_lir_allocation_new_constant();
    }
    case LAllocation::USE: {
      const LUse& use = *allocation.toUse();
      verifier::LirUseInfo* const outUseInfo = MarshallLirUseInfo(use);

      switch (use.policy()) {
        case LUse::FIXED: {
          const verifier::PhysicalRegCode outPhysicalRegCode =
              use.registerCode();
          verifier::PhysicalLoc* const outPhysicalLoc =
              AnyRegister::FromCode(outPhysicalRegCode).isFloat()
                  ? verifirefox_ast_lir_physical_loc_new_float_reg(
                      outPhysicalRegCode)
                  : verifirefox_ast_lir_physical_loc_new_general_reg(
                      outPhysicalRegCode);
          return verifirefox_ast_lir_allocation_new_static(outPhysicalLoc,
                                                       outUseInfo);
        }
        default: {
          MOZ_ASSERT(use.policy() == LUse::ANY ||
                     use.policy() == LUse::REGISTER ||
                     use.policy() == LUse::KEEPALIVE ||
                     use.policy() == LUse::RECOVERED_INPUT,
                     "Unrecognized LIR use policy");
          return verifirefox_ast_lir_allocation_new_dynamic(outUseInfo);
        }
      }
    }
    case LAllocation::GPR:
    case LAllocation::FPU:
    case LAllocation::STACK_SLOT:
    case LAllocation::ARGUMENT_SLOT: {
      verifier::PhysicalLoc* const outPhysicalLoc =
          MarshallPhysicalLoc(allocation);
      return verifirefox_ast_lir_allocation_new_static(outPhysicalLoc, nullptr);
    }
    default: {
      MOZ_RELEASE_ASSERT(false, "Unrecognized LIR allocation kind");
    }
  }
}

verifier::LirDefinition* MarshallLirDefinition(const LDefinition& definition) {
  if (definition.isBogus()) {
    return nullptr;
  }

  const verifier::VirtualReg outVirtualReg = definition.virtualRegister();
  MOZ_ASSERT(outVirtualReg);

  const verifier::LirType outType = MarshallLirType(definition.type());

  switch (definition.policy()) {
    case LDefinition::FIXED: {
      verifier::PhysicalLoc* const outPhysicalLoc =
          MarshallPhysicalLoc(*definition.output());
      return verifirefox_ast_lir_definition_new_with_fixed_policy(outVirtualReg,
                                                              outType,
                                                              outPhysicalLoc);
    }
    case LDefinition::REGISTER: {
      MOZ_ASSERT(definition.output()->isBogus());
      return verifirefox_ast_lir_definition_new_with_reg_policy(outVirtualReg,
                                                            outType);
    }
    case LDefinition::MUST_REUSE_INPUT: {
      const size_t outInput = definition.getReusedInput();
      return verifirefox_ast_lir_definition_new_with_reuse_input_policy(
          outVirtualReg, outType, outInput);
    }
  }
}

verifier::LirMove* MarshallLirMove(const LMove& move) {
  verifier::LirAllocation* const outFrom = MarshallLirAllocation(move.from());
  verifier::LirAllocation* const outTo = MarshallLirAllocation(move.to());
  const verifier::LirType outType = MarshallLirType(move.type());
  return verifirefox_ast_lir_move_new(outFrom, outTo, outType);
}

verifier::LirMoveGroup* MarshallLirMoveGroup(const LMoveGroup& moveGroup) {
  const size_t numMoves = moveGroup.numMoves();

  verifier::LirMoveGroup* outMoveGroup =
      verifirefox_ast_lir_move_group_new(numMoves);

  for (size_t moveIndex = 0; moveIndex < numMoves; ++moveIndex) {
    const LMove& move = moveGroup.getMove(moveIndex);
    verifier::LirMove* const outMove = MarshallLirMove(move);
    verifirefox_ast_lir_move_group_push_move(outMoveGroup, outMove);
  }

  return outMoveGroup;
}

verifier::LirNode* MarshallLirNode(const LNode& node,
                                   const LNode* const prevNode,
                                   const LNode* const nextNode) {
  verifier::LirOperation* outOperation;

  switch (node.op()) {
    case LNode::Opcode::CallSetElement: {
      outOperation = verifirefox_ast_lir_operation_new_call_set_element();
      break;
    }
    case LNode::Opcode::LoadElementV: {
      outOperation = verifirefox_ast_lir_operation_new_load_element_v();
      break;
    }
    case LNode::Opcode::MoveGroup: {
      const LMoveGroup& moveGroup = *node.toMoveGroup();
      verifier::LirMoveGroup* const outMoveGroup =
          MarshallLirMoveGroup(moveGroup);
      outOperation = verifirefox_ast_lir_operation_new_move_group(outMoveGroup);
      break;
    }
    case LNode::Opcode::Phi: {
      outOperation = verifirefox_ast_lir_operation_new_phi();
      break;
    }
    case LNode::Opcode::SpectreMaskIndex: {
      outOperation = verifirefox_ast_lir_operation_new_spectre_mask_index();
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

  const size_t numPredecessors =
      prevNode
          ? 1
          : node.block()->mir()->numPredecessors();

  const size_t numSuccessors =
      nextNode
          ? 1
          : node.block()->mir()->numSuccessors();

  verifier::LirNode* const outNode = verifirefox_ast_lir_node_new(
      outOperation, numOperands, numDefs, numTemps, numPredecessors,
      numSuccessors, prevNode == nullptr);

  for (size_t operandIndex = 0; operandIndex < numOperands; operandIndex++) {
    const LAllocation& operand =
        node.isPhi()
            ? *node.toPhi()->getOperand(operandIndex)
            : *node.toInstruction()->getOperand(operandIndex);

    verifier::LirAllocation* const outOperand = MarshallLirAllocation(operand);
    verifirefox_ast_lir_node_push_operand(outNode, outOperand);
  }

  for (size_t defIndex = 0; defIndex < numDefs; ++defIndex) {
    const LDefinition& def =
        node.isPhi()
            ? *node.toPhi()->getDef(defIndex)
            : *node.toInstruction()->getDef(defIndex);

    verifier::LirDefinition* const outDef = MarshallLirDefinition(def);
    verifirefox_ast_lir_node_push_def(outNode, outDef);
  }

  if (node.isInstruction()) {
    const LInstruction& instruction = *node.toInstruction();

    for (size_t tempIndex = 0; tempIndex < numTemps; ++tempIndex) {
      const LDefinition& temp = *instruction.getTemp(tempIndex);
      verifier::LirDefinition* const outTemp = MarshallLirDefinition(temp);
      verifirefox_ast_lir_node_push_temp(outNode, outTemp);
    }
  }

  if (prevNode) {
    verifirefox_ast_lir_node_push_predecessor(outNode, prevNode->id());
  } else {
    const MBasicBlock& mirBlock = *node.block()->mir();
    for (size_t predecessorIndex = 0;
         predecessorIndex < numPredecessors;
         ++predecessorIndex) {
      const MBasicBlock& predecessorMirBlock =
          *mirBlock.getPredecessor(predecessorIndex);
      verifirefox_ast_lir_node_push_predecessor(
          outNode, predecessorMirBlock.lir()->lastId());
    }
  }

  if (nextNode) {
    verifirefox_ast_lir_node_push_successor(outNode, nextNode->id());
  } else {
    const MBasicBlock& mirBlock = *node.block()->mir();
    for (size_t successorIndex = 0;
         successorIndex < numSuccessors;
         ++successorIndex) {
      MBasicBlock& successorMirBlock =
          *mirBlock.getSuccessor(successorIndex);
      verifirefox_ast_lir_node_push_successor(outNode,
                                          successorMirBlock.lir()->firstId());
    }
  }

  return outNode;
}

void MarshallLirNodes(verifier::LirGraph* const outGraph, const LBlock& block) {
  const LNode* prevNode = nullptr;

  const size_t numPhis = block.numPhis();

  size_t nextPhiIndex = 0;
  const LPhi* nextPhi =
      nextPhiIndex < numPhis
          ? block.getPhi(nextPhiIndex)
          : nullptr;

  while (nextPhi) {
    const LPhi& phi = *nextPhi;

    ++nextPhiIndex;
    nextPhi =
        nextPhiIndex < numPhis
            ? block.getPhi(nextPhiIndex)
            : nullptr;

    const LNode* const nextNode =
        nextPhi
            ? static_cast<const LNode*>(nextPhi)
            : static_cast<const LNode*>(*block.begin());

    verifier::LirNode* const outNode = MarshallLirNode(phi, prevNode, nextNode);
    verifirefox_ast_lir_graph_put_node(outGraph, phi.id(), outNode);

    prevNode = &phi;
  }

  for (LInstructionIterator instructionIterator(block.begin());
       instructionIterator != block.end();
       ++instructionIterator) {
    const LInstruction& instruction = **instructionIterator;

    ++instructionIterator;
    const LNode* const nextNode =
        instructionIterator == block.end()
            ? nullptr
            : *instructionIterator;
    --instructionIterator;

    verifier::LirNode* const outNode = MarshallLirNode(instruction, prevNode,
                                                       nextNode);
    verifirefox_ast_lir_graph_put_node(outGraph, instruction.id(), outNode);

    prevNode = &instruction;
  }
}

verifier::LIRGraph verifier::MarshallLirGraph(
    const jit::LIRGraph& graph) {
  LirGraph* const outGraph =
      verifirefox_ast_lir_graph_new(graph.numInstructions());

  for (size_t blockIndex = 0; blockIndex < graph.numBlocks(); ++blockIndex) {
    const jit::LBlock& block = *graph.getBlock(blockIndex);
    MarshallLirNodes(outGraph, block);
  }

  return LIRGraph(verifirefox_ast_lir_graph_into_handle(outGraph));
}

verifier::MirInstruction* MarshallMirInstruction(const jit::MInstruction& instr) {
  verifier::MirOperation* op;
  switch (instr.op()) {
    default:
      op = verifirefox_ast_mir_operation_new_other();
      break;
  }
  verifier::MirInstruction* const out =
    verifirefox_ast_mir_instruction_new(op, instr.numOperands(), instr.id());
  for (size_t i = 0; i < instr.numOperands(); ++i) {
    verifirefox_ast_mir_instruction_push_input(out, instr.getOperand(i)->id());
  }
  return out;
}

verifier::MirPhi* MarshallMirPhi(const jit::MPhi& phi) {
  verifier::MirPhi* const out =
    verifirefox_ast_mir_phi_new(phi.numOperands(), phi.id());
  for (size_t i = 0; i < phi.numOperands(); ++i) {
    verifirefox_ast_mir_phi_push_input(out, phi.getOperand(i)->id());
  }
  return out;
}

verifier::MirBasicBlock* MarshallMirBasicBlock(jit::MBasicBlock& block, const std::unordered_map<uint32_t, size_t>& blockIdsToIndices) {
  std::vector<verifier::MirPhi*> phis;
  for (auto phi = block.phisBegin(); phi != block.phisEnd(); ++phi) {
    phis.push_back(MarshallMirPhi(**phi));
  }
  std::vector<verifier::MirInstruction*> instrs;
  for (auto i = block.begin(); i != block.end(); ++i) {
    instrs.push_back(MarshallMirInstruction(**i));
  }
  verifier::MirBasicBlock* const out =
    verifirefox_ast_mir_basic_block_new(phis.size(), instrs.size(), block.numPredecessors(), block.numSuccessors());
  for (auto phi : phis) {
    verifirefox_ast_mir_basic_block_push_phi(out, phi);
  }
  for (auto instr : instrs) {
    verifirefox_ast_mir_basic_block_push_instruction(out, instr);
  }
  for (size_t i = 0; i != block.numPredecessors(); ++i) {
    verifirefox_ast_mir_basic_block_push_predecessor(out, blockIdsToIndices.at(block.getPredecessor(i)->id()));
  }
  for (size_t i = 0; i != block.numSuccessors(); ++i) {
    verifirefox_ast_mir_basic_block_push_successor(out, blockIdsToIndices.at(block.getSuccessor(i)->id()));
  }
  return out;
}

verifier::MIRGraph verifier::MarshallMirGraph(
    jit::MIRGraph& graph) {
  MirGraph* const outGraph =
      verifirefox_ast_mir_graph_new(graph.numBlocks());

  // Record block indices
  std::unordered_map<uint32_t, size_t> blockIdsToIndices;
  {
    size_t blockIndex = 0;
    for (jit::MBasicBlock* block : graph) {
      blockIdsToIndices.emplace( block->id(), blockIndex );
      ++blockIndex;
    }
  }

  //
  for (jit::MBasicBlock* block : graph) {
    verifirefox_ast_mir_graph_put_block(outGraph, blockIdsToIndices.at(block->id()), MarshallMirBasicBlock(*block, blockIdsToIndices));
  }

  return MIRGraph(verifirefox_ast_mir_graph_into_handle(outGraph));
}
