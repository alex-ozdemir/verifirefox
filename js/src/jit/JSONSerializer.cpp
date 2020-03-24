/* -*- Mode: C++; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 2 -*-
 * vim: set ts=8 sts=2 et sw=2 tw=80:
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#include "jit/JSONSerializer.h"
#include "jit/MIR.h"
#include "jit/MIRGraph.h"

using namespace js;
using namespace js::jit;
using namespace js::jit::details;

void JSONSerializer::beginPass(uint32_t graphId, const char* const pass, const char* type) {
  beginObject();

  property("graphId", graphId);
  property("pass", pass);

  beginObjectProperty(type);
}

void JSONSerializer::endPass() {
  endObject();

  endObject();
}

//
// LIR
//

void JSONSerializer::serializeLIR(const LIRGraph& lir) {
  beginListProperty("blocks");
  for (size_t blockIndex = 0; blockIndex < lir.numBlocks(); ++blockIndex) {
    beginObject();
    serializeLBlock(*lir.getBlock(blockIndex));
    endObject();
  }
  endList();
}

void JSONSerializer::serializeLBlock(const LBlock& block) {
  const MBasicBlock& mirBlock = *block.mir();

  property("id", mirBlock.id());

  beginListProperty("predecessors");

  for (size_t predecessorIndex = 0;
       predecessorIndex < mirBlock.numPredecessors();
       ++predecessorIndex) {
    const MBasicBlock& mirPredecessor =
        *mirBlock.getPredecessor(predecessorIndex);
    value(mirPredecessor.id());
  }

  endList();

  beginListProperty("successors");

  for (size_t successorIndex = 0;
       successorIndex < mirBlock.numSuccessors();
       ++successorIndex) {
    const MBasicBlock& mirSuccessor =
        *mirBlock.getSuccessor(successorIndex);
    value(mirSuccessor.id());
  }

  endList();

  serializeLMoveGroupProperty("entryMoves", block.entryMoves());
  serializeLMoveGroupProperty("exitMoves", block.exitMoves());

  beginListProperty("nodes");

  for (size_t phiIndex = 0; phiIndex < block.numPhis(); ++phiIndex) {
    beginObject();
    serializeLNode(*block.getPhi(phiIndex));
    endObject();
  }

  for (LInstructionIterator instructionIterator(block.begin());
       instructionIterator != block.end();
       instructionIterator++) {
    beginObject();
    serializeLNode(**instructionIterator);
    endObject();
  }

  endList();
}

void JSONSerializer::serializeLNode(const LNode& node) {
  property("id", node.id());

  beginObjectProperty("operation");

  property("code", node.opName());

  switch (node.op()) {
    case LNode::Opcode::MoveGroup: {
      serializeLMoveGroup(*node.toMoveGroup());
      break;
    }
    default: {
      break;
    }
  }

  endObject();

  property("isCall", node.isCall());
  property("recoversInput", node.recoversInput());

  beginListProperty("callPreservesRegs");
  static const RegisterSet allRegisters(RegisterSet::All());
  for (AnyRegisterIterator registerIterator(allRegisters);
       registerIterator.more();
       ++registerIterator) {
    AnyRegister reg = *registerIterator;
    if (node.isCallPreserved(reg)) {
      value("%s", reg.name());
    }
  }
  endList();

  beginListProperty("operands");
  const size_t numOperands =
    node.isPhi()
      ? node.toPhi()->numOperands()
      : node.toInstruction()->numOperands();
  for (size_t operandIndex = 0; operandIndex < numOperands; operandIndex++) {
    beginObject();
    serializeLAllocation(
      node.isPhi()
        ? *node.toPhi()->getOperand(operandIndex)
        : *node.toInstruction()->getOperand(operandIndex));
    endObject();
  }
  endList();

  beginListProperty("defs");
  for (size_t defIndex = 0; defIndex < node.numDefs(); ++defIndex) {
    beginObject();
    serializeLDefinition(
      node.isPhi()
        ? *node.toPhi()->getDef(defIndex)
        : *node.toInstruction()->getDef(defIndex));
    endObject();
  }
  endList();

  if (node.isInstruction()) {
    const LInstruction& instruction = *node.toInstruction();

    beginListProperty("temps");
    const size_t numTemps = instruction.numTemps();
    for (size_t tempIndex = 0; tempIndex < numTemps; ++tempIndex) {
      beginObject();
      serializeLDefinition(*instruction.getTemp(tempIndex));
      endObject();
    }
    endList();

    beginListProperty("successors");
    const size_t numSuccessors = instruction.numSuccessors();
    for (size_t successorIndex = 0;
         successorIndex < numSuccessors;
         ++successorIndex) {
      const MBasicBlock* const successor = instruction.getSuccessor(successorIndex);
      value(successor->id());
    }
    endList();

    serializeLMoveGroupProperty("inputMoves", instruction.inputMoves());
    serializeLMoveGroupProperty("fixReuseMoves", instruction.fixReuseMoves());
    serializeLMoveGroupProperty("movesAfter", instruction.movesAfter());
  } else {
    beginListProperty("temps");
    endList();

    beginListProperty("successors");
    endList();

    serializeLMoveGroupProperty("inputMoves", nullptr);
    serializeLMoveGroupProperty("fixReuseMoves", nullptr);
    serializeLMoveGroupProperty("movesAfter", nullptr);
  }
}

void JSONSerializer::serializeLDefinition(const LDefinition& definition) {
  property("virtualRegister", definition.virtualRegister());

  beginStringProperty("type");
  serializeLDefinitionType(definition.type());
  endStringProperty();

  beginStringProperty("policy");
  switch (definition.policy()) {
    case LDefinition::FIXED: {
      out_.put("fixed");
      break;
    }
    case LDefinition::REGISTER: {
      out_.put("register");
      break;
    }
    case LDefinition::MUST_REUSE_INPUT: {
      out_.put("mustReuseInput");
      break;
    }
    default: {
      out_.put("unknown");
      break;
    }
  }
  endStringProperty();

  if (definition.policy() == LDefinition::FIXED || definition.output()->isUse()) {
    beginObjectProperty("output");
    serializeLAllocation(*definition.output());
    endObject();
  } else {
    propertyName("output");
    out_.put("null");
  }
}

void JSONSerializer::serializeLDefinitionType(const LDefinition::Type type) {
  switch (type) {
    case LDefinition::GENERAL: {
      out_.put("general");
      break;
    }
    case LDefinition::INT32: {
      out_.put("int32");
      break;
    }
    case LDefinition::OBJECT: {
      out_.put("object");
      break;
    }
    case LDefinition::SLOTS: {
      out_.put("slots");
      break;
    }
    case LDefinition::FLOAT32: {
      out_.put("float32");
      break;
    }
    case LDefinition::DOUBLE: {
      out_.put("double");
      break;
    }
    case LDefinition::SIMD128INT: {
      out_.put("simd128int");
      break;
    }
    case LDefinition::SIMD128FLOAT: {
      out_.put("simd128float");
      break;
    }
#ifdef JS_NUNBOX32
    case LDefinition::TYPE: {
      out_.put("type");
      break;
    }
    case LDefinition::PAYLOAD: {
      out_.put("payload");
      break;
    }
#elif JS_PUNBOX64
    case LDefinition::BOX: {
      out_.put("box");
      break;
    }
#endif
    default: {
      out_.put("unknown");
      break;
    }
  }
}

void JSONSerializer::serializeLAllocation(const LAllocation& allocation) {
  beginStringProperty("kind");
  switch (allocation.kind()) {
    case LAllocation::CONSTANT_VALUE: {
      out_.put("constantValue");
      endStringProperty();
      break;
    }
    case LAllocation::CONSTANT_INDEX: {
      out_.put("constantIndex");
      endStringProperty();

      const LConstantIndex* const constantIndex = allocation.toConstantIndex();

      property("index", constantIndex->index());

      break;
    }
    case LAllocation::USE: {
      out_.put("use");
      endStringProperty();

      const LUse* const use = allocation.toUse();

      beginStringProperty("policy");
      switch (use->policy()) {
        case LUse::ANY: {
          out_.put("any");
          break;
        }
        case LUse::REGISTER: {
          out_.put("register");
          break;
        }
        case LUse::FIXED: {
          out_.put("fixed");
          break;
        }
        case LUse::KEEPALIVE: {
          out_.put("keepAlive");
          break;
        }
        case LUse::RECOVERED_INPUT: {
          out_.put("recoveredInput");
          break;
        }
        default: {
          out_.put("unknown");
          break;
        }
      }
      endStringProperty();

      property("virtualRegister", use->virtualRegister());
      property("registerCode", use->registerCode());
      property("usedAtStart", use->usedAtStart());

      break;
    }
    case LAllocation::GPR: {
      out_.put("gpr");
      endStringProperty();

      const LGeneralReg* const generalReg = allocation.toGeneralReg();

      property("reg", generalReg->reg().name());

      break;
    }
    case LAllocation::FPU: {
      out_.put("fpu");
      endStringProperty();

      const LFloatReg* const floatReg = allocation.toFloatReg();

      property("reg", floatReg->reg().name());

      break;
    }
    case LAllocation::STACK_SLOT: {
      out_.put("stackSlot");
      endStringProperty();

      const LStackSlot* const stackSlot = allocation.toStackSlot();

      property("slot", stackSlot->slot());

      break;
    }
    case LAllocation::ARGUMENT_SLOT: {
      out_.put("argumentSlot");
      endStringProperty();

      const LArgument* const argument = allocation.toArgument();

      property("index", argument->index());

      break;
    }
    default: {
      out_.put("unknown");
      endStringProperty();
      break;
    }
  }
}

void JSONSerializer::serializeLMoveGroup(const LMoveGroup& instruction) {
  beginListProperty("moves");

  for (size_t moveIndex = 0; moveIndex < instruction.numMoves(); ++moveIndex) {
    beginObject();
    serializeLMove(instruction.getMove(moveIndex));
    endObject();
  }

  endList();
}

void JSONSerializer::serializeLMove(const LMove& move) {
  beginObjectProperty("from");
  serializeLAllocation(move.from());
  endObject();

  beginObjectProperty("to");
  serializeLAllocation(move.to());
  endObject();

  beginStringProperty("type");
  serializeLDefinitionType(move.type());
  endStringProperty();
}

void JSONSerializer::serializeLMoveGroupProperty(
    const char* const name, const LMoveGroup* const value) {
  if (value) {
    property(name, value->id());
  } else {
    propertyName(name);
    out_.put("null");
  }
}

//
// MIR
//

void JSONSerializer::serializeMIR(MIRGraph& mir) {
  beginListProperty("blocks");
  for (MBasicBlockIterator iter(mir.begin()); iter != mir.end(); iter++) {
    beginObject();
    serializeMBasicBlock(**iter);
    endObject();
  }
  endList();
}

void JSONSerializer::serializeMBasicBlock(MBasicBlock& block) {
  property("id", block.id());

  property("kind", block.isLoopHeader() ? "loopHeader" :
      block.isDead() ? "dead" :
      block.isSplitEdge() ? "splitEdge" :
      "normal");
  // should probably not be pending-loop-header (seems internal)

  property("unreachable", block.unreachable());

  property("marked", block.isMarked());


  beginListProperty("predecessors");

  for (size_t predecessorIndex = 0; predecessorIndex < block.numPredecessors();
      ++predecessorIndex) {
    const MBasicBlock& predecessor = *block.getPredecessor(predecessorIndex);
    value(predecessor.id());
  }

  endList();

  beginListProperty("successors");

  for (size_t successorIndex = 0; successorIndex < block.numSuccessors();
      ++successorIndex) {
    const MBasicBlock& successor = *block.getSuccessor(successorIndex);
    value(successor.id());
  }

  endList();

  // serializeLMoveGroupProperty("entryMoves", block.entryMoves());
  // serializeLMoveGroupProperty("exitMoves", block.exitMoves());

  beginObjectProperty("resumePoint");
  if (MResumePoint* resume = block.entryResumePoint()) {
    serializeMNode(*resume);
  }
  endObject();

  beginListProperty("phiNodes");
  for (MPhiIterator iter(block.phisBegin()); iter != block.phisEnd(); iter++) {
    beginObject();
    serializeMNode(**iter);
    endObject();
  }
  endList();

  beginListProperty("instructionNodes");
  for (MInstructionIterator iter(block.begin()); iter != block.end(); iter++) {
    beginObject();
    serializeMNode(**iter);
    endObject();
  }
  endList();

}

void JSONSerializer::serializeMNode(MNode& node) {

  property("kind", node.isDefinition() ? "definition" : "resumePoint");

  if (node.isDefinition()) {
    serializeMDefinition(*node.toDefinition());
  } else {
    serializeMResumePoint(*node.toResumePoint());
  }
}

void JSONSerializer::serializeMDefinition(MDefinition& def) {
  property("id", def.id());
  property("type", StringFromMIRType(def.type()));
  property("opName", def.opName());

  beginListProperty("operands");
  for (size_t i = 0; i < def.numOperands(); i++) {
    auto op = def.getOperand(i);
    beginObject();
    if (op) {
      property("name", op->opName());
      property("type", StringFromMIRType(op->type()));
      property("id", op->id());
      property("blockId", op->block()->id());
    }
    endObject();
  }
  endList();

  if (def.isInstruction()) {
    if (MResumePoint* resume = def.toInstruction()->resumePoint()) {
      beginObjectProperty("resumePoint");
      serializeMResumePoint(*resume);
      endObject();
    }
  }
}

void JSONSerializer::serializeMResumePoint(MResumePoint& rp) {
  property("mode", rp.mode() == MResumePoint::ResumeAt ? "ResumeAt" :
      rp.mode() == MResumePoint::ResumeAfter ? "ResumeAfter" : "Outer");

  if (rp.mode() == MResumePoint::ResumeAt && rp.instruction()) {
    property("resumeAt", rp.instruction()->id());
  }

  if (MResumePoint* c = rp.caller()) {
    property("callerInBlock", c->block()->id());
  }

  beginListProperty("operands");
  for (size_t i = 0; i < rp.numOperands(); i++) {
    auto op = rp.getOperand(i);
    beginObject();
    if (op) {
      property("name", op->opName());
      property("type", StringFromMIRType(op->type()));
      property("id", op->id());
      property("blockId", op->block()->id());
    }
    endObject();
  }
  endList();
}
