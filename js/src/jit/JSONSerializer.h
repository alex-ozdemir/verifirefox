/* -*- Mode: C++; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 2 -*-
 * vim: set ts=8 sts=2 et sw=2 tw=80:
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#ifndef jit_JSONSerializer_h
#define jit_JSONSerializer_h

#include "jit/LIR.h"
#include "jit/MIR.h"
#include "jit/MIRGraph.h"
#include "vm/JSONPrinter.h"

namespace js {
namespace jit {

class JSONSerializer : JSONPrinter {
 public:
  explicit JSONSerializer(GenericPrinter& out) : JSONPrinter(out) {}

  void beginPass(uint32_t graphId, const char* pass, const char* type);
  void endPass();

  // LIR

  void serializeLIR(const LIRGraph& lir);
  void serializeLBlock(const LBlock& block);
  void serializeLNode(const LNode& node);
  void serializeLDefinition(const LDefinition& definition);
  void serializeLDefinitionType(LDefinition::Type type);
  void serializeLAllocation(const LAllocation& allocation);

  void serializeLMoveGroup(const LMoveGroup& instruction);
  void serializeLMove(const LMove& move);

  // MIR

  void serializeMIR(MIRGraph& mir);
  void serializeMBasicBlock(MBasicBlock& block);
  void serializeMNode(MNode& node);
  void serializeMDefinition(MDefinition& def);
  void serializeMResumePoint(MResumePoint& rp);

 private:
  void serializeLMoveGroupProperty(const char* name,
                                   const LMoveGroup* moveGroupPtr);
};

}  // namespace jit
}  // namespace js

#endif /* jit_JSONSerializer_h */
