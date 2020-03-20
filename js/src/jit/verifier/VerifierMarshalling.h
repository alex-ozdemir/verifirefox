/* -*- Mode: C++; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 2 -*-
 * vim: set ts=8 sts=2 et sw=2 tw=80:
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#ifndef jit_verifier_VerifierMarshalling_h
#define jit_verifier_VerifierMarshalling_h

#include "jit/LIR.h"
#include "jit/MIR.h"

#include "jit/verifier/VerifierBindings.h"

namespace js {
namespace jit {
namespace verifier {

LIRGraph* MarshallLIRGraph(const jit::LIRGraph& graph);

inline void DropLIRGraph(LIRGraph* graph) {
  verifirefox_lir_graph_drop(graph);
}

}  // namespace verifier
}  // namespace jit
}  // namespace js

#endif /* jit_verifier_VerifierMarshalling_h */
