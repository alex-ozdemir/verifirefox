/* -*- Mode: C++; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 2 -*-
 * vim: set ts=8 sts=2 et sw=2 tw=80:
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#ifndef jit_verifier_VerifierPasses_h
#define jit_verifier_VerifierPasses_h

#include "jit/verifier/VerifierBindings.h"

namespace js {
namespace jit {
namespace verifier {

inline void RunRegAllocPassSync(LIRGraph&& beforeGraph, LIRGraph&& afterGraph) {
  verifirefox_passes_reg_alloc_sync(*beforeGraph, *afterGraph);
}

inline void RunRegAllocPassAsync(LIRGraph&& beforeGraph,
                                 LIRGraph&& afterGraph) {
  verifirefox_passes_reg_alloc_async(*beforeGraph, *afterGraph);
}

}  // namespace verifier
}  // namespace jit
}  // namespace js

#endif /* jit_verifier_VerifierPasses_h */
