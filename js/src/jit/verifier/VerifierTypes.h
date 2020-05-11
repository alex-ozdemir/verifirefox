/* -*- Mode: C++; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 2 -*-
 * vim: set ts=8 sts=2 et sw=2 tw=80:
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#ifndef jit_verifier_VerifierTypes_h
#define jit_verifier_VerifierTypes_h

#include "jit/LIR.h"
#include "jit/MIR.h"

#include "jit/verifier/VerifierBindings.h"

namespace js {
namespace jit {
namespace verifier {

template<typename T, T (*CloneHook)(const T* inner), void (*DropHook)(T inner)>
class Handle {
    T inner_;

    void DropInner() {
      if (inner_) {
        DropHook(**this);
      }
    }

  public:
    Handle()
      : inner_(nullptr)
    {
    }

    Handle(T inner)
      : inner_(inner)
    {
    }

    Handle(Handle&& other)
      : inner_(*other)
    {
    }

    Handle& operator=(Handle&& other) {
      DropInner();
      inner_ = *other;
      return *this;
    }

    Handle(const Handle& other)
      : Handle(CloneHook(&other.inner_))
    {
    }

    Handle& operator=(const Handle& other) {
      DropInner();
      inner_ = CloneHook(&other.inner_);
      return *this;
    }

    ~Handle() {
      DropInner();
    }

    T operator*() {
      T inner(inner_);
      inner_ = nullptr;
      return inner;
    }
};

typedef
  Handle<LirGraphHandle,
         verifirefox_ast_lir_graph_clone_handle,
         verifirefox_ast_lir_graph_drop_handle>
  LIRGraph;

typedef
  Handle<MirGraphHandle,
         verifirefox_ast_mir_graph_clone_handle,
         verifirefox_ast_mir_graph_drop_handle>
  MIRGraph;

}  // namespace verifier
}  // namespace jit
}  // namespace js

#endif /* jit_verifier_VerifierTypes_h */
