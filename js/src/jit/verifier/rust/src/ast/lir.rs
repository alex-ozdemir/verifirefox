use std::collections::HashSet;
use std::fmt;

use typed_index_derive::TypedIndex;

pub type VirtualReg = u32;
pub type PhysicalRegCode = u32;
pub type StackSlotIndex = u32;
pub type ArgumentIndex = u32;

// TODO: Encode platform-specific knowledge of register codes on the Rust side.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum PhysicalReg {
    General(PhysicalRegCode),
    Float(PhysicalRegCode),
}

impl PhysicalReg {
    pub fn is_general(&self) -> bool {
        match self {
            PhysicalReg::General(_) => true,
            _ => false,
        }
    }

    pub fn is_float(&self) -> bool {
        match self {
            PhysicalReg::Float(_) => true,
            _ => false,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum PhysicalLoc {
    Reg(PhysicalReg),
    StackSlot(StackSlotIndex),
    Argument(ArgumentIndex),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(u8)]
pub enum LirType {
    General = 0,
    Int32 = 1,
    Object = 2,
    Slots = 3,
    Float32 = 4,
    Double = 5,
    Simd128Int = 6,
    Simd128Float = 7,
    Type = 8,
    Payload = 9,
    Box = 10,
}

#[derive(Clone, Copy, Default, Eq, Ord, PartialEq, PartialOrd, Hash)]
#[repr(transparent)]
pub struct LirNodeId(u32);

impl fmt::Debug for LirNodeId {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        fmt::Debug::fmt(&self.0, f)
    }
}

impl fmt::Display for LirNodeId {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        fmt::Display::fmt(&self.0, f)
    }
}

impl From<u32> for LirNodeId {
    fn from(lir_node_id: u32) -> Self {
        LirNodeId(lir_node_id)
    }
}

impl From<&u32> for LirNodeId {
    fn from(lir_node_id: &u32) -> Self {
        LirNodeId::from(*lir_node_id)
    }
}

impl From<LirNodeId> for u32 {
    fn from(lir_node_id: LirNodeId) -> Self {
        lir_node_id.0
    }
}

impl From<&LirNodeId> for u32 {
    fn from(lir_node_id: &LirNodeId) -> Self {
        u32::from(*lir_node_id)
    }
}

#[derive(Clone, Copy, Default, Eq, Ord, PartialEq, PartialOrd, Hash, TypedIndex)]
#[typed_index(LirNode)]
pub struct LirNodeIndex(pub usize);

impl fmt::Debug for LirNodeIndex {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        fmt::Debug::fmt(&self.0, f)
    }
}

impl fmt::Display for LirNodeIndex {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        fmt::Display::fmt(&self.0, f)
    }
}

impl From<LirNodeId> for LirNodeIndex {
    fn from(lir_node_id: LirNodeId) -> Self {
        LirNodeIndex(u32::from(lir_node_id) as usize - 1)
    }
}

impl From<&LirNodeId> for LirNodeIndex {
    fn from(lir_node_id: &LirNodeId) -> Self {
        LirNodeIndex::from(*lir_node_id)
    }
}

pub type LirGraph = Box<[LirNode]>;

#[derive(Clone, Debug, Default)]
pub struct LirNode {
    operation: LirOperation,
    operands: Vec<LirAllocation>,
    defs: Vec<Option<LirDefinition>>,
    temps: Vec<Option<LirDefinition>>,
    predecessors: Vec<LirNodeIndex>,
    successors: Vec<LirNodeIndex>,
    is_at_block_start: bool,
    index: LirNodeIndex,
}

impl LirNode {
    pub fn new(
        operation: LirOperation,
        operand_capacity: usize,
        def_capacity: usize,
        temp_capacity: usize,
        predecessor_capacity: usize,
        successor_capacity: usize,
        is_at_block_start: bool,
        index: LirNodeIndex,
    ) -> Self {
        LirNode {
            operation,
            operands: Vec::with_capacity(operand_capacity),
            defs: Vec::with_capacity(def_capacity),
            temps: Vec::with_capacity(temp_capacity),
            predecessors: Vec::with_capacity(predecessor_capacity),
            successors: Vec::with_capacity(successor_capacity),
            is_at_block_start,
            index,
        }
    }

    pub fn operation(&self) -> &LirOperation {
        &self.operation
    }

    pub fn set_operation(&mut self, operation: LirOperation) {
        self.operation = operation;
    }

    pub fn operands(&self) -> &[LirAllocation] {
        &self.operands
    }

    pub fn push_operand(&mut self, operand: LirAllocation) {
        self.operands.push(operand);
    }

    pub fn defs(&self) -> &[Option<LirDefinition>] {
        &self.defs
    }

    pub fn push_def(&mut self, def: Option<LirDefinition>) {
        self.defs.push(def);
    }

    pub fn temps(&self) -> &[Option<LirDefinition>] {
        &self.temps
    }

    pub fn push_temp(&mut self, temp: Option<LirDefinition>) {
        self.temps.push(temp);
    }

    pub fn predecessors(&self) -> &[LirNodeIndex] {
        &self.predecessors
    }

    pub fn push_predecessor(&mut self, predecessor_node_id: LirNodeId) {
        self.predecessors.push(predecessor_node_id.into());
    }

    pub fn successors(&self) -> &[LirNodeIndex] {
        &self.successors
    }

    pub fn push_successor(&mut self, successor_node_id: LirNodeId) {
        self.successors.push(successor_node_id.into());
    }

    pub fn index(&self) -> LirNodeIndex {
        self.index
    }

    pub fn is_at_block_start(&self) -> bool {
        self.is_at_block_start
    }

    pub fn set_is_at_block_start(&mut self, value: bool) {
        self.is_at_block_start = value;
    }

    pub fn regs(&self) -> HashSet<VirtualReg> {
        self.operands
            .iter()
            .filter_map(|o| o.use_info().map(|i| i.virtual_reg))
            .chain(
                self.defs
                    .iter()
                    .filter_map(|od| od.as_ref().map(|d| d.virtual_reg)),
            )
            .collect()
    }
}

#[derive(Clone, Debug)]
pub enum LirOperation {
    CallSetElement,
    MoveGroup(LirMoveGroup),
    Phi,

    SpectreMaskIndex,

    ArrayPopShiftV,
    ArrayPopShiftT,
    ArrayPushV,
    ArrayPushT,
    StoreElementV,
    StoreElementT,
    StoreElementHoleV,
    StoreElementHoleT,
    FallibleStoreElementT,
    FallibleStoreElementV,
    StoreUnboxedScalar,
    StoreUnboxedBigInt,
    StoreTypedArrayElementHole,
    StoreTypedArrayElementHoleBigInt,
    LoadElementV,
    LoadElementT,
    LoadElementHole,
    LoadUnboxedScalar,
    LoadUnboxedBigInt,
    LoadTypedArrayElementHole,
    LoadTypedArrayElementHoleBigInt,

    ArrayLength,
    TypedArrayLength,
    InitializedLength,
    SetInitializedLength,
    Other(String),
}

impl Default for LirOperation {
    fn default() -> Self {
        LirOperation::Other("uninit".to_string())
    }
}

pub(crate) mod typed_ops {
    use super::*;
    macro_rules! operation_args {
        [ $($op:ident$(($($arg:ident),+))?),*$(,)? ] => {
            use ref_cast::RefCast;
            $(
                #[derive(RefCast)]
                #[repr(transparent)]
                pub struct $op(LirNode);

                #[allow(dead_code)]
                impl $op {
                    pub fn from_node(node: &LirNode) -> Option<&Self> {
                        match node.operation() {
                            LirOperation::$op => Some($op::ref_cast(node)),
                            _ => None,
                        }
                    }

                    pub fn node(&self) -> &LirNode {
                        &self.0
                    }

                    operation_field_impl!(0, $($($arg),+)?);
                }
            )*
        };
    }

    macro_rules! operation_field_impl {
        ($index:expr, $field:ident) => {
            pub fn $field(&self) -> &LirAllocation {
                &self.0.operands()[$index]
            }
        };

        ($index:expr, $field:ident, $($rest:ident),+) => {
            operation_field_impl!($index, $field);

            operation_field_impl!($index + 1, $($rest),+);
        };

        ($index:expr,) => {};
    }

    operation_args![
        Phi,
        CallSetElement,
        SpectreMaskIndex(index, length),
        ArrayPushV(array, value),
        ArrayPushT(array, value),
        StoreElementV(array, index, value),
        StoreElementT(array, index, value),
        StoreElementHoleV(object, array, index, value),
        StoreElementHoleT(object, array, index, value),
        FallibleStoreElementT(object, array, index),
        FallibleStoreElementV(object, array, index),
        StoreUnboxedScalar(array, index, value),
        StoreUnboxedBigInt(array, index, value),
        StoreTypedArrayElementHole(array, length, index, value),
        StoreTypedArrayElementHoleBigInt(array, length, index, value),
        LoadElementV(array, index),
        LoadElementT(array, index),
        LoadElementHole(array, index, init_length),
        LoadUnboxedScalar(array, index),
        LoadUnboxedBigInt(array, index),
        LoadTypedArrayElementHole(object, index),
        LoadTypedArrayElementHoleBigInt(object, index),
        ArrayLength(array),
        TypedArrayLength(obj),
        InitializedLength(array),
    ];


    #[macro_export]
    macro_rules! match_op {

        (match $node:ident { $($tt:tt)* }) => { match_op!(match ($node) { $($tt)* }) };

        (match ($node:expr) {
            $( typed_ops::$op:ident($it:ident) => $res:expr, )*
            _ => $catch_all:expr $(,)?
        }) => {{
            $( if let Some($it) = typed_ops::$op::from_node($node) { $res } else )*
            { $catch_all }
        }};
    }
}

#[derive(Clone, Debug)]
pub struct LirMoveGroup {
    moves: Vec<LirMove>,
}

impl LirMoveGroup {
    pub fn new(move_capacity: usize) -> Self {
        LirMoveGroup {
            moves: Vec::with_capacity(move_capacity),
        }
    }

    pub fn moves(&self) -> &[LirMove] {
        &self.moves
    }

    pub fn push_move(&mut self, move_info: LirMove) {
        self.moves.push(move_info);
    }
}

#[derive(Clone, Debug)]
pub struct LirMove {
    from: LirAllocation,
    to: LirAllocation,
    ty: LirType,
}

impl LirMove {
    pub fn new(from: LirAllocation, to: LirAllocation, ty: LirType) -> Self {
        LirMove {
            from: from,
            to: to,
            ty: ty,
        }
    }

    pub fn from(&self) -> &LirAllocation {
        &self.from
    }

    pub fn to(&self) -> &LirAllocation {
        &self.to
    }

    pub fn ty(&self) -> LirType {
        self.ty
    }
}

#[derive(Clone, Debug)]
pub struct LirDefinition {
    virtual_reg: VirtualReg,
    ty: LirType,
    policy: LirDefinitionPolicy,
}

impl LirDefinition {
    pub fn new(virtual_reg: VirtualReg, ty: LirType, policy: LirDefinitionPolicy) -> Self {
        LirDefinition {
            virtual_reg: virtual_reg,
            ty: ty,
            policy: policy,
        }
    }

    pub fn virtual_reg(&self) -> VirtualReg {
        self.virtual_reg
    }

    pub fn physical_loc(&self) -> Option<PhysicalLoc> {
        self.policy.physical_loc()
    }

    pub fn ty(&self) -> LirType {
        self.ty
    }

    pub fn policy(&self) -> &LirDefinitionPolicy {
        &self.policy
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum LirDefinitionPolicy {
    Reg,
    ReuseInput(LirReuseInputDefinitionPolicy),
    Fixed(LirFixedDefinitionPolicy),
}

impl LirDefinitionPolicy {
    pub fn physical_loc(&self) -> Option<PhysicalLoc> {
        match self {
            LirDefinitionPolicy::Reg => None,
            LirDefinitionPolicy::ReuseInput(_) => None,
            LirDefinitionPolicy::Fixed(fixed_policy) => Some(fixed_policy.physical_loc()),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct LirReuseInputDefinitionPolicy {
    input: usize,
}

impl LirReuseInputDefinitionPolicy {
    pub fn new(input: usize) -> Self {
        LirReuseInputDefinitionPolicy { input: input }
    }

    pub fn input(&self) -> usize {
        self.input
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct LirFixedDefinitionPolicy {
    physical_loc: PhysicalLoc,
}

impl LirFixedDefinitionPolicy {
    pub fn new(physical_loc: PhysicalLoc) -> Self {
        LirFixedDefinitionPolicy {
            physical_loc: physical_loc,
        }
    }

    pub fn physical_loc(&self) -> PhysicalLoc {
        self.physical_loc
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum LirAllocation {
    Bogus,
    Constant,
    Static(LirStaticAllocation),
    Dynamic(LirDynamicAllocation),
}

impl LirAllocation {
    pub fn physical_loc(&self) -> Option<PhysicalLoc> {
        match self {
            LirAllocation::Bogus | LirAllocation::Constant | LirAllocation::Dynamic(_) => None,
            LirAllocation::Static(static_allocation) => Some(static_allocation.physical_loc()),
        }
    }

    pub fn use_info(&self) -> Option<&LirUseInfo> {
        match self {
            LirAllocation::Bogus | LirAllocation::Constant => None,
            LirAllocation::Static(static_allocation) => static_allocation.use_info(),
            LirAllocation::Dynamic(dynamic_allocation) => Some(dynamic_allocation.use_info()),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct LirStaticAllocation {
    physical_loc: PhysicalLoc,
    use_info: Option<LirUseInfo>,
}

impl LirStaticAllocation {
    pub fn new(physical_loc: PhysicalLoc, use_info: Option<LirUseInfo>) -> Self {
        LirStaticAllocation {
            physical_loc: physical_loc,
            use_info: use_info,
        }
    }

    pub fn physical_loc(&self) -> PhysicalLoc {
        self.physical_loc
    }

    pub fn use_info(&self) -> Option<&LirUseInfo> {
        self.use_info.as_ref()
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct LirDynamicAllocation {
    use_info: LirUseInfo,
}

impl LirDynamicAllocation {
    pub fn new(use_info: LirUseInfo) -> Self {
        LirDynamicAllocation { use_info: use_info }
    }

    pub fn use_info(&self) -> &LirUseInfo {
        &self.use_info
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct LirUseInfo {
    virtual_reg: VirtualReg,
    is_used_at_start: bool,
    policy: LirUsePolicy,
}

impl LirUseInfo {
    pub fn new(virtual_reg: VirtualReg, is_used_at_start: bool, policy: LirUsePolicy) -> Self {
        LirUseInfo {
            virtual_reg: virtual_reg,
            is_used_at_start: is_used_at_start,
            policy: policy,
        }
    }

    pub fn virtual_reg(&self) -> VirtualReg {
        self.virtual_reg
    }

    pub fn is_used_at_start(&self) -> bool {
        self.is_used_at_start
    }

    pub fn policy(&self) -> &LirUsePolicy {
        &self.policy
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum LirUsePolicy {
    Any(LirAnyUsePolicy),
    Reg,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct LirAnyUsePolicy {
    is_recovered_input: bool,
}

impl LirAnyUsePolicy {
    pub fn new(is_recovered_input: bool) -> Self {
        LirAnyUsePolicy {
            is_recovered_input: is_recovered_input,
        }
    }

    pub fn is_recovered_input(&self) -> bool {
        self.is_recovered_input
    }
}
