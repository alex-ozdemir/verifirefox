pub type ArgumentIndex = u32;
pub type PhysicalReg = u32;
pub type StackSlotIndex = u32;
pub type VirtualReg = u32;

pub type LirBlockId = u32;
pub type LirNodeId = u32;

// TODO: Static asserts on usize.

#[derive(Clone, Debug)]
pub struct LirGraph {
    blocks: Box<[Option<LirBlock>]>,
}

impl LirGraph {
    pub fn new(block_count: LirBlockId) -> Self {
        LirGraph {
            blocks: vec![None; block_count as usize].into_boxed_slice()
        }
    }

    pub fn iter_blocks(&self) -> impl Iterator<Item=&LirBlock> {
        self.blocks.iter().filter_map(|x| x.as_ref())
    }

    pub fn block(&self, block_id: LirBlockId) -> Option<&LirBlock> {
        if self.blocks.len() <= block_id as usize {
            None
        } else {
            self.blocks[block_id as usize].as_ref()
        }
    }

    pub fn put_block(&mut self, block: LirBlock) -> bool {
        if self.blocks.len() <= block.id as usize {
            // This block doesn't fit in the capacity we allocated.
            return false;
        }

        let block_entry = &mut self.blocks[block.id as usize];

        if block_entry.is_some() {
            // A block with this id has already been filled in.
            return false;
        }

        *block_entry = Some(block);
        true
    }
}

#[derive(Clone, Debug)]
pub struct LirBlock {
    id: LirBlockId,
    nodes: Vec<LirNode>,
}

impl LirBlock {
    pub fn new(id: LirBlockId, node_capacity: usize) -> Self {
        LirBlock {
            id: id,
            nodes: Vec::with_capacity(node_capacity),
        }
    }

    pub fn id(&self) -> LirBlockId { 
        self.id
    }

    pub fn nodes(&self) -> &[LirNode] {
        &self.nodes
    }

    pub fn push_node(&mut self, node: LirNode) {
        self.nodes.push(node);
    }
}

#[derive(Clone, Debug)]
pub struct LirNode {
    id: LirNodeId,
    operation: LirOperation,
    operands: Vec<LirAllocation>,
    defs: Vec<LirDefinition>,
    temps: Vec<LirDefinition>,
    successors: Vec<LirBlockId>,
}

impl LirNode {
    pub fn new(id: LirNodeId, operation: LirOperation, operand_capacity: usize, def_capacity: usize, temp_capacity: usize, successor_capacity: usize) -> Self {
        LirNode {
            id: id,
            operation: operation,
            operands: Vec::with_capacity(operand_capacity),
            defs: Vec::with_capacity(def_capacity),
            temps: Vec::with_capacity(temp_capacity),
            successors: Vec::with_capacity(successor_capacity),
        }
    }

    pub fn operands(&self) -> &[LirAllocation] {
        &self.operands
    }

    pub fn push_operand(&mut self, operand: LirAllocation) {
        self.operands.push(operand);
    }

    pub fn defs(&self) -> &[LirDefinition] {
        &self.defs
    }

    pub fn push_def(&mut self, def: LirDefinition) {
        self.defs.push(def);
    }

    pub fn temps(&self) -> &[LirDefinition] {
        &self.temps
    }

    pub fn push_temp(&mut self, temp: LirDefinition) {
        self.temps.push(temp);
    }

    pub fn successors(&self) -> &[LirBlockId] {
        &self.successors
    }

    pub fn push_successor(&mut self, successor: LirBlockId) {
        self.successors.push(successor);
    }
}

#[derive(Clone, Debug)]
pub enum LirOperation {
    LirMoveGroup(LirMoveGroup),
    Other,
}

#[derive(Clone, Debug)]
pub struct LirMoveGroup {
    moves: Vec<LirMove>,
}

impl LirMoveGroup {
    pub fn new(move_capacity: usize) -> Self {
        LirMoveGroup { moves: Vec::with_capacity(move_capacity) }
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
    ty: LirDefinitionType,
}

impl LirMove {
    pub fn new(from: LirAllocation, to: LirAllocation, ty: LirDefinitionType) -> Self {
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

    pub fn ty(&self) -> LirDefinitionType {
        self.ty
    }
}

#[derive(Clone, Debug)]
pub struct LirDefinition {
    virtual_reg: VirtualReg,
    ty: LirDefinitionType,
    policy: LirDefinitionPolicy,
    output: Option<LirAllocation>,
}

impl LirDefinition {
    pub fn new(virtual_reg: VirtualReg, ty: LirDefinitionType, policy: LirDefinitionPolicy, output: Option<LirAllocation>) -> Self {
        LirDefinition {
            virtual_reg: virtual_reg,
            ty: ty,
            policy: policy,
            output: output,
        }
    }

    pub fn virtual_reg(&self) -> VirtualReg {
        self.virtual_reg
    }

    pub fn ty(&self) -> LirDefinitionType {
        self.ty
    }

    pub fn policy(&self) -> LirDefinitionPolicy {
        self.policy
    }

    pub fn output(&self) -> Option<&LirAllocation> {
        self.output.as_ref()
    }
}

#[derive(Clone, Copy, Debug)]
#[repr(u8)]
pub enum LirDefinitionType {
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

#[derive(Clone, Copy, Debug)]
#[repr(u8)]
pub enum LirDefinitionPolicy {
    Fixed = 0,
    Register = 1,
    MustReuseInput = 2,
}

#[derive(Clone, Debug)]
pub enum LirAllocation {
    LirUse(LirUse),
    LirGeneralReg(PhysicalReg),
    LirFloatReg(PhysicalReg),
    LirStackSlot(StackSlotIndex),
    LirArgument(ArgumentIndex),
    Other,
}

#[derive(Clone, Debug)]
pub struct LirUse {
    policy: LirUsePolicy,
    virtual_reg: VirtualReg,
    physical_reg: Option<PhysicalReg>,
    used_at_start: bool,
}

impl LirUse {
    pub fn new(policy: LirUsePolicy, virtual_reg: VirtualReg, physical_reg: Option<PhysicalReg>, used_at_start: bool) -> Self {
        LirUse {
            policy: policy,
            virtual_reg: virtual_reg,
            physical_reg: physical_reg,
            used_at_start: used_at_start,
        }
    }

    pub fn policy(&self) -> LirUsePolicy {
        self.policy
    }

    pub fn virtual_reg(&self) -> VirtualReg {
        self.virtual_reg
    }

    pub fn physical_reg(&self) -> Option<PhysicalReg> {
        self.physical_reg
    }

    pub fn used_at_start(&self) -> bool {
        self.used_at_start
    }
}

#[derive(Clone, Copy, Debug)]
#[repr(u8)]
pub enum LirUsePolicy {
    Any = 0,
    UseRegister = 1,
    UseFixed = 2,
    KeepAlive = 3,
    RecoveredInput = 4,
}
