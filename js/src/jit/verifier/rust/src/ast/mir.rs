use typed_index_derive::TypedIndex;

pub type MirDefId = u32;

pub type MirGraph = Box<[MirBasicBlock]>;

#[derive(Clone, Copy, Default, Debug, Eq, Ord, PartialEq, PartialOrd, Hash, TypedIndex)]
#[typed_index(MirBasicBlock)]
pub struct MirBasicBlockIndex(pub usize);

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd, Hash, Default)]
pub struct MirBasicBlock {
    pub phis: Vec<MirPhi>,
    pub instructions: Vec<MirInstruction>,
    pub predecessors: Vec<MirBasicBlockIndex>,
    pub successors: Vec<MirBasicBlockIndex>,
}

impl MirBasicBlock {
    pub fn new(
        phi_capacity: usize,
        instruction_capacity: usize,
        predecessor_capacity: usize,
        successor_capacity: usize,
    ) -> Self {
        Self {
            phis: Vec::with_capacity(phi_capacity),
            instructions: Vec::with_capacity(instruction_capacity),
            predecessors: Vec::with_capacity(predecessor_capacity),
            successors: Vec::with_capacity(successor_capacity),
        }
    }

    pub fn push_phi(&mut self, phi: MirPhi) {
        self.phis.push(phi);
    }
    pub fn push_instruction(&mut self, instruction: MirInstruction) {
        self.instructions.push(instruction);
    }
    pub fn push_predecessor(&mut self, predecessor: MirBasicBlockIndex) {
        self.predecessors.push(predecessor);
    }
    pub fn push_successor(&mut self, successor: MirBasicBlockIndex) {
        self.successors.push(successor);
    }
    pub fn get_instr(&self, is_phi: bool, idx: u32) -> Option<Result<&MirPhi, &MirInstruction>> {
        if is_phi {
            self.phis.get(idx as usize).map(Result::Ok)
        } else {
            self.instructions.get(idx as usize).map(Result::Err)
        }
    }
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd, Hash)]
pub struct MirPhi {
    pub inputs: Vec<MirDefId>,
    pub output: MirDefId,
}

impl MirPhi {
    pub fn new(n_inputs: usize, output: MirDefId) -> Self {
        Self {
            inputs: Vec::with_capacity(n_inputs),
            output,
        }
    }

    pub fn push_input(&mut self, def_id: MirDefId) {
        self.inputs.push(def_id);
    }
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd, Hash)]
pub struct MirInstruction {
    pub operation: MirOperation,
    pub inputs: Vec<MirDefId>,
    pub output: MirDefId,
}

impl MirInstruction {
    pub fn new(operation: MirOperation, n_inputs: usize, output: MirDefId) -> Self {
        Self {
            operation,
            inputs: Vec::with_capacity(n_inputs),
            output,
        }
    }

    pub fn push_input(&mut self, def_id: MirDefId) {
        self.inputs.push(def_id);
    }
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd, Hash)]
pub enum MirOperation {
    /// Not yet marshalled properly
    Other,
}
