use std::collections::HashMap;
use std::fmt;

use anyhow::{anyhow, bail, ensure, Context, Error};
use typed_index_derive::TypedIndex;

use crate::ast::lir::{
    LirAllocation, LirDefinition, LirDefinitionPolicy, LirGraph, LirMove, LirMoveGroup, LirNode,
    LirNodeIndex, LirOperation, LirType, LirUsePolicy, PhysicalLoc, VirtualReg,
};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Possibility<T> {
    Known(T),
    Unknown,
    Invalid,
}

#[derive(Clone, Copy, Default, Eq, Ord, PartialEq, PartialOrd, Hash, TypedIndex)]
#[typed_index(RegAllocNode)]
pub struct RegAllocNodeIndex(usize);

impl fmt::Debug for RegAllocNodeIndex {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        fmt::Debug::fmt(&self.0, f)
    }
}

impl fmt::Display for RegAllocNodeIndex {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        fmt::Display::fmt(&self.0, f)
    }
}

impl From<LirNodeIndex> for RegAllocNodeIndex {
    fn from(lir_node_index: LirNodeIndex) -> Self {
        RegAllocNodeIndex(lir_node_index.into())
    }
}

impl From<&LirNodeIndex> for RegAllocNodeIndex {
    fn from(lir_node_index: &LirNodeIndex) -> Self {
        RegAllocNodeIndex::from(*lir_node_index)
    }
}

fn check_ty_constraint(ty: LirType, physical_loc: PhysicalLoc) -> Result<(), Error> {
    let is_consistent_with_type = match physical_loc {
        PhysicalLoc::Reg(physical_reg) => match ty {
            LirType::General
            | LirType::Int32
            | LirType::Object
            | LirType::Slots
            | LirType::Type
            | LirType::Payload
            | LirType::Box => physical_reg.is_general(),

            LirType::Float32 | LirType::Double | LirType::Simd128Int | LirType::Simd128Float => {
                physical_reg.is_float()
            }
        },
        _ => true,
    };

    ensure!(
        is_consistent_with_type,
        "Type constraint violated: {:?} vs {:?}",
        ty,
        physical_loc
    );

    Ok(())
}

#[derive(Clone, Debug)]
pub struct RegAllocMapping {
    pub virtual_reg: VirtualReg,
    pub physical_loc: PhysicalLoc,
}

impl RegAllocMapping {
    fn from_lir_allocations(
        before_allocation: &LirAllocation,
        after_allocation: &LirAllocation,
    ) -> Result<Option<Self>, Error> {
        let after_physical_loc = match after_allocation.physical_loc() {
            Some(after_physical_loc) => after_physical_loc,
            None => {
                return match (before_allocation, after_allocation) {
                    (LirAllocation::Bogus, LirAllocation::Bogus) |
                    (LirAllocation::Constant, LirAllocation::Constant) => Ok(None),
                    _ => Err(anyhow!("Allocation is missing a required physical location after register allocation")),
                };
            }
        };

        let before_use_info = before_allocation.use_info().ok_or_else(|| {
            anyhow!("Allocation is missing required use info before register allocation")
        })?;

        ensure!(
            after_allocation.use_info().is_none(),
            "Allocation has use info left over after register allocation"
        );

        if let Some(before_physical_loc) = before_allocation.physical_loc() {
            ensure!(before_physical_loc == after_physical_loc, "Allocation with a physical location before register allocation does not match after");
        }

        let is_consistent_with_before_use_policy = match before_use_info.policy() {
            LirUsePolicy::Any(_) => true,
            LirUsePolicy::Reg => match after_physical_loc {
                PhysicalLoc::Reg(_) => true,
                _ => false,
            },
        };
        ensure!(
            is_consistent_with_before_use_policy,
            "Allocation use policy violated: {:?} vs {:?}",
            before_use_info.policy(),
            after_physical_loc
        );

        Ok(Some(RegAllocMapping {
            virtual_reg: before_use_info.virtual_reg(),
            physical_loc: after_physical_loc,
        }))
    }

    fn from_lir_definitions(
        maybe_before_definition: &Option<LirDefinition>,
        maybe_after_definition: &Option<LirDefinition>,
    ) -> Result<Option<Self>, Error> {
        let (before_definition, after_definition) =
            match (maybe_before_definition, maybe_after_definition) {
                (Some(before_definition), Some(after_definition)) => {
                    (before_definition, after_definition)
                }
                (None, None) => return Ok(None),
                _ => bail!(
                    "Definition is bogus before register allocation but not after, or vice-versa"
                ),
            };

        ensure!(before_definition.virtual_reg() == after_definition.virtual_reg(), "Definition has different virtual registers before and after register allocation: {} vs {}", before_definition.virtual_reg(), after_definition.virtual_reg());

        ensure!(
            before_definition.ty() == after_definition.ty(),
            "Definition has different types before and after register allocation: {:?} vs {:?}",
            before_definition.ty(),
            after_definition.ty()
        );

        let after_physical_loc = after_definition.physical_loc().ok_or_else(|| {
            anyhow!("Definition has no physical location even after register allocation")
        })?;

        let is_consistent_with_before_policy = match before_definition.policy() {
            LirDefinitionPolicy::Reg => match after_physical_loc {
                PhysicalLoc::Reg(_) => true,
                _ => false,
            },
            LirDefinitionPolicy::ReuseInput(_) => true,
            LirDefinitionPolicy::Fixed(fixed_definition_policy) => {
                fixed_definition_policy.physical_loc() == after_physical_loc
            }
        };
        ensure!(
            is_consistent_with_before_policy,
            "Definition policy violated: {:?} vs {:?}",
            before_definition.policy(),
            after_physical_loc
        );

        check_ty_constraint(after_definition.ty(), after_physical_loc)?;

        Ok(Some(RegAllocMapping {
            virtual_reg: before_definition.virtual_reg(),
            physical_loc: after_physical_loc,
        }))
    }
}

#[derive(Clone, Debug)]
pub struct RegAllocGraph {
    pub nodes: Box<[RegAllocNode]>,
    queue_head_index: Option<RegAllocNodeIndex>,
    queue_tail_index: Option<RegAllocNodeIndex>,
}

impl RegAllocGraph {
    pub fn from_lir(before_graph: &LirGraph, after_graph: &LirGraph) -> Result<Self, Error> {
        let nodes = after_graph
            .iter()
            .enumerate()
            .map(|(raw_node_index, after_node)| {
                let maybe_before_node = LirNodeIndex::from(raw_node_index).get(before_graph);
                RegAllocNode::from_lir(maybe_before_node, after_node)
                    .with_context(|| format!("Invalid data for LIR node {}", raw_node_index))
            })
            .collect::<Result<Box<_>, _>>()?;

        Ok(RegAllocGraph {
            nodes,
            queue_head_index: None,
            queue_tail_index: None,
        })
    }

    pub fn queue_push(&mut self, node_index: RegAllocNodeIndex) {
        let node = &mut self.nodes[node_index];
        if node.is_in_queue {
            return;
        }
        node.is_in_queue = true;

        match self.queue_tail_index {
            Some(queue_tail_index) => {
                self.nodes[queue_tail_index].queue_next_index = Some(node_index);
                self.queue_tail_index = Some(node_index);
            }
            None => {
                self.queue_head_index = Some(node_index);
                self.queue_tail_index = Some(node_index);
            }
        }
    }

    pub fn queue_pop(&mut self) -> Option<RegAllocNodeIndex> {
        let queue_head_index = self.queue_head_index?;
        if self.queue_tail_index == Some(queue_head_index) {
            self.queue_tail_index = None;
        }

        let queue_head = &mut self.nodes[queue_head_index];
        queue_head.is_in_queue = false;

        self.queue_head_index = queue_head.queue_next_index;
        queue_head.queue_next_index = None;

        Some(queue_head_index)
    }
}

#[derive(Clone, Debug)]
pub struct RegAllocNode {
    pub operation: RegAllocOperation,
    pub predecessors: RegAllocPredecessors,
    pub successors: Box<[RegAllocNodeIndex]>,
    pub state: Option<RegAllocState>,
    pub has_complete_state: bool,
    is_in_queue: bool,
    queue_next_index: Option<RegAllocNodeIndex>,
}

impl RegAllocNode {
    fn from_lir(maybe_before_node: Option<&LirNode>, after_node: &LirNode) -> Result<Self, Error> {
        let out_operation = RegAllocOperation::from_lir(maybe_before_node, after_node)?;
        let out_predecessors = RegAllocPredecessors::from_lir(after_node)?;

        Ok(RegAllocNode {
            operation: out_operation,
            predecessors: out_predecessors,
            successors: after_node
                .successors()
                .iter()
                .map(RegAllocNodeIndex::from)
                .collect(),
            state: None,
            has_complete_state: false,
            is_in_queue: false,
            queue_next_index: None,
        })
    }
}

// TODO: Can we use a more efficient data structure than a HashMap?
pub type RegAllocState = HashMap<PhysicalLoc, Box<[Possibility<VirtualReg>]>>;

#[derive(Clone, Debug)]
pub enum RegAllocPredecessors {
    AtBlockStart(Box<[RegAllocNodeIndex]>),
    InBlockBody(RegAllocNodeIndex),
}

impl RegAllocPredecessors {
    fn from_lir(node: &LirNode) -> Result<Self, Error> {
        if node.is_at_block_start() {
            Ok(RegAllocPredecessors::AtBlockStart(
                node.predecessors()
                    .iter()
                    .map(RegAllocNodeIndex::from)
                    .collect(),
            ))
        } else {
            match node.predecessors() {
                [predecessor_node_index] => Ok(RegAllocPredecessors::InBlockBody(
                    predecessor_node_index.into(),
                )),
                _ => Err(anyhow!("Node in block body has multiple predecessors")),
            }
        }
    }
}

#[derive(Clone, Debug)]
pub enum RegAllocOperation {
    MoveGroup(RegAllocMoveGroup),
    Phi(RegAllocPhi),
    Instruction(RegAllocInstruction),
}

impl RegAllocOperation {
    fn from_lir(maybe_before_node: Option<&LirNode>, after_node: &LirNode) -> Result<Self, Error> {
        match after_node.operation() {
            LirOperation::MoveGroup(move_group) => {
                ensure!(
                    maybe_before_node.is_none(),
                    "Found MoveGroup node present before register allocation"
                );

                let out_move_group = RegAllocMoveGroup::from_lir(after_node, move_group)?;
                Ok(RegAllocOperation::MoveGroup(out_move_group))
            }
            LirOperation::Phi => {
                let before_node = maybe_before_node.ok_or_else(|| {
                    anyhow!("Found Phi node not present before register allocation")
                })?;

                match before_node.operation() {
                    LirOperation::Phi => (),
                    operation => bail!(
                        "Phi node was a different operation before register allocation: {:?}",
                        operation
                    ),
                }

                let out_phi = RegAllocPhi::from_lir(before_node, after_node)?;
                Ok(RegAllocOperation::Phi(out_phi))
            }
            _ => {
                let before_node = maybe_before_node.ok_or_else(|| {
                    anyhow!("Found Other node not present before register allocation")
                })?;

                match before_node.operation() {
                    LirOperation::Other(..) => (),
                    operation => bail!(
                        "Other node was a different operation before register allocation: {:?}",
                        operation
                    ),
                }

                let out_instruction = RegAllocInstruction::from_lir(before_node, after_node)?;
                Ok(RegAllocOperation::Instruction(out_instruction))
            }
        }
    }

    pub fn judge(&self, state: &RegAllocState) -> Possibility<()> {
        match self {
            RegAllocOperation::MoveGroup(_) => Possibility::Known(()),
            RegAllocOperation::Phi(phi) => phi.judge(state),
            RegAllocOperation::Instruction(instruction) => instruction.judge(state),
        }
    }

    pub fn transfer(&self, state: &mut RegAllocState) {
        match self {
            RegAllocOperation::MoveGroup(move_group) => move_group.transfer(state),
            RegAllocOperation::Phi(phi) => phi.transfer(state),
            RegAllocOperation::Instruction(instruction) => instruction.transfer(state),
        }
    }
}

#[derive(Clone, Debug)]
pub struct RegAllocMoveGroup {
    pub moves: Box<[RegAllocMove]>,
}

impl RegAllocMoveGroup {
    fn from_lir(after_node: &LirNode, move_group: &LirMoveGroup) -> Result<Self, Error> {
        ensure!(
            after_node.operands().len() == 0,
            "MoveGroup has operand count {} != 0",
            after_node.operands().len()
        );
        ensure!(
            after_node.defs().len() == 0,
            "MoveGroup has def count {} != 0",
            after_node.defs().len()
        );
        ensure!(
            after_node.temps().len() == 0,
            "MoveGroup has temp count {} != 0",
            after_node.temps().len()
        );
        ensure!(
            after_node.successors().len() == 1,
            "MoveGroup has successor count {} != 1",
            after_node.successors().len()
        );

        let out_moves = move_group
            .moves()
            .iter()
            .enumerate()
            .map(|(move_index, move_info)| {
                RegAllocMove::from_lir(move_info)
                    .with_context(|| format!("Invalid data for move {}", move_index))
            })
            .collect::<Result<Box<[_]>, _>>()?;

        Ok(RegAllocMoveGroup { moves: out_moves })
    }

    pub fn transfer(&self, state: &mut RegAllocState) {
        let old_state = state.clone();
        for move_info in self.moves.iter() {
            move_info.transfer(state, &old_state);
        }
    }
}

#[derive(Clone, Debug)]
pub struct RegAllocMove {
    pub from: PhysicalLoc,
    pub to: PhysicalLoc,
}

impl RegAllocMove {
    fn from_lir(move_info: &LirMove) -> Result<Self, Error> {
        fn handle_move_allocation(
            ty: LirType,
            lir_allocation: &LirAllocation,
        ) -> Result<PhysicalLoc, Error> {
            let physical_loc = lir_allocation
                .physical_loc()
                .ok_or_else(|| anyhow!("Move allocation is missing a physical location"))?;

            ensure!(
                lir_allocation.use_info().is_none(),
                "Found move allocation with use info"
            );

            check_ty_constraint(ty, physical_loc)?;
            Ok(physical_loc)
        }

        let out_from = handle_move_allocation(move_info.ty(), move_info.from())
            .context("Invalid data for move from-allocation")?;
        let out_to = handle_move_allocation(move_info.ty(), move_info.to())
            .context("Invalid data for move to-allocation")?;

        Ok(RegAllocMove {
            from: out_from,
            to: out_to,
        })
    }

    pub fn transfer(&self, state: &mut RegAllocState, old_state: &RegAllocState) {
        match old_state.get(&self.from) {
            Some(virtual_regs) => {
                state.insert(self.to, virtual_regs.clone());
            }
            None => {
                state.remove(&self.to);
            }
        }
    }
}

fn check_before_after_node_consistency(
    before_node: &LirNode,
    after_node: &LirNode,
) -> Result<(), Error> {
    ensure!(
        before_node.operands().len() == after_node.operands().len(),
        "Node has different operand counts before and after register allocation: {} vs {}",
        before_node.operands().len(),
        after_node.operands().len()
    );

    ensure!(
        before_node.defs().len() == after_node.defs().len(),
        "Node has different def counts before and after register allocation: {} vs {}",
        before_node.defs().len(),
        after_node.defs().len()
    );

    ensure!(
        before_node.temps().len() == after_node.temps().len(),
        "Node has different temp counts before and after register allocation: {} vs {}",
        before_node.temps().len(),
        after_node.temps().len()
    );

    // Temporarily disabled - this can legitimately occur due to the insertion of MoveGroups.
    // ensure!(before_node.predecessors() == after_node.predecessors(), "Node has different predecessors before and after register allocation.");

    // Temporarily disabled - this can legitimately occur due to the insertion of MoveGroups.
    // ensure!(before_node.successors() == after_node.successors(), "Node has different successors before and after register allocation");

    Ok(())
}

#[derive(Clone, Debug)]
pub struct RegAllocPhi {
    pub inputs: Box<[VirtualReg]>,
    pub output: RegAllocMapping,
}

impl RegAllocPhi {
    fn from_lir(before_node: &LirNode, after_node: &LirNode) -> Result<Self, Error> {
        check_before_after_node_consistency(before_node, after_node)?;

        ensure!(
            after_node.defs().len() == 1,
            "Phi node has def count {} != 1",
            after_node.defs().len()
        );
        ensure!(
            after_node.temps().len() == 0,
            "Phi node has temp count {} != 0",
            after_node.temps().len()
        );
        ensure!(
            after_node.successors().len() == 1,
            "Phi node has successor count {} != 1",
            after_node.successors().len()
        );

        let before_output = &before_node.defs()[0];
        let after_output = &after_node.defs()[0];

        match before_output.as_ref().map(LirDefinition::policy) {
            None => (),
            Some(LirDefinitionPolicy::ReuseInput(reuse_input_definition_policy))
                if reuse_input_definition_policy.input() < before_node.operands().len() =>
            {
                ()
            }
            Some(definition_policy) => bail!(
                "Phi output has invalid definition policy: {:?}",
                definition_policy
            ),
        }

        let out_output = RegAllocMapping::from_lir_definitions(before_output, after_output)
            .context("Invalid data for Phi output")?
            .ok_or_else(|| anyhow!("Phi has bogus output"))?;

        let before_inputs = before_node.operands().iter();
        let after_inputs = after_node.operands().iter();

        fn handle_input(
            before_input: &LirAllocation,
            after_input: &LirAllocation,
            output_physical_loc: PhysicalLoc,
        ) -> Result<VirtualReg, Error> {
            if let Some(before_use_info) = before_input.use_info() {
                ensure!(
                    !before_use_info.is_used_at_start(),
                    "Phi input has invalid use info"
                );

                match before_use_info.policy() {
                    LirUsePolicy::Any(before_any_use_policy) => {
                        ensure!(
                            !before_any_use_policy.is_recovered_input(),
                            "Phi input has invalid use info"
                        );
                    }
                    LirUsePolicy::Reg => (),
                }
            }

            let input_mapping = RegAllocMapping::from_lir_allocations(before_input, after_input)?
                .ok_or_else(|| anyhow!("Phi inout is not backed by storage"))?;

            ensure!(input_mapping.physical_loc == output_physical_loc, "Phi input allocation does not correspond to the output allocation's physical location");

            Ok(input_mapping.virtual_reg)
        }

        let out_inputs = before_inputs
            .zip(after_inputs)
            .enumerate()
            .map(|(input_index, (before_input, after_input))| {
                handle_input(before_input, after_input, out_output.physical_loc)
                    .with_context(|| format!("Invalid data for Phi input {}", input_index))
            })
            .collect::<Result<Box<[_]>, _>>()?;

        Ok(RegAllocPhi {
            inputs: out_inputs,
            output: out_output,
        })
    }

    pub fn judge(&self, state: &RegAllocState) -> Possibility<()> {
        let virtual_inputs = &self.inputs;
        let physical_output = self.output.physical_loc;

        let virtual_regs_at_output = match state.get(&physical_output) {
            Some(virtual_regs_at_output) => virtual_regs_at_output,
            None => return Possibility::Unknown,
        };

        if virtual_regs_at_output.len() != virtual_inputs.len() {
            return Possibility::Invalid;
        }

        let input_pairs = virtual_inputs.iter().zip(virtual_regs_at_output.iter());
        for (virtual_input, possibly_virtual_reg_at_output) in input_pairs {
            let virtual_reg_at_output = match possibly_virtual_reg_at_output {
                Possibility::Known(virtual_reg_at_output) => virtual_reg_at_output,
                Possibility::Unknown => return Possibility::Unknown,
                Possibility::Invalid => return Possibility::Invalid,
            };

            if virtual_input != virtual_reg_at_output {
                return Possibility::Invalid;
            }
        }

        Possibility::Known(())
    }

    pub fn transfer(&self, state: &mut RegAllocState) {
        let virtual_output = self.output.virtual_reg;
        let physical_output = self.output.physical_loc;
        state.insert(
            physical_output,
            vec![Possibility::Known(virtual_output)].into_boxed_slice(),
        );
    }
}

#[derive(Clone, Debug)]
pub struct RegAllocInstruction {
    pub operands: Box<[RegAllocMapping]>,
    pub defs: Box<[RegAllocMapping]>,
    pub temps: Box<[RegAllocMapping]>,
}

impl RegAllocInstruction {
    fn from_lir(before_node: &LirNode, after_node: &LirNode) -> Result<Self, Error> {
        check_before_after_node_consistency(before_node, after_node)?;

        fn flip_result_of_option<T, E>(result: Result<Option<T>, E>) -> Option<Result<T, E>> {
            match result {
                Ok(Some(value)) => Some(Ok(value)),
                Ok(None) => None,
                Err(err) => Some(Err(err)),
            }
        }

        let before_operands = before_node.operands().iter();
        let after_operands = after_node.operands().iter();

        fn handle_operand(
            before_operand: &LirAllocation,
            after_operand: &LirAllocation,
        ) -> Result<Option<RegAllocMapping>, Error> {
            if let Some(before_use_info) = before_operand.use_info() {
                match before_use_info.policy() {
                    LirUsePolicy::Any(before_any_use_policy) => {
                        ensure!(
                            !before_any_use_policy.is_recovered_input(),
                            "Instruction operand has invalid use info"
                        );
                    }
                    LirUsePolicy::Reg => (),
                }
            }

            RegAllocMapping::from_lir_allocations(before_operand, after_operand)
        }

        let out_operands = before_operands
            .zip(after_operands)
            .enumerate()
            .map(|(operand_index, (before_operand, after_operand))| {
                handle_operand(before_operand, after_operand)
                    .with_context(|| format!("Invalid data for operand {}", operand_index))
            })
            .filter_map(flip_result_of_option)
            .collect::<Result<Box<[_]>, _>>()?;

        let before_defs = before_node.defs().iter();
        let after_defs = after_node.defs().iter();

        let out_defs = before_defs
            .zip(after_defs)
            .enumerate()
            .map(|(def_index, (before_def, after_def))| {
                RegAllocMapping::from_lir_definitions(before_def, after_def)
                    .with_context(|| format!("Invalid data for def {}", def_index))
            })
            .filter_map(flip_result_of_option)
            .collect::<Result<Box<[_]>, _>>()?;

        let before_temps = before_node.temps().iter();
        let after_temps = after_node.temps().iter();

        let out_temps = before_temps
            .zip(after_temps)
            .enumerate()
            .map(|(temp_index, (before_temp, after_temp))| {
                RegAllocMapping::from_lir_definitions(before_temp, after_temp)
                    .with_context(|| format!("Invalid data for temp {}", temp_index))
            })
            .filter_map(flip_result_of_option)
            .collect::<Result<Box<[_]>, _>>()?;

        Ok(RegAllocInstruction {
            operands: out_operands,
            defs: out_defs,
            temps: out_temps,
        })
    }

    pub fn judge(&self, state: &RegAllocState) -> Possibility<()> {
        for operand in self.operands.iter() {
            let virtual_regs_at_operand = match state.get(&operand.physical_loc) {
                Some(virtual_regs_at_operand) => virtual_regs_at_operand,
                None => return Possibility::Unknown,
            };

            for possibly_virtual_reg_at_operand in virtual_regs_at_operand.iter() {
                let virtual_reg_at_operand = match possibly_virtual_reg_at_operand {
                    Possibility::Known(virtual_reg_at_operand) => virtual_reg_at_operand,
                    Possibility::Unknown => return Possibility::Unknown,
                    Possibility::Invalid => return Possibility::Invalid,
                };

                if operand.virtual_reg != *virtual_reg_at_operand {
                    return Possibility::Invalid;
                }
            }
        }

        Possibility::Known(())
    }

    pub fn transfer(&self, state: &mut RegAllocState) {
        // Make sure to apply temps before defs here! They can overlap, so order
        // is important.
        for def_or_temp in self.temps.iter().chain(self.defs.iter()) {
            state.insert(
                def_or_temp.physical_loc,
                vec![Possibility::Known(def_or_temp.virtual_reg)].into_boxed_slice(),
            );
        }
    }
}
