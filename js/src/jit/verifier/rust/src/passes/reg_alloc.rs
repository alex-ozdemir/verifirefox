// TODO: Verify allocation policies beyond internal consistency.
// TODO: Verify allocation used-at-start property.
// TODO: Verify allocation recovered-input property.
// TODO: Verify definition must-reuse-input property.
// TODO: Verify stack integrity?
// TODO: Account for inserted nodes in successors equivalence check.

use std::collections::{HashMap, VecDeque};
use std::error;
use std::sync::Arc;

use anyhow::Error;
use thiserror::Error;

use crate::ast::lir::{LirAllocation, LirDefinition, LirDefinitionPolicy, LirGraph, LirMove, LirMoveGroup, LirNodeId, LirNode, LirOperation, LirType, LirUsePolicy, PhysicalLoc, VirtualReg};
use crate::bag::Bag;

use crate::passes::base::Pass;

#[derive(Clone, Debug, Error)]
#[error("error at LIR node {node_id}")]
pub struct LirNodeError<Source> where Source: 'static + error::Error {
    node_id: LirNodeId,
    source: Source,
}

#[derive(Clone, Debug)]
pub struct RegAllocPass {
    before_graph: Arc<LirGraph>,
    after_graph: Arc<LirGraph>,
}

impl RegAllocPass {
    pub fn new(before_graph: Arc<LirGraph>, after_graph: Arc<LirGraph>) -> Self {
        RegAllocPass { before_graph, after_graph }
    }
}

impl Pass for RegAllocPass {
    fn run(&self) -> Result<(), Error> {
        let mut graph = reg_alloc_graph_from_lir(&*self.before_graph, &*self.after_graph)?;

        let dirty = graph.iter().map(|(node_id, _)| node_id).collect::<VecDeque<_>>();
        find_fixpoint(&mut graph, dirty, false)?;

        let dirty = graph.iter().filter_map(|(node_id, node)| {
            match node.predecessors {
                RegAllocPredecessors::AtBlockStart(_) => Some(node_id),
                RegAllocPredecessors::InBlockBody(_) => None,
            }
        }).collect::<VecDeque<_>>();
        find_fixpoint(&mut graph, dirty, true)?;

        check_graph_completeness(&graph)?;
        Ok(())
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum Possibility<T> {
    Known(T),
    Unknown,
    Invalid,
}

// TODO: Can we use a more efficient data structure than a HashMap?
type RegAllocState = HashMap<PhysicalLoc, Box<[Possibility<VirtualReg>]>>;

#[derive(Clone, Debug, Error)]
#[error("found inconsistency with state {state:?}")]
pub struct RegAllocFixpointError {
    state: RegAllocState,
}

fn find_fixpoint(graph: &mut RegAllocGraph, mut dirty: VecDeque<LirNodeId>, strict_meet: bool) -> Result<(), LirNodeError<RegAllocFixpointError>> {
    let prefix = format!("{:p}", graph);
    let graph_size = graph.capacity();

    eprintln!("{} find_fixpoint on {}", prefix, graph_size);
    let mut iterations = 0;

    loop {
        let node_id = match dirty.pop_front() {
            Some(item) => item,
            None => break,
        };

        iterations += 1;
        if iterations % 100000 == 0 {
            eprintln!("{} still going on {} at {}; queue = {} : {:?}", prefix, graph_size, iterations, dirty.len(), dirty);
        }

        let mut state = meet(graph, &graph.item(node_id).unwrap().predecessors, strict_meet);

        let node = graph.item_mut(node_id).unwrap();

        if iterations % 100000 == 0 {
            //eprintln!("{} before {} {:?}", prefix, node_id, node);
        }

        let state_is_complete = match node.operation.judge(&state) {
            Possibility::Known(_) => true,
            Possibility::Unknown => false,
            Possibility::Invalid => {
                return Err(LirNodeError { node_id: node_id, source: RegAllocFixpointError { state: state.clone() } });
            }
        };

        node.operation.transfer(&mut state);

        if state != node.state {
            //dirty.extend(node.successors.iter());
            for successor in node.successors.iter() {
                if !dirty.contains(successor) {
                    dirty.push_back(*successor);
                }
            }
            node.state = state;
        }

        node.has_complete_state = state_is_complete;

        if iterations % 100000 == 0 {
            //eprintln!("{} after {} {:?}", prefix, node_id, node);
        }

        if iterations > 1000000 {
            //eprintln!("{} {:?} {} {:?}", prefix, dirty, node_id, node);
        }
    }

    eprintln!("{} end {} on {}", prefix, iterations, graph_size);
    Ok(())
}

#[derive(Clone, Debug, Error)]
#[error("found incomplete fixpoint state {state:?}, graph: {graph:#?}")]
pub struct RegAllocCompletenessError {
    state: RegAllocState,
    graph: RegAllocGraph,
}

fn check_graph_completeness(graph: &RegAllocGraph) -> Result<(), LirNodeError<RegAllocCompletenessError>> {
    for (node_id, node) in graph.iter() {
        if !node.has_complete_state {
            let state = meet(graph, &node.predecessors, true);

            return Err(LirNodeError {
                node_id,
                source: RegAllocCompletenessError { state, graph: graph.clone() }
            });
        }
    }

    Ok(())
}

fn meet(graph: &RegAllocGraph, predecessors: &RegAllocPredecessors, strict: bool) -> RegAllocState {
    match predecessors {
        RegAllocPredecessors::AtBlockStart(all_predecessors) => {
            let mut state = HashMap::new();

            for (predecessor_index, predecessor_node_id) in all_predecessors.iter().enumerate() {
                let predecessor_node = graph.item(*predecessor_node_id).unwrap();
                for (physical_loc, virtual_regs) in predecessor_node.state.iter() {
                    let mut out_possibly_virtual_reg = Possibility::Unknown;

                    for possibly_virtual_reg in virtual_regs.iter() {
                        let virtual_reg = match possibly_virtual_reg {
                            Possibility::Known(virtual_reg) => *virtual_reg,
                            Possibility::Unknown => {
                                if strict {
                                    out_possibly_virtual_reg = Possibility::Unknown;
                                    break;
                                } else {
                                    continue;
                                }
                            }
                            Possibility::Invalid => {
                                out_possibly_virtual_reg = Possibility::Invalid;
                                break;
                            }
                        };

                        match out_possibly_virtual_reg {
                            Possibility::Known(out_virtual_reg) => {
                                if out_virtual_reg != virtual_reg {
                                    out_possibly_virtual_reg = Possibility::Invalid;
                                    break;
                                }
                            }
                            Possibility::Unknown => {
                                out_possibly_virtual_reg = Possibility::Known(virtual_reg);
                            }
                            Possibility::Invalid => break,
                        }
                    }

                    if let Possibility::Unknown = out_possibly_virtual_reg {
                        continue;
                    }

                    let out_virtual_regs = state.entry(*physical_loc).or_insert_with(|| vec![Possibility::Unknown; all_predecessors.len()].into_boxed_slice());
                    out_virtual_regs[predecessor_index] = out_possibly_virtual_reg;
                }
            }

            state
        },
        RegAllocPredecessors::InBlockBody(predecessor_node_id) => {
            let predecessor_node = graph.item(*predecessor_node_id).unwrap();
            predecessor_node.state.clone()
        },
    }
}

#[derive(Clone, Debug, Error)]
pub enum RegAllocInputError {
    #[error("found a violation of a type constraint")]
    TypeConstraintViolated,
    #[error("found a violation of an allocation's use policy")]
    AllocationUsePolicyViolated,
    #[error("found allocation which is missing a required physical location after register allocation")]
    AfterAllocationMissingPhysicalLoc,
    #[error("found allocation which is missing required use info before register allocation")]
    BeforeAllocationMissingUseInfo,
    #[error("found allocation which has use info left over after register allocation")]
    AfterAllocationHasUseInfo,
    #[error("found allocation with a physical location before register allocation which does not match after")]
    BeforeAllocationPhysicalLocDoesNotMatchAfter,
    #[error("found definition with different virtual registers before and after allocation")]
    BeforeDefinitionVirtualRegDoesNotMatchAfter,
    #[error("found definition with different types before and after allocation")]
    BeforeDefinitionTypeDoesNotMatchAfter,
    #[error("found definition which has no physical location even after register allocation")]
    AfterDefinitionHasNoPhysicalLoc,
    #[error("found a violation of a definition's policy")]
    DefinitionPolicyViolated,
    #[error("node in block body has multiple predecessors")]
    NodeInBlockBodyHasMultiplePredecessors,
    #[error("found move group node present before register allocation")]
    MoveGroupHasBeforeNode,
    #[error("found phi node not present before register allocation")]
    PhiHasNoBeforeNode,
    #[error("found phi node which was a different operation before register allocation")]
    PhiDoesNotMatchBeforeOperation,
    #[error("found instruction node not present before register allocation")]
    InstructionHasNoBeforeNode,
    #[error("move group node has operands")]
    MoveGroupHasOperands,
    #[error("move group node has defs")]
    MoveGroupHasDefs,
    #[error("move group node has temps")]
    MoveGroupHasTemps,
    #[error("move group node has a successor count != 1")]
    MoveGroupHasInvalidSuccessorCount,
    #[error("move allocation has no physical loc")]
    MoveAllocationHasNoPhysicalLoc,
    #[error("found move allocation with use info")]
    MoveAllocationHasUseInfo,
    #[error("found a node with different operand counts before and after register allocation")]
    OperandCountsDoNotMatch,
    #[error("found a node with different def counts before and after register allocation")]
    DefCountsDoNotMatch,
    #[error("found a node with different temp counts before and after register allocation")]
    TempCountsDoNotMatch,
    #[error("found a node with different successors before and after register allocation")]
    SuccessorsDoNotMatch,
    #[error("phi node has a def count != 1")]
    PhiHasInvalidDefCount,
    #[error("phi node has a temp count != 0")]
    PhiHasInvalidTempCount,
    #[error("phi node has a successor count != 1")]
    PhiHasInvalidSuccessorCount,
    #[error("found phi output with invalid policy")]
    PhiOutputHasInvalidPolicy,
    #[error("found bogus phi output")]
    PhiOutputIsBogus,
    #[error("found phi input with invalid use info")]
    PhiInputHasInvalidUseInfo,
    #[error("found phi input which is not backed by storage")]
    PhiInputIsNotBackedByStorage,
    #[error("found phi input which is not allocated to the phi's physical output")]
    PhiInputIsNotAllocatedToOutput,
    #[error("found instruction operand with invalid use info")]
    InstructionOperandHasInvalidUseInfo,
    #[error("found definition in instruction which is bogus before register allocation but not after, or vice-versa")]
    InstructionMismatchedBogusDefinition,
}

fn check_ty_constraint(ty: LirType, physical_loc: PhysicalLoc) -> Result<(), RegAllocInputError> {
    let is_consistent_with_type = match physical_loc {
        PhysicalLoc::Reg(physical_reg) => {
             match ty {
                LirType::General |
                LirType::Int32 |
                LirType::Object |
                LirType::Slots |
                LirType::Type |
                LirType::Payload |
                LirType::Box => physical_reg.is_general(),

                LirType::Float32 |
                LirType::Double |
                LirType::Simd128Int |
                LirType::Simd128Float => physical_reg.is_float(),
            }
        },
        _ => true,
    };

    if !is_consistent_with_type {
        return Err(RegAllocInputError::TypeConstraintViolated);
    }

    Ok(())
}

#[derive(Clone, Debug)]
struct RegAllocMapping {
    virtual_reg: VirtualReg,
    physical_loc: PhysicalLoc,
}

impl RegAllocMapping {
    fn from_lir_allocations(before_allocation: &LirAllocation, after_allocation: &LirAllocation) -> Result<Option<Self>, RegAllocInputError> {
        let after_physical_loc = match after_allocation.physical_loc() {
            Some(after_physical_loc) => after_physical_loc,
            None => {
                return match (before_allocation, after_allocation) {
                    (LirAllocation::Bogus, LirAllocation::Bogus) |
                    (LirAllocation::Constant, LirAllocation::Constant) => Ok(None),
                    _ => Err(RegAllocInputError::AfterAllocationMissingPhysicalLoc),
                };
            }
        };

        let before_use_info = before_allocation.use_info().ok_or(RegAllocInputError::BeforeAllocationMissingUseInfo)?;

        if after_allocation.use_info().is_some() {
            return Err(RegAllocInputError::AfterAllocationHasUseInfo);
        }

        if let Some(before_physical_loc) = before_allocation.physical_loc() {
            if before_physical_loc != after_physical_loc {
                return Err(RegAllocInputError::BeforeAllocationPhysicalLocDoesNotMatchAfter);
            }
        }

        let is_consistent_with_before_use_policy = match before_use_info.policy() {
            LirUsePolicy::Any(_) => true,
            LirUsePolicy::Reg => match after_physical_loc {
                PhysicalLoc::Reg(_) => true,
                _ => false,
            },
        };
        if !is_consistent_with_before_use_policy {
            return Err(RegAllocInputError::AllocationUsePolicyViolated);
        }

        Ok(Some(RegAllocMapping {
            virtual_reg: before_use_info.virtual_reg(),
            physical_loc: after_physical_loc,
        }))
    }

    fn from_lir_definitions(maybe_before_definition: &Option<LirDefinition>, maybe_after_definition: &Option<LirDefinition>) -> Result<Option<Self>, RegAllocInputError> {
        let (before_definition, after_definition) = match (maybe_before_definition, maybe_after_definition) {
            (Some(before_definition), Some(after_definition)) => (before_definition, after_definition),
            (None, None) => return Ok(None),
            _ => return Err(RegAllocInputError::InstructionMismatchedBogusDefinition),
        };

        if before_definition.virtual_reg() != after_definition.virtual_reg() {
            return Err(RegAllocInputError::BeforeDefinitionVirtualRegDoesNotMatchAfter);
        }

        if before_definition.ty() != after_definition.ty() {
            return Err(RegAllocInputError::BeforeDefinitionTypeDoesNotMatchAfter);
        }

        let after_physical_loc = after_definition.physical_loc().ok_or(RegAllocInputError::AfterDefinitionHasNoPhysicalLoc)?;

        let is_consistent_with_before_policy = match before_definition.policy() {
            LirDefinitionPolicy::Reg => match after_physical_loc {
                PhysicalLoc::Reg(_) => true,
                _ => false,
            },
            LirDefinitionPolicy::ReuseInput(_) => true,
            LirDefinitionPolicy::Fixed(fixed_definition_policy) => fixed_definition_policy.physical_loc() == after_physical_loc,
        };
        if !is_consistent_with_before_policy {
            return Err(RegAllocInputError::DefinitionPolicyViolated);
        }

        check_ty_constraint(after_definition.ty(), after_physical_loc)?;

        Ok(Some(RegAllocMapping {
            virtual_reg: before_definition.virtual_reg(),
            physical_loc: after_physical_loc,
        }))
    }
}

// TODO: Replace with a pinned slice of RegAllocNodes and use direct pinned
// internal references instead of node id lookups.
type RegAllocGraph = Bag<LirNodeId, RegAllocNode>;

fn reg_alloc_graph_from_lir(before_graph: &LirGraph, after_graph: &LirGraph) -> Result<RegAllocGraph, LirNodeError<RegAllocInputError>> {
    let mut out_graph = Bag::new(after_graph.capacity());

    for (node_id, after_node) in after_graph.iter() {
        let maybe_before_node = before_graph.item(node_id);
        let out_node = RegAllocNode::from_lir(maybe_before_node, after_node).map_err(|err| LirNodeError { node_id: node_id, source: err })?;
        out_graph.put(node_id, out_node);
    }

    Ok(out_graph)
}

#[derive(Clone, Debug)]
struct RegAllocNode {
    operation: RegAllocOperation,
    predecessors: RegAllocPredecessors,
    successors: Box<[LirNodeId]>,
    state: RegAllocState,
    has_complete_state: bool,
}

impl RegAllocNode {
    fn from_lir(maybe_before_node: Option<&LirNode>, after_node: &LirNode) -> Result<Self, RegAllocInputError> {
        let out_operation = RegAllocOperation::from_lir(maybe_before_node, after_node)?;
        let out_predecessors = RegAllocPredecessors::from_lir(after_node)?;

        Ok(RegAllocNode {
            operation: out_operation,
            predecessors: out_predecessors,
            successors: after_node.successors().to_owned().into_boxed_slice(),
            state: RegAllocState::new(),
            has_complete_state: false,
        })
    }
}

#[derive(Clone, Debug)]
enum RegAllocPredecessors {
    AtBlockStart(Box<[LirNodeId]>),
    InBlockBody(LirNodeId),
}

impl RegAllocPredecessors {
    fn from_lir(node: &LirNode) -> Result<Self, RegAllocInputError> {
        if node.is_at_block_start() {
            Ok(RegAllocPredecessors::AtBlockStart(node.predecessors().to_owned().into_boxed_slice()))
        } else {
            match node.predecessors() {
                [predecessor_node_id] => Ok(RegAllocPredecessors::InBlockBody(*predecessor_node_id)),
                _ => Err(RegAllocInputError::NodeInBlockBodyHasMultiplePredecessors),
            }
        }
    }
}

#[derive(Clone, Debug)]
enum RegAllocOperation {
    MoveGroup(RegAllocMoveGroup),
    Phi(RegAllocPhi),
    Instruction(RegAllocInstruction),
}

impl RegAllocOperation {
    fn from_lir(maybe_before_node: Option<&LirNode>, after_node: &LirNode) -> Result<Self, RegAllocInputError> {
        match after_node.operation() {
            LirOperation::MoveGroup(move_group) => {
                if maybe_before_node.is_some() {
                    return Err(RegAllocInputError::MoveGroupHasBeforeNode);
                }

                let out_move_group = RegAllocMoveGroup::from_lir(after_node, move_group)?;
                Ok(RegAllocOperation::MoveGroup(out_move_group))
            },
            LirOperation::Phi => {
                let before_node = maybe_before_node.ok_or(RegAllocInputError::PhiHasNoBeforeNode)?;

                match before_node.operation() {
                    LirOperation::Phi => (),
                    _ => return Err(RegAllocInputError::PhiDoesNotMatchBeforeOperation),
                }

                let out_phi = RegAllocPhi::from_lir(before_node, after_node)?;
                Ok(RegAllocOperation::Phi(out_phi))
            },
            LirOperation::Other => {
                let before_node = maybe_before_node.ok_or(RegAllocInputError::InstructionHasNoBeforeNode)?;

                let out_instruction = RegAllocInstruction::from_lir(before_node, after_node)?;
                Ok(RegAllocOperation::Instruction(out_instruction))
            }
        }
    }

    fn judge(&self, state: &RegAllocState) -> Possibility<()> {
        match self {
            RegAllocOperation::MoveGroup(_) => Possibility::Known(()),
            RegAllocOperation::Phi(phi) => phi.judge(state),
            RegAllocOperation::Instruction(instruction) => instruction.judge(state),
        }
    }

    fn transfer(&self, state: &mut RegAllocState) {
        match self {
            RegAllocOperation::MoveGroup(move_group) => move_group.transfer(state),
            RegAllocOperation::Phi(phi) => phi.transfer(state),
            RegAllocOperation::Instruction(instruction) => instruction.transfer(state),
        }
    }
}

#[derive(Clone, Debug)]
struct RegAllocMoveGroup {
    moves: Box<[RegAllocMove]>,
}

impl RegAllocMoveGroup {
    fn from_lir(after_node: &LirNode, move_group: &LirMoveGroup) -> Result<Self, RegAllocInputError> {
        if after_node.operands().len() != 0 {
            return Err(RegAllocInputError::MoveGroupHasOperands);
        }

        if after_node.defs().len() != 0 {
            return Err(RegAllocInputError::MoveGroupHasDefs);
        }

        if after_node.temps().len() != 0 {
            return Err(RegAllocInputError::MoveGroupHasTemps);
        }

        if after_node.successors().len() != 1 {
            return Err(RegAllocInputError::MoveGroupHasInvalidSuccessorCount);
        }

        let out_moves = move_group.moves().iter().map(RegAllocMove::from_lir).collect::<Result<Box<[_]>, _>>()?;

        Ok(RegAllocMoveGroup {
            moves: out_moves,
        })
    }

    fn transfer(&self, state: &mut RegAllocState) {
        let old_state = state.clone();
        for move_info in self.moves.iter() {
            move_info.transfer(state, &old_state);
        }
    }
}

#[derive(Clone, Debug)]
struct RegAllocMove {
    from: PhysicalLoc,
    to: PhysicalLoc,
}

impl RegAllocMove {
    fn from_lir(move_info: &LirMove) -> Result<Self, RegAllocInputError> {
        fn unpack_move_allocation(ty: LirType, lir_allocation: &LirAllocation) -> Result<PhysicalLoc, RegAllocInputError> {
            let physical_loc = lir_allocation.physical_loc().ok_or(RegAllocInputError::MoveAllocationHasNoPhysicalLoc)?;

            if lir_allocation.use_info().is_some() {
                return Err(RegAllocInputError::MoveAllocationHasUseInfo);
            }

            check_ty_constraint(ty, physical_loc)?;
            Ok(physical_loc)
        }

        let out_from = unpack_move_allocation(move_info.ty(), move_info.from())?;
        let out_to = unpack_move_allocation(move_info.ty(), move_info.to())?;

        Ok(RegAllocMove {
            from: out_from,
            to: out_to,
        })
    }

    fn transfer(&self, state: &mut RegAllocState, old_state: &RegAllocState) {
        match old_state.get(&self.from) {
            Some(virtual_regs) => {
                state.insert(self.to, virtual_regs.clone());
            },
            None => {
                state.remove(&self.to);
            },
        }
    }
}

fn check_before_after_node_consistency(before_node: &LirNode, after_node: &LirNode) -> Result<(), RegAllocInputError> {
    if before_node.operands().len() != after_node.operands().len() {
        return Err(RegAllocInputError::OperandCountsDoNotMatch);
    }

    if before_node.defs().len() != after_node.defs().len() {
        return Err(RegAllocInputError::DefCountsDoNotMatch);
    }

    if before_node.temps().len() != after_node.temps().len() {
        return Err(RegAllocInputError::TempCountsDoNotMatch);
    }

    /*
    if before_node.successors() != after_node.successors() {
        return Err(RegAllocInputError::SuccessorsDoNotMatch);
    }
    */

    Ok(())
}

#[derive(Clone, Debug)]
struct RegAllocPhi {
    inputs: Box<[VirtualReg]>,
    output: RegAllocMapping,
}

impl RegAllocPhi {
    fn from_lir(before_node: &LirNode, after_node: &LirNode) -> Result<Self, RegAllocInputError> {
        check_before_after_node_consistency(before_node, after_node)?;

        if after_node.defs().len() != 1 {
            return Err(RegAllocInputError::PhiHasInvalidDefCount);
        }

        if after_node.temps().len() != 0 {
            return Err(RegAllocInputError::PhiHasInvalidTempCount);
        }

        if after_node.successors().len() != 1 {
            return Err(RegAllocInputError::PhiHasInvalidSuccessorCount);
        }

        let before_output = &before_node.defs()[0];
        let after_output = &after_node.defs()[0];

        match before_output.as_ref().map(LirDefinition::policy) {
            None => (),
            Some(LirDefinitionPolicy::ReuseInput(reuse_input_definition_policy)) if reuse_input_definition_policy.input() < before_node.operands().len() => (),
            _ => return Err(RegAllocInputError::PhiOutputHasInvalidPolicy),
        }

        let out_output = RegAllocMapping::from_lir_definitions(before_output, after_output)?.ok_or(RegAllocInputError::PhiOutputIsBogus)?;

        let before_inputs = before_node.operands().iter();
        let after_inputs = after_node.operands().iter();

        let out_inputs = before_inputs.zip(after_inputs).map(|(before_operand, after_operand)| {
            if let Some(before_use_info) = before_operand.use_info() {
                if before_use_info.is_used_at_start() {
                    return Err(RegAllocInputError::PhiInputHasInvalidUseInfo);
                }

                match before_use_info.policy() {
                    LirUsePolicy::Any(before_any_use_policy) => {
                        if before_any_use_policy.is_recovered_input() {
                            return Err(RegAllocInputError::PhiInputHasInvalidUseInfo);
                        }
                    },
                    LirUsePolicy::Reg => (),
                }
            }

            let operand_mapping = RegAllocMapping::from_lir_allocations(before_operand, after_operand)?.ok_or(RegAllocInputError::PhiInputIsNotBackedByStorage)?;

            if operand_mapping.physical_loc != out_output.physical_loc {
                return Err(RegAllocInputError::PhiInputIsNotAllocatedToOutput);
            }

            Ok(operand_mapping.virtual_reg)
        }).collect::<Result<Box<[_]>, _>>()?;

        Ok(RegAllocPhi {
            inputs: out_inputs,
            output: out_output,
        })
    }

    fn judge(&self, state: &RegAllocState) -> Possibility<()> {
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

    fn transfer(&self, state: &mut RegAllocState) {
        let virtual_output = self.output.virtual_reg;
        let physical_output = self.output.physical_loc;
        state.insert(physical_output, vec![Possibility::Known(virtual_output)].into_boxed_slice());
    }
}

#[derive(Clone, Debug)]
struct RegAllocInstruction {
    operands: Box<[RegAllocMapping]>,
    defs: Box<[RegAllocMapping]>,
    temps: Box<[RegAllocMapping]>,
}

impl RegAllocInstruction {
    fn from_lir(before_node: &LirNode, after_node: &LirNode) -> Result<Self, RegAllocInputError> {
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

        let out_operands = before_operands.zip(after_operands).map(|(before_operand, after_operand)| {
            if let Some(before_use_info) = before_operand.use_info() {
                match before_use_info.policy() {
                    LirUsePolicy::Any(before_any_use_policy) => {
                        if before_any_use_policy.is_recovered_input() {
                            return Err(RegAllocInputError::InstructionOperandHasInvalidUseInfo);
                        }
                    },
                    LirUsePolicy::Reg => (),
                }
            }

            RegAllocMapping::from_lir_allocations(before_operand, after_operand)
        }).filter_map(flip_result_of_option).collect::<Result<Box<[_]>, _>>()?;

        let before_defs = before_node.defs().iter();
        let after_defs = after_node.defs().iter();

        let out_defs = before_defs.zip(after_defs).map(|(before_def, after_def)| {
            RegAllocMapping::from_lir_definitions(before_def, after_def)
        }).filter_map(flip_result_of_option).collect::<Result<Box<[_]>, _>>()?;

        let before_temps = before_node.temps().iter();
        let after_temps = after_node.temps().iter();

        let out_temps = before_temps.zip(after_temps).map(|(before_temp, after_temp)| {
            RegAllocMapping::from_lir_definitions(before_temp, after_temp)
        }).filter_map(flip_result_of_option).collect::<Result<Box<[_]>, _>>()?;

        Ok(RegAllocInstruction {
            operands: out_operands,
            defs: out_defs,
            temps: out_temps,
        })
    }

    fn judge(&self, state: &RegAllocState) -> Possibility<()> {
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

    fn transfer(&self, state: &mut RegAllocState) {
        // Make sure to apply temps before defs here! They can overlap, so order
        // is important.
        for def_or_temp in self.temps.iter().chain(self.defs.iter()) {
            state.insert(def_or_temp.physical_loc, vec![Possibility::Known(def_or_temp.virtual_reg)].into_boxed_slice());
        }
    }
}
