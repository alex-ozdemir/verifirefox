mod base;
mod reg_alloc;
mod spectre;
mod undef;

pub use crate::passes::base::Pass;
pub use crate::passes::reg_alloc::RegAllocPass;
pub use crate::passes::spectre::SpectrePass;
pub use crate::passes::undef::lir::LirUndefUsePass;
pub use crate::passes::undef::mir::MirUndefUsePass;
