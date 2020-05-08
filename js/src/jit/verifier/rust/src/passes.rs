mod base;
mod reg_alloc;
mod spectre;

pub use crate::passes::base::Pass;
pub use crate::passes::reg_alloc::RegAllocPass;
pub use crate::passes::spectre::SpectrePass;

