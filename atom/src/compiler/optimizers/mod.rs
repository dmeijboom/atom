use atom_ir::IR;

use crate::compiler::Module;

pub mod call_void;
pub mod load_local_twice_add;
pub mod pre_compute_labels;
pub mod remove_core_validations;

pub type Optimizer = fn(&Module, &mut IR);
