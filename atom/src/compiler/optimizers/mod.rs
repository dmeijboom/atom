use atom_ir::IR;

use crate::compiler::Module;

pub mod call_void;
pub mod load_local_twice_add;
pub mod pre_compute_labels;
pub mod remove_type_cast;
pub mod tail_call;

pub type Optimizer = fn(&Module, &mut IR);
