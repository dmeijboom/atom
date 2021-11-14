use crate::compiler::ir::IR;

use crate::compiler::Module;

pub mod call_void;
pub mod pre_compute_labels;
pub mod remove_type_cast;
pub mod replace_load_with_const;
pub mod tail_call;

pub type Optimizer = fn(&Module, &mut IR);
