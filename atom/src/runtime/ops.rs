macro_rules! float_op {
    ($lhs:expr, $rhs:expr, bitand, $block:block) => {
        $block
    };

    ($lhs:expr, $rhs:expr, bitor, $block:block) => {
        $block
    };
    
    ($lhs:expr, $rhs:expr, bitxor, $block:block) => {
        $block
    };

    ($lhs:expr, $rhs:expr, shl, $block:block) => {
        $block
    };

    ($lhs:expr, $rhs:expr, shr, $block:block) => {
        $block
    };

    ($lhs:expr, $rhs:expr, $name:ident, $block:block) => {
        $lhs.$name($rhs).into()
    };
}

pub(crate) use float_op;

macro_rules! string_op {
    ($lhs:expr, $rhs:expr, eq, $block:block) => {
        $lhs.eq($rhs).into()
    };

    ($lhs:expr, $rhs:expr, ne, $block:block) => {
        $lhs.ne($rhs).into()
    };

    ($lhs:expr, $rhs:expr, $name:ident, $block:block) => {
        $block
    };
}

pub(crate) use string_op;

macro_rules! atom_op {
    ($lhs:expr, $rhs:expr, eq, $block:block) => {
        $lhs.eq($rhs).into()
    };

    ($lhs:expr, $rhs:expr, ne, $block:block) => {
        $lhs.ne($rhs).into()
    };

    ($lhs:expr, $rhs:expr, $name:ident, $block:block) => {
        $block
    };
}

pub(crate) use atom_op;
