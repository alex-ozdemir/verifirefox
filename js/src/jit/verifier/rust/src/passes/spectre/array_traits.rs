use crate::ast::lir::{LirAllocation, LirNode, typed_ops::{self,*}};
use crate::match_op;

pub trait ArrayAccess {
    fn node(&self) -> &LirNode;
    fn array(&self) -> &LirAllocation;
    fn index(&self) -> &LirAllocation;
}

macro_rules! impl_array_accesses {
    [ $($op:ident($array:ident, $index:ident)),+ $(,)?] => {
        pub fn array_access<'a>(node: &'a LirNode) -> Option<&'a dyn ArrayAccess> {
            match_op! {
                match node {
                    $(typed_ops::$op(it) => Some(it),)+
                    _ => {None},
                }
            }
        }

        $(impl_array_accesses!($op, $array, $index);)*
    };

    ($typ:ty, $array:ident, $index:ident) => {
        impl ArrayAccess for $typ {
            fn node(&self) -> &LirNode {
                Self::node(self)
            }

            fn array(&self) -> &LirAllocation {
                Self::$array(self)
            }

            fn index(&self) -> &LirAllocation {
                Self::$index(self)
            }
        }
    };
}

impl_array_accesses! [
    StoreElementV(array, index),
    StoreElementT(array, index),
    StoreUnboxedScalar(array, index),
    StoreUnboxedBigInt(array, index),
    LoadElementV(array, index),
    LoadElementT(array, index),
    LoadUnboxedScalar(array, index),
    LoadUnboxedBigInt(array, index),

    // Element hole loading implements spectre checks as part of lowering
    // So for the purposes of LIR-level spectre checks, we can ignore them

    // LoadElementHole(array, index),
    // LoadTypedArrayElementHole(object, index),
    // LoadTypedArrayElementHoleBigInt(object, index),
    // StoreTypedArrayElementHole(array, index),
    // StoreTypedArrayElementHoleBigInt(array, index),
    // StoreElementHoleV(array, index),
    // StoreElementHoleT(array, index),

    // Fallible stores also implement their spectre mask during lowering
    // FallibleStoreElementT(array, index),
    // FallibleStoreElementV(array, index),
];


pub trait ArrayLengthAccess {
    fn node(&self) -> &LirNode;
    fn array(&self) -> &LirAllocation;
}

pub fn array_length_access<'a>(node: &'a LirNode) -> Option<&'a dyn ArrayLengthAccess> {
    match_op! {
        match node {
            typed_ops::ArrayLength(it) => Some(it),
            typed_ops::TypedArrayLength(it) => Some(it),
            typed_ops::InitializedLength(it) => Some(it),
            _ => None
        }
    }
}

impl ArrayLengthAccess for ArrayLength {
    fn node(&self) -> &LirNode {
        ArrayLength::node(self)
    }
    fn array(&self) -> &LirAllocation {
        ArrayLength::array(self)
    }
}

impl ArrayLengthAccess for TypedArrayLength {
    fn node(&self) -> &LirNode {
        TypedArrayLength::node(self)
    }
    fn array(&self) -> &LirAllocation {
        TypedArrayLength::obj(self)
    }
}

impl ArrayLengthAccess for InitializedLength {
    fn node(&self) -> &LirNode {
        InitializedLength::node(self)
    }
    fn array(&self) -> &LirAllocation {
        InitializedLength::array(self)
    }
}