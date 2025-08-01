use std::rc::Rc;

use crate::nimy::types::NimType;

// True enum listing types that have no value associated and are usually used as type constraints for generics.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum NimTypeClass {
    Union(Rc<NimType>, Rc<NimType>), // type u = a|b, also written type u = a or b.
    Object,
    Tuple,
    Enum,
    Proc,
    Iterator,
    Ref,
    Ptr,
    Var,
    Distinct,
    Array,
    Set,
    Seq,
    Auto,
    Range,
}

impl std::fmt::Display for NimTypeClass {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NimTypeClass::Union(left, right) => write!(f, "{left} | {right}"),
            NimTypeClass::Object => write!(f, "object"),
            NimTypeClass::Tuple => write!(f, "tuple"),
            NimTypeClass::Enum => write!(f, "enum"),
            NimTypeClass::Proc => write!(f, "proc"),
            NimTypeClass::Iterator => write!(f, "iterator"),
            NimTypeClass::Ref => write!(f, "ref"),
            NimTypeClass::Ptr => write!(f, "ptr"),
            NimTypeClass::Var => write!(f, "var"),
            NimTypeClass::Distinct => write!(f, "distinct"),
            NimTypeClass::Array => write!(f, "array"),
            NimTypeClass::Set => write!(f, "set"),
            NimTypeClass::Seq => write!(f, "seq"),
            NimTypeClass::Auto => write!(f, "auto"),
            NimTypeClass::Range => write!(f, "range"),
        }
    }
}

thread_local! {
    pub static AUTO_CLASS: Rc<NimType> = Rc::new(NimType::TypeClass(NimTypeClass::Auto));
    pub static RANGE_CLASS: Rc<NimType> = Rc::new(NimType::TypeClass(NimTypeClass::Range));
}

// Returns true if a value of type `subtype` can be considered a value of type `supertype`
// If `subtype == supertype`, this is trivially true.
// If the types are different, this is false in most cases except if `supertype` is a type class, or contains a type class.
pub fn is_subtype(subtype: &NimType, supertype: &NimType) -> bool {
    if subtype == supertype {
        return true;
    }
    let NimType::TypeClass(super_type_class) = supertype else {
        return false;
    };
    match super_type_class {
        NimTypeClass::Auto => true, // Auto is a supertype of everything.
        NimTypeClass::Range => matches!(
            subtype,
            NimType::Int | NimType::Range(..) | NimType::Enum(..)
        ),
        NimTypeClass::Ptr => matches!(subtype, NimType::Ptr(..)),
        NimTypeClass::Ref => matches!(subtype, NimType::Ref(..)),
        NimTypeClass::Var => matches!(subtype, NimType::Var(..)),
        NimTypeClass::Distinct => matches!(subtype, NimType::Distinct(..)),
        NimTypeClass::Object => matches!(subtype, NimType::Object(..)),
        NimTypeClass::Tuple => matches!(subtype, NimType::Tuple(..)),
        NimTypeClass::Enum => matches!(subtype, NimType::Enum(..)),
        NimTypeClass::Proc => matches!(subtype, NimType::Proc(..)),
        NimTypeClass::Array => matches!(subtype, NimType::Array(..)),
        NimTypeClass::Set => matches!(subtype, NimType::Set(..)),
        NimTypeClass::Seq => matches!(subtype, NimType::Seq(..)),
        NimTypeClass::Iterator => true, // Not done.
        NimTypeClass::Union(left, right) => is_subtype(subtype, left) || is_subtype(subtype, right),
    }
}

#[cfg(test)]
mod tests {
    use crate::nimy::types::str_to_type;

    use super::*;

    #[test]
    fn test_subtypes() {
        // Let's start by building some types for our tests.
        let int_type = str_to_type("int").unwrap();
        let float_type = str_to_type("float").unwrap();

        assert!(is_subtype(&int_type, &int_type)); // T is a subtype of T
        assert!(!is_subtype(&int_type, &float_type)); // In general, types are not subtypes of other types
    }

    #[test]
    fn test_typeclass() {
        let int_type = str_to_type("int").unwrap();
        let float_type = str_to_type("float").unwrap();
        let string_type = str_to_type("string").unwrap();
        let string_ptr_type = NimType::Ptr(string_type.clone());
        let ptr_class = NimType::TypeClass(NimTypeClass::Ptr);

        // A pointer to a type is a subtype of the pointer type class
        assert!(is_subtype(&string_ptr_type, &ptr_class));
        assert!(!is_subtype(&string_type, &ptr_class));
        assert!(!is_subtype(&ptr_class, &string_type));
        assert!(is_subtype(&string_ptr_type, &string_ptr_type));

        let auto_type = NimType::TypeClass(NimTypeClass::Auto);
        // Auto is a supertype of everything.
        assert!(is_subtype(&int_type, &auto_type));
        assert!(is_subtype(&float_type, &auto_type));
        assert!(is_subtype(&string_ptr_type, &auto_type));
        assert!(is_subtype(&ptr_class, &auto_type));
    }
}
