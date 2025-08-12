use crate::nimy::{generics::GenericProc, symbols::Symbol, types::NimProcType};

#[derive(Debug)]
pub struct NimProc {
    pub sym: Symbol,
    pub nimtype: NimProcType,
}

impl std::fmt::Display for NimProc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} := {}", self.sym.name, self.nimtype)
    }
}

#[derive(Debug)]
pub enum MaybeGenericProc {
    GenericProc(GenericProc),
    RegularProc(NimProc),
}
