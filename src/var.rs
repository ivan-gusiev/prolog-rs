use std::{collections::HashMap, fmt::Display, hash::Hash, iter::FromIterator};

use crate::{
    data::{Addr, Data, HeapPtr, Local, RegPtr},
    lang::{Term, VarName},
    symbol::{to_display, SymDisplay, SymbolTable},
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct VarInfo<T: Eq + Hash>(HashMap<T, VarName>);

impl<T: Eq + Hash> Default for VarInfo<T> {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<T: Display + Eq + Hash> Display for VarInfo<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut pairs = self.0.iter();
        write!(f, "{{")?;
        if let Some((t, s)) = pairs.next() {
            write!(f, "{s}={t}")?;
            for (t, s) in pairs {
                write!(f, ", {s}={t}")?;
            }
        }
        write!(f, "}}")
    }
}

impl<T: SymDisplay + Eq + Hash> SymDisplay for VarInfo<T> {
    fn sym_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        symbol_table: &SymbolTable,
    ) -> std::fmt::Result {
        let mut pairs = self.0.iter();
        write!(f, "{{")?;
        if let Some((t, s)) = pairs.next() {
            write!(f, "{}={}", s, to_display(t, symbol_table))?;
            for (t, s) in pairs {
                write!(f, ", {}={}", s, to_display(t, symbol_table))?;
            }
        }
        write!(f, "}}")
    }
}

impl<T: Eq + Hash> FromIterator<(T, VarName)> for VarInfo<T> {
    fn from_iter<I: IntoIterator<Item = (T, VarName)>>(iter: I) -> Self {
        Self(HashMap::from_iter(iter))
    }
}

impl<T: Clone + Eq + Hash> VarInfo<T> {
    pub fn from_hash(hash_map: HashMap<T, VarName>) -> VarInfo<T> {
        Self(hash_map)
    }

    pub fn from_inverse(hash_map: &HashMap<VarName, T>) -> VarInfo<T> {
        Self::from_iter(hash_map.iter().map(|(v, t)| (t.clone(), *v)))
    }

    pub fn get(&self, key: &T) -> Option<VarName> {
        self.0.get(key).copied()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn iter(&self) -> impl Iterator<Item = (&T, &VarName)> {
        self.0.iter()
    }

    pub fn append(&mut self, other: Self) {
        self.0.extend(other.0)
    }

    pub fn insert(&mut self, key: T, var: VarName) -> Option<VarName> {
        self.0.insert(key, var)
    }

    pub fn traverse<NewT, F, E>(&self, mut mapper: F) -> Result<VarInfo<NewT>, E>
    where
        F: FnMut(&T) -> Result<NewT, E>,
        NewT: Clone + Eq + Hash,
    {
        let mut result = HashMap::<NewT, VarName>::new();
        for (t, v) in self.iter() {
            result.insert(mapper(t)?, *v);
        }
        Ok(VarInfo::from_hash(result))
    }

    pub fn traverse_filter<NewT, F, E>(&self, mut mapper: F) -> VarInfo<NewT>
    where
        F: FnMut(&T) -> Result<NewT, E>,
        NewT: Clone + Eq + Hash,
    {
        let mut result = HashMap::<NewT, VarName>::new();
        for (t, v) in self.iter() {
            if let Ok(newt) = mapper(t) {
                result.insert(newt, *v);
            }
        }
        VarInfo::from_hash(result)
    }

    pub fn retain_keys<F>(&mut self, mut f: F)
    where
        F: FnMut(&T) -> bool,
    {
        self.0.retain(|k, _| f(k))
    }
}

pub type VarRegs = VarInfo<RegPtr>;

pub type VarMapping = VarInfo<Local>;

pub type VarBindings = VarInfo<HeapPtr>;

#[derive(Debug)]
pub struct VarDescription(VarName, Addr, Data, Term);

impl SymDisplay for VarDescription {
    fn sym_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        symbol_table: &SymbolTable,
    ) -> Result<(), std::fmt::Error> {
        write!(
            f,
            "{}\t({}) =\t{}\t// {}",
            to_display(&self.0, symbol_table),
            self.1,
            to_display(&self.2, symbol_table),
            to_display(&self.3, symbol_table),
        )
    }
}

impl VarDescription {
    pub fn new(name: VarName, addr: Addr, data: Data, term: Term) -> Self {
        Self(name, addr, data, term)
    }

    pub fn short(&self, symbol_table: &SymbolTable) -> String {
        format!(
            "{0} = {1}",
            to_display(&self.0, symbol_table),
            to_display(&self.3, symbol_table)
        )
    }

    pub fn to_assignment(&self) -> (VarName, Term) {
        (self.0, self.3.clone())
    }
}
