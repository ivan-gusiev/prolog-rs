use std::{collections::HashMap, fmt::Display, hash::Hash, iter::FromIterator};

use crate::{
    data::{HeapPtr, RegPtr},
    lang::{Term, VarName},
    symbol::{to_display, SymDisplay, SymbolTable},
};

#[derive(Debug, PartialEq, Eq)]
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
            write!(f, "{}={}", s, t)?;
            while let Some((t, s)) = pairs.next() {
                write!(f, ", {}={}", s, t)?;
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
            while let Some((t, s)) = pairs.next() {
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

impl<T: Eq + Hash> VarInfo<T> {
    pub fn from_hash(hash_map: HashMap<T, VarName>) -> VarInfo<T> {
        Self(hash_map)
    }

    pub fn from_inverse(hash_map: HashMap<VarName, T>) -> VarInfo<T> {
        Self::from_iter(hash_map.into_iter().map(|(v, t)| (t, v)))
    }

    pub fn get(&self, key: &T) -> Option<VarName> {
        self.0.get(key).copied()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn info(&self) -> impl Iterator<Item = (&T, &VarName)> {
        self.0.iter()
    }

    pub fn transform<NewT, F>(&self, mut mapper: F) -> VarInfo<NewT>
    where
        F: FnMut(&T) -> NewT,
        NewT: Eq + Hash,
    {
        VarInfo::<NewT>::from_iter(self.info().map(|(t, v)| (mapper(t), *v)))
    }

    pub fn traverse<NewT, F, E>(&self, mut mapper: F) -> Result<VarInfo<NewT>, E>
    where
        F: FnMut(&T) -> Result<NewT, E>,
        NewT: Eq + Hash,
    {
        let mut result = HashMap::<NewT, VarName>::new();
        for (t, v) in self.info() {
            result.insert(mapper(t)?, *v);
        }
        Ok(VarInfo::from_hash(result))
    }
}

/* // TODO: remove this
impl<T> VarInfo<T> where T: Eq + Hash + Clone {
    pub fn invert(&self) -> HashMap<VarName, T> {
        HashMap::from_iter(self.info().map(|(k, v)| (v.clone(), *k)))
    }
}
*/

pub type VarMapping = VarInfo<RegPtr>;

pub type VarBindings = VarInfo<HeapPtr>;

pub type VarValues = VarInfo<Term>;
