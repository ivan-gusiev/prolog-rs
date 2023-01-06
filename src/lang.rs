use std::fmt::{Display, Formatter};

pub type VarName = char;
pub type FunctorName = char;
pub type Arity = u32;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Functor(pub FunctorName, pub Arity);

impl Display for Functor {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}/{}", self.0, self.1)
    }
}
