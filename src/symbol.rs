extern crate string_interner;

use std::convert::{TryFrom, TryInto};

use self::string_interner::{DefaultSymbol, StringInterner, Symbol as _};

// TODO: This panics if the interned string symbol starts with bit 1. Fix this.
/// An inlinable, copyable reference to a string
/// If the head bit is 1, clear it and treat the whole thing as a [DefaultSymbol].
/// Otherwise,
///   * the first byte is the length (0-3).
///   * the next 0-3 bytes are string content
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
struct Symbol(u8, u8, u8, u8);

impl Symbol {
    pub fn is_interned(&self) -> bool {
        is_head_set(self.0)
    }

    pub fn is_inline(&self) -> bool {
        return !self.is_interned();
    }
}

impl From<u32> for Symbol {
    fn from(value: u32) -> Self {
        // TODO: should use native endianness, I guess?
        let bytes = u32::to_be_bytes(value);
        Symbol(bytes[0], bytes[1], bytes[2], bytes[3])
    }
}

impl From<Symbol> for u32 {
    fn from(value: Symbol) -> Self {
        u32::from_be_bytes([value.0, value.1, value.2, value.3])
    }
}

impl TryFrom<&str> for Symbol {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let bytes = value.as_bytes();
        if bytes.len() > 3 {
            return Err(());
        }

        let mut result = Symbol(bytes.len() as u8, 0, 0, 0);
        let outs = [&mut result.1, &mut result.2, &mut result.3];
        for i in 0..bytes.len() {
            *outs[i] = bytes[i]
        }
        Ok(result)
    }
}

impl TryFrom<DefaultSymbol> for Symbol {
    type Error = ();

    fn try_from(value: DefaultSymbol) -> Result<Self, Self::Error> {
        let val: u32 = value.to_usize().try_into().map_err(|_| ())?;
        if val & 0x80000000 == 0x80000000 {
            return Err(());
        }

        Ok((val | 0x80000000).into())
    }
}

impl TryFrom<Symbol> for String {
    type Error = ();

    fn try_from(value: Symbol) -> Result<Self, Self::Error> {
        if !value.is_inline() {
            return Err(());
        }

        fn mkstr(bytes: &[u8]) -> Result<String, ()> {
            std::str::from_utf8(bytes)
                .map(|s| s.to_string())
                .map_err(|_| ())
        }

        match value.0 {
            0 => Ok(String::new()),
            1 => mkstr(&[value.1]),
            2 => mkstr(&[value.1, value.2]),
            3 => mkstr(&[value.1, value.2, value.3]),
            _ => Err(()),
        }
    }
}

impl TryFrom<Symbol> for DefaultSymbol {
    type Error = ();

    fn try_from(mut value: Symbol) -> Result<Self, Self::Error> {
        if !value.is_interned() {
            return Err(());
        }

        value.0 = clear_head(value.0);
        let combined: u32 = value.into();
        DefaultSymbol::try_from_usize(combined as usize).ok_or(())
    }
}

fn is_head_set(byte: u8) -> bool {
    byte & 0x80 == 0x80
}

fn clear_head(byte: u8) -> u8 {
    byte & 0x7f
}

/// A symbol table that associates any non-self-sufficient [Symbol] with a string.
/// Really an opaque wrapper over the library implementation of string interner.
#[derive(Default)]
struct SymbolTable(StringInterner);

impl SymbolTable {
    pub fn new() -> SymbolTable {
        Default::default()
    }

    pub fn intern(&mut self, str: &str) -> Symbol {
        match str.try_into() {
            Ok(sym) => sym,
            Err(_) => {
                let sisym = self.0.get_or_intern(str);
                sisym.try_into().expect(
                    format!(
                        "Interning backend returned a higher than expected symbol index {:?}",
                        sisym
                    )
                    .as_str(),
                )
            }
        }
    }

    pub fn resolve(&self, sym: Symbol) -> Option<String> {
        let str_result: Result<String, ()> = sym.try_into();
        match str_result {
            Ok(str) => Some(str),
            Err(_) => match sym.try_into() {
                Ok(defsym) => self.0.resolve(defsym).map(|s| s.to_string()),
                Err(_) => None,
            },
        }
    }
}

#[test]
fn test_roundtrip_short() {
    let mut table = SymbolTable::new();
    let sym = table.intern("cat");
    assert_eq!(table.resolve(sym), Some("cat".to_string()))
}

#[test]
fn test_roundtrip_long() {
    let mut table = SymbolTable::new();
    let sym = table.intern("caterpillar");
    assert_eq!(table.resolve(sym), Some("caterpillar".to_string()))
}
