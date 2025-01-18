use crate::lex::Tk;
use logos::Logos;
use thiserror::Error;

/// Errors that may happen while parsing
#[derive(Error, Debug, Eq, PartialEq)]
pub enum ParseError {
    #[error("(At line: {0}) Unexpected token: {1}, expecting one of: {2:?}")]
    Unexpected(usize, String, &'static [&'static str]),

    #[error("Unknown error: {0}")]
    Unknown(String),

    #[error("Error while lexing the file, at index: {0}, near: {1:?}")]
    Lex(usize, String),
}

/// A type, pseudobnf:
/// type ::= ( "const" ? ) ( "!" ?) storage-class
#[derive(Debug, PartialEq, Eq)]
pub struct Ty {
    pub constant: bool,
    pub error: bool,
    pub storage: StorageClass,
}

/// The actual data structure used to store the data, pseudobnf:
/// storage-class ::= primitive-ty
///                 | pointer-ty
///                 | struct-union-ty
///                 | tuple-ty
///                 | tagged-union-ty
///                 | slice-array-ty
///                 | function-ty
///                 | alias-ty
///                 | unwrapped-alias-ty
#[derive(Debug, PartialEq, Eq)]
pub enum StorageClass {
    Primitive(PrimitiveTy),
}

/// A primitive type, pseudobnf:
/// primitive-ty ::=
#[derive(Debug, PartialEq, Eq)]
pub enum PrimitiveTy {
    IntegerTy(IntegerTy),
    FloatTy(FloatTy),
    Bool,
    Done,
    Never,
    Opaque,
    Rune,
    Str,
    Valist,
    Void,
}

/// The integer types
#[derive(Debug, PartialEq, Eq)]
pub enum IntegerTy {
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    Int,
    UInt,
    Size,
    UIntPtr,
}

/// The floating point types
#[derive(Debug, PartialEq, Eq)]
pub enum FloatTy {
    F32,
    F64,
}

/// This struct transforms a sequence of tokens into structures that are more ergonomic
/// This is anover layer of abstraction just over the lex stage
#[derive(Debug)]
pub struct Parser<'a> {
    data: Vec<Tk<'a>>,
    lines: Vec<usize>,
    idx: usize,
}

impl<'a> Parser<'a> {
    pub(crate) fn unexpected(&self, hints: &'static [&'static str]) -> ParseError {
        let line = match self.lines.get(self.lines.len().min(self.idx)) {
            Some(line) => line,
            Option::None => return ParseError::Unknown("The parser has no data!".into()),
        };
        // Should not panic, coz its already handled above
        let tk = self.lines[self.idx];
        ParseError::Unexpected(*line, format!("{tk:?}"), hints)
    }

    pub(crate) fn advance(&mut self, cnt: usize) {
        self.idx += cnt;
    }

    pub(crate) fn slice(&self) -> &[Tk<'a>] {
        &self.data[self.idx..]
    }

    pub fn reset(&mut self) {
        self.idx = 0;
    }

    /// Reduce a primitive type
    pub(crate) fn reduce_primitive_ty(&mut self) -> Result<PrimitiveTy, ParseError> {
        let ty = match self.slice() {
            // i <3 (neo)vim macros
            [Tk::I8, ..] => PrimitiveTy::IntegerTy(IntegerTy::I8),
            [Tk::I16, ..] => PrimitiveTy::IntegerTy(IntegerTy::I16),
            [Tk::I32, ..] => PrimitiveTy::IntegerTy(IntegerTy::I32),
            [Tk::I64, ..] => PrimitiveTy::IntegerTy(IntegerTy::I64),
            [Tk::U8, ..] => PrimitiveTy::IntegerTy(IntegerTy::U8),
            [Tk::U16, ..] => PrimitiveTy::IntegerTy(IntegerTy::U16),
            [Tk::U32, ..] => PrimitiveTy::IntegerTy(IntegerTy::U32),
            [Tk::U64, ..] => PrimitiveTy::IntegerTy(IntegerTy::U64),
            [Tk::Int, ..] => PrimitiveTy::IntegerTy(IntegerTy::Int),
            [Tk::Uint, ..] => PrimitiveTy::IntegerTy(IntegerTy::UInt),
            [Tk::Size, ..] => PrimitiveTy::IntegerTy(IntegerTy::Size),
            [Tk::Uintptr, ..] => PrimitiveTy::IntegerTy(IntegerTy::UIntPtr),
            [Tk::F32, ..] => PrimitiveTy::FloatTy(FloatTy::F32),
            [Tk::F64, ..] => PrimitiveTy::FloatTy(FloatTy::F64),
            [Tk::Bool, ..] => PrimitiveTy::Bool,
            [Tk::Done, ..] => PrimitiveTy::Done,
            [Tk::Never, ..] => PrimitiveTy::Never,
            [Tk::Opaque, ..] => PrimitiveTy::Opaque,
            [Tk::Rune, ..] => PrimitiveTy::Rune,
            [Tk::Str, ..] => PrimitiveTy::Str,
            [Tk::Valist, ..] => PrimitiveTy::Valist,
            [Tk::Void, ..] => PrimitiveTy::Void,
            _ => {
                return Err(self.unexpected(&[
                    "i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64", "int", "uint", "size",
                    "uintptr", "f32", "f64", "bool", "never", "opaque", "rune", "str", "valist",
                    "void",
                ]))
            }
        };
        self.advance(1);
        Ok(ty)
    }

    /// Reduce a storage-class
    pub(crate) fn reduce_storage_class(&mut self) -> Result<StorageClass, ParseError> {
        let idx = self.idx;

        // Check if is a primitive type
        if let Ok(prim) = self.reduce_primitive_ty() {
            return Ok(StorageClass::Primitive(prim));
        };
        self.idx = idx; // (Restore the state)

        // Nothing matched
        return Err(self.unexpected(&[
            "primitive-type",
            "pointer-type",
            "struct-union-type",
            "tuple-type",
            "tagged-union-type",
            "slice-array-type",
            "function-type",
            "alias-type",
            "unwrapped-alias",
        ]));
    }

    /// Reduce a type, see [`Ty`]
    pub(crate) fn reduce_ty(&mut self) -> Result<Ty, ParseError> {
        // Check if is constant
        let constant = match self.slice() {
            [Tk::Const, ..] => {
                self.advance(1);
                true
            }
            _ => false,
        };

        // Check if is error
        let errty = match self.slice() {
            [Tk::Excl, ..] => {
                self.advance(1);
                true
            }
            _ => false,
        };

        // Now get the storage class
        let stor = self.reduce_storage_class()?;
        Ok(Ty {
            constant,
            error: errty,
            storage: stor,
        })
    }

    pub fn new(data: impl Into<&'a str>) -> Result<Self, ParseError> {
        let mut stream = Tk::lexer(data.into());
        let mut tks = Vec::new();
        let mut lns = Vec::new();
        let mut line = 1;
        // TODO: Keep the comments right before a declaration
        while let Some(tk) = stream.next() {
            match tk {
                Ok(Tk::Nl) => line += 1,
                Ok(Tk::Comment(_)) => (),
                Ok(to) => {
                    tks.push(to);
                    lns.push(line);
                }
                Err(_) => {
                    let sli = stream.slice();
                    return Err(ParseError::Lex(line, sli[..sli.len().min(3)].into()));
                }
            }
        }

        Ok(Self {
            data: tks,
            lines: lns,
            idx: 0,
        })
    }
}
