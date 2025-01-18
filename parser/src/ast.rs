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

/// An identifier
/// Which may be an absolute path to some type/function
#[derive(Debug, PartialEq, Eq)]
pub struct Id(Vec<String>);

/// A type, pseudobnf:
/// type ::= ( "const" ? ) ( "!" ?) storage-class
#[derive(Debug, PartialEq, Eq)]
pub struct Ty {
    pub constant: bool,
    pub error: bool,
    pub storage: Box<StorageClass>,
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
    Pointer { nullable: bool, ty: Ty },
    StructUnion(StructUnionTy),
}

/// A primitive type, pseudobnf:
/// primitive-ty ::=
#[derive(Debug, PartialEq, Eq)]
pub enum PrimitiveTy {
    Integer(IntegerTy),
    Float(FloatTy),
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

/// A struct or union type
#[derive(Debug, PartialEq, Eq)]
pub enum StructUnionTy {
    /// A struct definition
    /// struct @packed? { struct-field* }
    Struct {
        packed: bool,
        fields: Vec<StructField>,
    },
    /// A union definition
    /// union { struct-union-field* }
    Union { fields: Vec<StructUnionField> },
}

/// A struct field
#[derive(Debug, PartialEq, Eq)]
pub enum StructUnionField {
    Field { name: String, ty: Ty },
    Sub(Box<StructUnionTy>),
    Id(Id),
}

/// A struct field
/// struct-union-field ::= @offset(expr)? struct-union-fild
#[derive(Debug, PartialEq, Eq)]
pub struct StructField {
    pub offset: Option<()>,
    pub field: StructUnionField,
}

/// This struct transforms a sequence of tokens into structures that are more ergonomic
/// This is another layer of abstraction just over the lex stage
#[derive(Debug)]
pub struct Parser<'a> {
    data: Vec<Tk<'a>>,
    spans: Vec<&'a str>,
    lines: Vec<usize>,
    idx: usize,
}

impl<'a> Parser<'a> {
    pub(crate) fn unexpected(&self, hints: &'static [&'static str]) -> ParseError {
        if self.lines.len() == 0 {
            return ParseError::Unknown("The parser has no data!".into());
        }

        let idx = self.idx.min(self.data.len() - 1);

        // Should not panic, coz its already handled above
        let line = self.lines[self.idx];
        let tk = &self.data[self.idx];
        let span = &self.spans[self.idx];
        ParseError::Unexpected(line, format!("{span:?} ({tk:?})"), hints)
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
            [Tk::I8, ..] => PrimitiveTy::Integer(IntegerTy::I8),
            [Tk::I16, ..] => PrimitiveTy::Integer(IntegerTy::I16),
            [Tk::I32, ..] => PrimitiveTy::Integer(IntegerTy::I32),
            [Tk::I64, ..] => PrimitiveTy::Integer(IntegerTy::I64),
            [Tk::U8, ..] => PrimitiveTy::Integer(IntegerTy::U8),
            [Tk::U16, ..] => PrimitiveTy::Integer(IntegerTy::U16),
            [Tk::U32, ..] => PrimitiveTy::Integer(IntegerTy::U32),
            [Tk::U64, ..] => PrimitiveTy::Integer(IntegerTy::U64),
            [Tk::Int, ..] => PrimitiveTy::Integer(IntegerTy::Int),
            [Tk::Uint, ..] => PrimitiveTy::Integer(IntegerTy::UInt),
            [Tk::Size, ..] => PrimitiveTy::Integer(IntegerTy::Size),
            [Tk::Uintptr, ..] => PrimitiveTy::Integer(IntegerTy::UIntPtr),
            [Tk::F32, ..] => PrimitiveTy::Float(FloatTy::F32),
            [Tk::F64, ..] => PrimitiveTy::Float(FloatTy::F64),
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

    /// Reduce a pointer type
    pub(crate) fn reduce_pointer_ty(&mut self) -> Result<StorageClass, ParseError> {
        // Maybe nullable
        let nullable = match self.slice() {
            [Tk::Nullable, ..] => {
                self.advance(1);
                true
            }
            _ => false,
        };

        // Expect *
        match self.slice() {
            [Tk::Star, ..] => self.advance(1),
            _ => return Err(self.unexpected(&["* (star)"])),
        }

        // Sub-type
        let sub = self.reduce_ty()?;
        Ok(StorageClass::Pointer { nullable, ty: sub })
    }

    /// Reduce an identifier
    pub(crate) fn reduce_id(&mut self) -> Result<Id, ParseError> {
        // Expect at least one name
        let mut path = Vec::new();

        // First segment
        match self.slice() {
            [Tk::Name(name), ..] => {
                path.push((*name).into());
                self.advance(1);
            }
            _ => return Err(self.unexpected(&["(name)"])),
        }

        loop {
            // Check if follows `::`
            match self.slice() {
                [Tk::DColon, ..] => self.advance(1),
                _ => break,
            }

            // Next segment
            match self.slice() {
                [Tk::Name(name), ..] => {
                    path.push((*name).into());
                    self.advance(1);
                }
                _ => return Err(self.unexpected(&["(name)"])),
            }
        }

        Ok(Id(path))
    }

    /// Reduce a struct-union field
    pub(crate) fn reduce_struct_union_field(&mut self) -> Result<StructUnionField, ParseError> {
        match self.slice() {
            [Tk::Name(_), ..] => {
                // May be an identifier of a field decl
                ()
            }
            [Tk::Struct, ..] | [Tk::Union, ..] => {
                // Struct or union declaration
                return Ok(StructUnionField::Sub(self.reduce_struct_union_ty()?.into()));
            }
            _ => return Err(self.unexpected(&["(name)", "(identifier)", "struct", "union"])),
        }

        // Lookahead and check if is a field
        match self.slice() {
            [Tk::Name(name), Tk::Colon, ..] => {
                // It really is
                let name = (*name).into();
                self.advance(2);
                let ty = self.reduce_ty()?;
                Ok(StructUnionField::Field { name, ty })
            }
            _ => Ok(StructUnionField::Id(self.reduce_id()?)),
        }
    }

    /// Reduce a struct field
    pub(crate) fn reduce_struct_field(&mut self) -> Result<StructField, ParseError> {
        // Check if follows an offset specifier
        // TODO: Implement the offset specifier
        let offset = None;

        // Now read the field
        Ok(StructField {
            offset,
            field: self.reduce_struct_union_field()?,
        })
    }

    /// Reduce a struct type
    pub(crate) fn reduce_struct_ty(&mut self) -> Result<StructUnionTy, ParseError> {
        // Expect `struct`
        match self.slice() {
            [Tk::Struct, ..] => self.advance(1),
            _ => return Err(self.unexpected(&["struct"])),
        }

        // Check if packed
        let packed = match self.slice() {
            [Tk::PackedAttr, ..] => {
                self.advance(1);
                true
            }
            _ => false,
        };

        // Expect `{`
        match self.slice() {
            [Tk::LCurlyBrac, ..] => self.advance(1),
            _ => return Err(self.unexpected(&["{"])),
        }

        // Read the fields
        let mut fields = Vec::new();

        // Read the first one
        let fst = self.reduce_struct_field()?;
        fields.push(fst);

        // The remaining fields
        loop {
            match self.slice() {
                [Tk::Comma, Tk::RCurlyBrac, ..] => {
                    self.advance(2);
                    break;
                }
                [Tk::Comma, ..] => self.advance(1),
                [Tk::RCurlyBrac, ..] => {
                    self.advance(1);
                    break;
                }
                _ => return Err(self.unexpected(&[",", "}"])),
            }

            // Maybe read the field
            fields.push(self.reduce_struct_field()?);
        }

        Ok(StructUnionTy::Struct { packed, fields })
    }

    /// Reduce an union type
    pub(crate) fn reduce_union_ty(&mut self) -> Result<StructUnionTy, ParseError> {
        // Expect `union`
        match self.slice() {
            [Tk::Union, ..] => self.advance(1),
            _ => return Err(self.unexpected(&["union"])),
        }

        // Expect `{`
        match self.slice() {
            [Tk::LCurlyBrac, ..] => self.advance(1),
            _ => return Err(self.unexpected(&["{"])),
        }

        // Read the fields
        let mut fields = Vec::new();
        let fst = self.reduce_struct_union_field()?;
        fields.push(fst);

        loop {
            match self.slice() {
                [Tk::Comma, Tk::RCurlyBrac, ..] => {
                    self.advance(2);
                    break;
                }
                [Tk::Comma, ..] => self.advance(1),
                [Tk::RCurlyBrac, ..] => {
                    self.advance(1);
                    break;
                }
                _ => return Err(self.unexpected(&[",", "}"])),
            }

            fields.push(self.reduce_struct_union_field()?);
        }

        Ok(StructUnionTy::Union { fields })
    }

    /// Reduce a struct-union type
    pub(crate) fn reduce_struct_union_ty(&mut self) -> Result<StructUnionTy, ParseError> {
        match self.slice() {
            [Tk::Struct, ..] => self.reduce_struct_ty(),
            [Tk::Union, ..] => self.reduce_union_ty(),
            _ => Err(self.unexpected(&["struct", "union"])),
        }
    }

    /// Reduce a storage-class
    /// Its the actual value behind a type, see [`Ty`] and [`StorageClass`]
    pub(crate) fn reduce_storage_class(&mut self) -> Result<StorageClass, ParseError> {
        // Check if is a primitive
        if let Ok(prim) = self.reduce_primitive_ty() {
            return Ok(StorageClass::Primitive(prim));
        };

        match self.slice() {
            [Tk::Star, ..] => self.reduce_pointer_ty(),
            [Tk::Struct, ..] | [Tk::Union, ..] => {
                Ok(StorageClass::StructUnion(self.reduce_struct_union_ty()?))
            }
            _ => Err(self.unexpected(&[
                "primitive-type",
                "pointer-type",
                "struct-union-type",
                "tuple-type",
                "tagged-union-type",
                "slice-array-type",
                "function-type",
                "alias-type",
                "unwrapped-alias",
            ])),
        }
    }

    /// Reduce a type
    /// A type is a container like a struct, or an union. Or a pointer, or primitive types, et
    /// cetera, see the documentation of [`Ty`] for more information
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
            storage: stor.into(),
        })
    }

    /// Create a new instance of the parser
    /// example usage:
    /// ```rust
    /// use parser::ast::Parser;
    ///
    /// let data = r#"
    /// export fn main() void {
    ///     return 0;
    /// };
    /// "#;
    ///
    /// let parser = Parser::new(data);
    /// ```
    pub fn new(data: impl Into<&'a str>) -> Result<Self, ParseError> {
        let mut stream = Tk::lexer(data.into());

        let mut tks = Vec::new();
        let mut lns = Vec::new();
        let mut spans = Vec::new();

        let mut line = 1;
        // TODO: Keep only the comments right before a declaration
        while let Some(tk) = stream.next() {
            match tk {
                Ok(Tk::Nl) => line += 1,
                Ok(Tk::Comment(_)) => line += 1,
                Ok(to) => {
                    tks.push(to);
                    lns.push(line);
                    spans.push(stream.slice());
                }
                Err(_) => {
                    let sli = stream.slice();
                    return Err(ParseError::Lex(line, sli[..sli.len().min(3)].into()));
                }
            }
        }

        Ok(Self {
            spans,
            data: tks,
            lines: lns,
            idx: 0,
        })
    }
}
