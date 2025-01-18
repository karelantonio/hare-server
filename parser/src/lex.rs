//! Lex definitions (tokens et cetera)

use logos::Logos;

/// A token, the lowest bit of information in the Syntax Tree
/// It represents relevan data, like a Lt (<) sign, or a string, or a number
#[derive(Logos, Debug, PartialEq, Eq)]
#[logos(skip "[ \r\t\x0c]+")]
pub enum Tk<'a> {
    /// A new-line character (linefeed, \n)
    #[token("\n")]
    Nl,

    /// A comment:
    #[regex("//[^\n]*")]
    Comment(&'a str),

    // An hexadecimal integer literal
    #[regex("0x[0-9a-fA-F]+")]
    HexNum(&'a str),

    /// An octadecimal integer literal
    #[regex("0o[0-7]+")]
    OctNum(&'a str),

    // An integer constant (decimal)
    #[regex("[0-9]+")]
    DecNum(&'a str),

    // Keywords (vim macros are the goat)
    #[token("abort")]
    Abort,

    #[token("align")]
    Align,

    #[token("alloc")]
    Alloc,

    #[token("append")]
    Append,

    #[token("as")]
    As,

    #[token("assert")]
    Assert,

    #[token("bool")]
    Bool,

    #[token("break")]
    Break,

    #[token("case")]
    Case,

    #[token("const")]
    Const,

    #[token("continue")]
    Continue,

    #[token("def")]
    Def,

    #[token("defer")]
    Defer,

    #[token("delete")]
    Delete,

    #[token("done")]
    Done,

    #[token("else")]
    Else,

    #[token("enum")]
    Enum,

    #[token("export")]
    Export,

    #[token("f32")]
    F32,

    #[token("f64")]
    F64,

    #[token("false")]
    False,

    #[token("fn")]
    Fn,

    #[token("for")]
    For,

    #[token("free")]
    Free,

    #[token("i16")]
    I16,

    #[token("i32")]
    I32,

    #[token("i64")]
    I64,

    #[token("i8")]
    I8,

    #[token("if")]
    If,

    #[token("insert")]
    Insert,

    #[token("int")]
    Int,

    #[token("is")]
    Is,

    #[token("len")]
    Len,

    #[token("let")]
    Let,

    #[token("match")]
    Match,

    #[token("never")]
    Never,

    #[token("null")]
    Null,

    #[token("nullable")]
    Nullable,

    #[token("offset")]
    Offset,

    #[token("opaque")]
    Opaque,

    #[token("return")]
    Return,

    #[token("rune")]
    Rune,

    #[token("size")]
    Size,

    #[token("static")]
    Static,

    #[token("str")]
    Str,

    #[token("struct")]
    Struct,

    #[token("switch")]
    Switch,

    #[token("true")]
    True,

    #[token("type")]
    Type,

    #[token("u16")]
    U16,

    #[token("u32")]
    U32,

    #[token("u64")]
    U64,

    #[token("u8")]
    U8,

    #[token("uint")]
    Uint,

    #[token("uintptr")]
    Uintptr,

    #[token("union")]
    Union,

    #[token("use")]
    Use,

    #[token("vaarg")]
    Vaarg,

    #[token("vaend")]
    Vaend,

    #[token("valist")]
    Valist,

    #[token("vastart")]
    Vastart,

    #[token("void")]
    Void,

    #[token("yield")]
    Yield,

    #[token("_", priority = 1)]
    Underscore,

    /// A name
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*")]
    Name(&'a str),

    // Attributes
    /// Run when finished
    #[token("@fini")]
    FiniAttr,

    /// Run when loaded
    #[token("@init")]
    InitAttr,

    /// The offset
    #[token("@offset")]
    OffsetAttr,

    /// Packed
    #[token("@packed")]
    PackedAttr,

    /// Override the symbol
    #[token("@symbol")]
    SymbolAttr,

    /// This is a test
    #[token("@test")]
    TestAttr,

    /// Something is thread local
    #[token("@threadlocal")]
    ThreadLocalAttr,

    /// An invalid attr
    #[regex("@[a-zA-Z_][a-zA-Z0-9_]*")]
    InvalidAttr(&'a str),

    // Operators:
    /// Distinct
    #[token("!=")]
    Neq,

    /// A negation
    #[token("!")]
    Excl,

    /// A percent assignment (modulo assignment)
    #[token("%=")]
    PercEq,

    /// A percent (modulo operator)
    #[token("%")]
    Perc,

    /// Logical and assignment
    #[token("&&=")]
    LAndEq,

    /// Bitwise And assignment
    #[token("&=")]
    BAndEq,

    /// Logical And
    LAnd,

    /// Bitwise And
    #[token("&")]
    BAnd,

    /// Par open
    #[token("(")]
    LPar,

    /// Par close
    #[token(")")]
    RPar,

    /// Mult assignment
    #[token("*=")]
    StarEq,

    /// Star operator (mult)
    #[token("*")]
    Star,

    /// Add assignment
    #[token("+=")]
    AddEq,

    /// Add
    #[token("+")]
    Add,

    /// Comma
    #[token(",")]
    Comma,

    /// Minus assignment
    #[token("-=")]
    SubEq,

    /// Minus
    #[token("-")]
    Sub,

    /// Three dots: ...
    #[token("...")]
    Dot3,

    /// Two dots: ..
    #[token("..")]
    Dot2,

    /// One dot
    #[token(".")]
    Dot,

    /// Div assignment
    #[token("/=")]
    DivEq,

    /// Division
    #[token("/")]
    Div,

    /// Double Colon ::
    #[token("::")]
    DColon,

    /// Colon :
    #[token(":")]
    Colon,

    /// Semicolon ;
    #[token(";")]
    Semicolon,

    /// LShift assignment
    #[token("<<=")]
    LShiftEq,

    /// LShift
    #[token("<<")]
    LShift,

    /// Lower or equal
    #[token("<=")]
    LEq,

    /// Lower than
    #[token("<")]
    Lt,

    /// Equals operator
    #[token("==")]
    Eq,

    /// Right arrow
    #[token("=>")]
    Arrow,

    /// Assign operator
    #[token("=")]
    Assign,

    /// Right shift assignment
    #[token(">>=")]
    RShiftEq,

    /// Right shift
    #[token(">>")]
    RShift,

    /// Greater than or equals
    #[token(">=")]
    Ge,

    /// Greater than
    #[token(">")]
    Gt,

    /// Question mark: ?
    #[token("?")]
    Question,

    /// Left square bracket
    #[token("[")]
    LBrac,

    /// Right square bracket
    #[token("]")]
    RBrac,

    /// Logical XOR assignment
    #[token("^^=")]
    LXorEq,

    /// Logical XOR
    #[token("^^")]
    LXor,

    /// Bitwise XOR assignment
    #[token("^=")]
    BXorEq,

    /// Bitwise XOR
    #[token("^")]
    BXor,

    /// Left Curly Bracket
    #[token("{")]
    LCurlyBrac,

    /// Right Curly Bracket
    #[token("}")]
    RCurlyBrac,

    /// Logical OR assignment
    #[token("||=")]
    LOrEq,

    /// Logical OR
    #[token("||")]
    LOr,

    /// Bitwise OR assigment
    #[token("|=")]
    BOrEq,

    /// Bitwise OR
    #[token("|")]
    BOr,

    /// Tilde character
    #[token("~")]
    Tilde,
}

