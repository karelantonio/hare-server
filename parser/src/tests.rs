use crate::ast::*;
use crate::lex::Tk;
use logos::Logos;

#[test]
fn test_lex_dec_numbers() {
    assert_eq!(
        Tk::lexer("123")
            .next()
            .expect("Expeting exactly one token")
            .expect("Should be no errors here"),
        Tk::DecNum("123")
    );
    assert_eq!(
        Tk::lexer("0")
            .next()
            .expect("Expeting exactly one token")
            .expect("Should be no errors here"),
        Tk::DecNum("0")
    );
    assert_eq!(
        Tk::lexer("1010")
            .next()
            .expect("Expeting exactly one token")
            .expect("Should be no errors here"),
        Tk::DecNum("1010")
    );
    assert_eq!(
        Tk::lexer("999999999999999")
            .next()
            .expect("Expeting exactly one token")
            .expect("Should be no errors here"),
        Tk::DecNum("999999999999999")
    );
    assert_eq!(
        Tk::lexer("0999999999999999")
            .next()
            .expect("Expeting exactly one token")
            .expect("Should be no errors here"),
        Tk::DecNum("0999999999999999")
    );
}

/// Test lexing a basic program
#[test]
fn test_lex_basic_main() {
    let data = r#"fn main() void = {
    return 0;
};
"#;
    let exp = [
        Tk::Fn,
        Tk::Name("main"),
        Tk::LPar,
        Tk::RPar,
        Tk::Void,
        Tk::Assign,
        Tk::LCurlyBrac,
        Tk::Nl,
        Tk::Return,
        Tk::DecNum("0"),
        Tk::Semicolon,
        Tk::Nl,
        Tk::RCurlyBrac,
        Tk::Semicolon,
        Tk::Nl,
    ];

    // Lex the file and dump
    let mut tk = Tk::lexer(data);
    while let Some(Ok(tk)) = tk.next() {
        println!("  {tk:?}");
    }

    let tk = Tk::lexer(data)
        .into_iter()
        .collect::<Result<Vec<_>, ()>>()
        .expect("Should not be any error while lexing");

    assert_eq!(&tk, &exp);
}

/// Test parse a basic type
#[test]
fn test_parse_tys() {
    let expr = r#"i32"#;
    let mut parser = Parser::new(expr).expect("Should not be any lex error");
    let res = parser.reduce_ty();
    println!("Expression: {expr:?} and result: {res:?}");
    assert_eq!(
        Ok(Ty {
            error: false,
            constant: false,
            storage: StorageClass::Primitive(PrimitiveTy::Integer(IntegerTy::I32)).into(),
        }),
        res
    );
    let expr = r#"f64"#;
    let mut parser = Parser::new(expr).expect("Should not be any lex error");
    let res = parser.reduce_ty();
    println!("Expression: {expr:?} and result: {res:?}");
    assert_eq!(
        Ok(Ty {
            error: false,
            constant: false,
            storage: StorageClass::Primitive(PrimitiveTy::Float(FloatTy::F64)).into(),
        }),
        res
    );
}

/// Test parse a little more complex type
#[test]
fn test_parse_tys_little_more_complex() {
    let expr = r#"const uintptr"#;
    let mut parser = Parser::new(expr).expect("Should not be any lex error");
    let res = parser.reduce_ty();
    println!("Expression: {expr:?} and result: {res:?}");
    assert_eq!(
        Ok(Ty {
            error: false,
            constant: true,
            storage: StorageClass::Primitive(PrimitiveTy::Integer(IntegerTy::UIntPtr)).into(),
        }),
        res
    );
    let expr = r#"const !uintptr"#;
    let mut parser = Parser::new(expr).expect("Should not be any lex error");
    let res = parser.reduce_ty();
    println!("Expression: {expr:?} and result: {res:?}");
    assert_eq!(
        Ok(Ty {
            error: true,
            constant: true,
            storage: StorageClass::Primitive(PrimitiveTy::Integer(IntegerTy::UIntPtr)).into(),
        }),
        res
    );
    let expr = r#"const *const !int"#;
    let mut parser = Parser::new(expr).expect("Should not be any lex error");
    let res = parser.reduce_ty();
    println!("Expression: {expr:?} and result: {res:?}");
    assert_eq!(
        Ok(Ty {
            error: false,
            constant: true,
            storage: StorageClass::Pointer {
                nullable: false,
                ty: Ty {
                    error: true,
                    constant: true,
                    storage: StorageClass::Primitive(PrimitiveTy::Integer(IntegerTy::Int)).into()
                }
            }
            .into(),
        }),
        res
    );
}

/// Test a complex type declaration
#[test]
fn test_parse_ty_complex() {
    let expr = r#"
    *struct{
        name: union{
            a: i32
        },
        age: f64,
        nameagain: str,
        lastname: struct{
            z: *u32,
        }
    }  "#;

    let mut parser = Parser::new(expr).unwrap();
    println!("{parser:?}");
    let ty = parser.reduce_ty().expect("Should not error parsing");
    println!("{ty:?}");
}

/// Test parse a simple expresion
#[test]
fn test_parse_basic_expr() {
    let expr = r#"1+1"#;
    let parser = Parser::new(expr);
    println!("{parser:?}");
}
