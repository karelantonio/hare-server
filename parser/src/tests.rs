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
