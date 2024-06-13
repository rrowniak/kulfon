use crate::bnf_parser;
use crate::lexer;
use crate::lang_def;

const BNF_GRAMMAR: &str = r#"
<program> ::= <fun_def>;
<fun_def> ::= "fn" <FUN_IDENTIFIER> "(" <fn_args> ")" <fn_ret> "{" <fn_body> "}";

"#;

pub fn parse(code: &str) -> Result<(), String> {
    let bnf = bnf_parser::parse(BNF_GRAMMAR)?;
    // tokenize code
    let (tokens, errors) = lexer::tokenize(&lang_def::Lang::new(), code);
    if errors.len() > 0 {
        let mut error = String::new();
        for e in errors {
            error.push_str(&e.msg);
            error += "\n";
        }
        return Err(error);
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_hello_word() {
        let kulfon_code = r#"
        fn main() {
            println("hello word");
        }
        "#;
        let result = parse(kulfon_code);
        assert!(result.is_ok());
    }
}
