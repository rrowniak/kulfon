// ===================================================
// This file is part of the Kulfon compiler.
// Author: Rafał Równiak
// License: Read LICENSE file
// Created on: 23.07.2024
// ---------------------------------------------------
use crate::lang_def::{KfTokKind, KfToken};
use crate::type_system::EvaluatedType;

#[derive(Debug, Copy, Clone)]
pub struct TextPoint {
    pub line: usize,
    pub col: usize,
}

#[derive(Debug)]
pub enum MessageKind {
    Error,
    // Warning,
}

#[derive(Debug)]
pub struct CompileMessage {
    pub kind: MessageKind,
    pub msg: String,
    pub details: String,
    pub at: Option<TextPoint>,
}

pub type CompileMsgCol = Vec<CompileMessage>;

// Lexer errors
pub fn error_eof_unterminated_char() -> CompileMessage {
    CompileMessage {
        kind: MessageKind::Error,
        msg: "unterminated character literal".into(),
        details: "the character literal is not closed. Ensure that it is enclosed with matching single quotes ('). For example: 'a'.".into(),
        at: None,
    }
}

pub fn error_invalid_char(at: TextPoint) -> CompileMessage {
    CompileMessage {
        kind: MessageKind::Error,
        msg: "invalid character literal".into(),
        details: "the character literal format is invalid. Only ASCII characters or escape sequences like '\n' are allowed. Ensure your character literal is either a single ASCII character or a valid escape sequence enclosed in single quotes ('). For example: 'a' or '\n'.".into(),
        at: Some(at)
    }
}

pub fn error_eof_unterminated_string(at: TextPoint) -> CompileMessage {
    CompileMessage {
        kind: MessageKind::Error,
        msg: "unterminated string literal".into(),
        details: r#"the string literal is not closed. Ensure that it is enclosed with matching double quotes ("). For example: "This is a valid string"."#.into(),
        at: Some(at),
    }
}

pub fn error_non_ascii(at: TextPoint) -> CompileMessage {
    CompileMessage {
        kind: MessageKind::Error,
        msg: "non-ASCII character encountered".into(),
        details: "only ASCII characters are allowed in this context. Ensure that the character is within the ASCII range (0-127). For example, 'a' is valid, but 'ł' is not.".into(),
        at: Some(at),
    }
}

// Parser errors
pub fn error_eof_generic() -> CompileMessage {
    CompileMessage {
        kind: MessageKind::Error,
        msg: "unexpected end of file".into(),
        details: "the end of the file was reached unexpectedly. The parser was expecting additional input. Ensure that your code is complete and correctly formatted.".into(),
        at: None,
    }
}

pub fn error_arg_list_expected_coma_or_parenth(at: TextPoint) -> CompileMessage {
    CompileMessage {
        kind: MessageKind::Error,
        msg: "expected ',' or ')' in argument list".into(),
        details: "while parsing the list of arguments, the parser expected a comma (',') to separate arguments or a closing parenthesis (')') to end the list. Ensure that all arguments are correctly separated by commas and that the list is properly enclosed in parentheses. For example: function(arg1, arg2).".into(),
        at: Some(at),
    }
}

pub fn error_invalid_primary_expression(at: TextPoint) -> CompileMessage {
    CompileMessage {
        kind: MessageKind::Error,
        msg: "invalid primary terminal in expression".into(),
        details: "the expression contains an invalid primary terminal. Expected one of the following: true, false, an open parenthesis '(', a character literal, a string literal, or a literal like a function or variable name. Ensure that the deepest part of your expression is one of these valid types. For example: true, false, (expression), 'a', \"string\", variable, or functionName.".into(),
        at: Some(at)
    }
}

pub fn error_expr_list_expected_coma_or_parenth(at: TextPoint) -> CompileMessage {
    CompileMessage {
        kind: MessageKind::Error,
        msg: "expected ',' or ')' in expression list".into(),
        details: "while parsing the list of arguments, the parser expected a comma (',') to separate arguments or a closing parenthesis (')') to end the list. Ensure that all arguments are correctly separated by commas and that the list is properly enclosed in parentheses. For example: function(arg1, arg2).".into(),
        at: Some(at),
    }
}

pub fn error_expected_ctrl_flow(at: TextPoint) -> CompileMessage {
    CompileMessage {
        kind: MessageKind::Error,
        msg: "expected 'if', 'for', 'while' or 'loop' statements".into(),
        details: "the expression contains an invalid statement. Expected one of the following: 'if', 'for', 'while' or 'loop'".into(),
        at: Some(at)
    }
}

pub fn error_unexpected_token(expected: &KfTokKind, got: &KfToken) -> CompileMessage {
    CompileMessage {
        kind: MessageKind::Error,
        msg: "unexpected token".into(),
        details: format!("Expected the '{expected:?}' keyword but encountered a different token '{got:?}'. Ensure that your syntax is correct and that the '{expected:?}' keyword is used appropriately."),
        at: Some(got.at),
    }
}

pub fn error_expected_literal(got: &KfToken) -> CompileMessage {
    CompileMessage {
        kind: MessageKind::Error,
        msg: "unexpected token".into(),
        details: format!("Expected a literal (such as a number, function name, type name, or variable name) but encountered a different token '{got:?}'. Ensure that your expression includes a valid literal in the expected position. For example: 42, my_function, or variableName."),
        at: Some(got.at),
    }
}

pub fn error_inv_statement_glob() -> CompileMessage {
    CompileMessage {
        kind: MessageKind::Error,
        msg: "invalid statement in global scope".into(),
        details: "only function declarations and variable declarations are allowed in the global scope. Ensure that all other statements are placed within appropriate functions or variable declaration blocks. For example, 'let myVariable: i32 = 0;' or 'fn myFunction() { ... }' are valid in the global scope.".into(),
        at: None,
    }
}

pub fn error_inv_var_fn_name(at: TextPoint) -> CompileMessage {
    CompileMessage {
        kind: MessageKind::Error,
        msg: "invalid variable or function name".into(),
        details: "variable and function names must start with an ASCII alphabetic character (a-z or A-Z). Names starting with a digit or other characters are not allowed. Ensure that your names begin with a letter. For example: 'myVariable', 'functionName'.".into(),
        at: Some(at),
    }
}

pub fn error_fn_name_in_use(at: TextPoint) -> CompileMessage {
    CompileMessage {
        kind: MessageKind::Error,
        msg: "name already in use".into(),
        details: "the function name is already used for another function or global variable. Ensure that each function has a unique name that is not shared with any variable or other function. For example, if you have a global variable named 'myVariable', you cannot also have a function named 'myVariable'.".into(),
        at: Some(at),
    }
}

pub fn error_bool_type_expected(at: TextPoint, got: EvaluatedType) -> CompileMessage {
    CompileMessage {
        kind: MessageKind::Error,
        msg: "boolean type expected".into(),
        details: format!("an operator requires an argument that evaluates to a boolean type, but a different type was provided '{got:?}'. Ensure that the argument is a boolean expression. For example, true, false, or a condition like a > b."),
        at: Some(at),
    }
}

pub fn error_numeric_type_expected(at: TextPoint, got: EvaluatedType) -> CompileMessage {
    CompileMessage {
        kind: MessageKind::Error,
        msg: "numeric type expected".into(),
        details: format!("an operator requires an argument that evaluates to a numeric type (either floating point or integer), but a different type was provided '{got:?}'. Ensure that the argument is a valid numeric expression. For example, 42, 3.14, or a variable representing a number."),
        at: Some(at),
    }
}

pub fn error_assign_to_immutable(at: TextPoint) -> CompileMessage {
    CompileMessage {
        kind: MessageKind::Error,
        msg: "cannot assign to immutable variable".into(),
        details: "an attempt was made to assign a value to an immutable variable. Ensure that the variable is declared as mutable if you need to modify its value. For example, you can use keywords 'let mut' for mutable variables and 'let' for immutable variables.".into(),
        at: Some(at),
    }
}

pub fn error_undeclared_var(at: TextPoint) -> CompileMessage {
    CompileMessage {
        kind: MessageKind::Error,
        msg: "undeclared variable".into(),
        details: "the variable used has not been declared. Ensure that all variables are declared before they are used. For example, declare the variable with a statement like 'let myVariable;' before using it in your code.".into(),
        at: Some(at),
    }
}

pub fn error_condition_must_be_bool(
    at: TextPoint,
    expr: &str,
    got: EvaluatedType,
) -> CompileMessage {
    CompileMessage {
        kind: MessageKind::Error,
        msg: "condition expression must evaluate to boolean".into(),
        details: format!("the condition expression in the {expr} statement must evaluate to a boolean value, but it evaluated to '{got:?}'. Ensure that the expression is a valid boolean condition. For example, '{expr} a > b' or '{expr} isTrue'."),
        at: Some(at),
    }
}

pub fn error_fn_call_incorrect_no_args(
    at: TextPoint,
    supplied: usize,
    expected: usize,
) -> CompileMessage {
    CompileMessage {
        kind: MessageKind::Error,
        msg: "incorrect number of arguments in function call".into(),
        details: format!("the function call provided an incorrect number of arguments. The function expects {expected} arguments, but {supplied} were supplied. Ensure that the number of arguments matches the function’s parameter list. For example, if the function definition is 'fn myFunction(a: bool, b: i32)', it requires exactly 2 arguments."),
        at: Some(at),
    }
}

pub fn error_fn_call_arg_mismatch(
    at: TextPoint,
    got: EvaluatedType,
    exp: EvaluatedType,
) -> CompileMessage {
    CompileMessage {
        kind: MessageKind::Error,
        msg: "argument type mismatch in function call".into(),
        details: format!("the type of the provided argument '{got:?}' does not match the type expected by the function '{exp:?}'. Ensure that each argument matches the corresponding parameter type in the function definition. For example, if the function definition is 'myFunction(a: i32, b: f32)', the arguments should be an i32 and a f32, respectively."),
        at: Some(at)
    }
}

pub fn error_unrecognised_type(at: TextPoint, typename: &str) -> CompileMessage {
    CompileMessage {
        kind: MessageKind::Error,
        msg: "unrecognized type name".into(),
        details: format!("the type name provided '{typename}' is not recognized as a built-in type or a user-defined type. Ensure that the type name is correct and has been properly declared or defined. For example, built-in types might include bool, i32, or char, while user-defined types must be defined elsewhere in your code."),
        at: Some(at),
    }
}

pub fn error_undefined_function(at: TextPoint, name: &str) -> CompileMessage {
    CompileMessage {
        kind: MessageKind::Error,
        msg: "undefined function".into(),
        details: format!("the function '{name}' being called has not been defined. Ensure that the function is declared and defined before it is used. Verify the function name and its parameters to ensure they match the function’s declaration. For example, if you call 'myFunction()', make sure there is a corresponding 'fn myFunction() {{ ... }}' definition."),
        at: Some(at),
    }
}

pub fn error_type_mismatch_for_bin_op(
    at: TextPoint,
    left: EvaluatedType,
    right: EvaluatedType,
) -> CompileMessage {
    CompileMessage {
        kind: MessageKind::Error,
        msg: "type mismatch for binary operator".into(),
        details: format!("binary operators require both operands to be of the same type. The provided operands have different types, left is '{left:?}', right is '{right:?}', which is not allowed. Ensure that both sides of the operator are of the same type. For example, if you are using an operator like +, both operands should be either integers or floats, but not a mix of both."),
        at: Some(at)
    }
}

pub fn error_numeric_type_for_bin_op(
    at: TextPoint,
    left: EvaluatedType,
    right: EvaluatedType,
) -> CompileMessage {
    CompileMessage {
        kind: MessageKind::Error,
        msg: "numeric types required for binary operator".into(),
        details: format!("the binary operator requires both operands to be numeric types (either integer or float). One or both of the operands do not meet this requirement:left is '{left:?}', right is '{right:?}'. Ensure that both sides of the operator are numeric types. For example, for an operator like +, both operands should be either integers or floats, but not strings or other types."),
        at: Some(at)
    }
}

pub fn error_var_type_not_deduced(
    at: TextPoint,
    var_name: &str
) -> CompileMessage {
    CompileMessage {
        kind: MessageKind::Error,
        msg: "unable to deduce variable type".into(),
        details: format!("the type of the variable '{var_name}' cannot be deduced. Ensure that the variable is explicitly typed or initialized with a value from which the type can be inferred. For example, declare the variable with a type like 'let myVariable: i32 = 10;' or ensure it is assigned a value that clearly indicates its type."),
        at: Some(at)
    }
}

pub fn error_type_mismatch_var_assign(
    at: TextPoint,
    var_name: &str,
    type_a: &EvaluatedType,
    type_b: &EvaluatedType,
) -> CompileMessage {
    CompileMessage {
        kind: MessageKind::Error,
        msg: "type mismatch in variable assignment".into(),
        details: format!("the type of the variable '{var_name}' - {type_a:?} does not match the type of the assigned expression - {type_b:?}. Ensure that the expression's type is compatible with the variable's declared type. For example, if you declare 'let x: bool', you must assign it a boolean value like true or false, not a string or other type. Correct usage: 'let x: bool = true;'"),
        at: Some(at)
    }
}

