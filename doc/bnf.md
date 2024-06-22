# BNF-like representation of the kulfon parser

## Program
parse_prog ::= parse_fun*
parse_fun ::= 'fn' FN_NAME '(' parse_arg_list ')' ('->' parse_type)? parse_scope
parse_arg_list ::= E | (ARG_NAME ':' parse_type) (',' parse_arg_list)*
parse_scope ::= '{' parse_expression* '}'
parse_type ::= TYPE_LITERAL

## Expressions
parse_expression ::= parse_equality
parse_equality ::= parse_comparison (('!=' | '==') parse_comparison)*
parse_comparison ::= parse_term (('>' | '>=' | '<' | '<=') parse_term)*
parse_term ::= parse_factor (('-' | '+') parse_factor)*
parse_factor ::= parse_unary (('/' | '*') parse_unary)*
parse_unary ::= ('!' | '-') parse_unary | parse_primary
parse_primary ::= parse_fn_call | NUMBER | STRING | CHAR | 'true' | 'false' | '(' parse_expression ')'

## Function calls
parse_fn_call ::= FN_NAME '(' parse_expr_list ')
parse_expr_list ::= E | (parse_expression) (',' parse_expression)*
