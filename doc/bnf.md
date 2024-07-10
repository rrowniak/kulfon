# BNF-like representation of the kulfon parser

## Program
```
parse_prog ::= parse_prog_statement*
parse_prog_statement ::= parse_fun | parse_var | parse_mut_var
```

## Function definition
```
parse_fun ::= 'fn' FN_NAME '(' parse_arg_list ')' ('->' parse_type)? parse_scope
parse_arg_list ::= E | (ARG_NAME ':' parse_type) (',' ARG_NAME ':' parse_type)*
parse_type ::= TYPE_LITERAL
```

## Scope
// break and continue should be loop-specific. This is fine for now.
parse_scope ::= '{' ((parse_assign | parse_ctrl_flow | parse_var | parse_expression | 'break' | 'continue') ';')* '}'

## Expression
```
parse_expression ::= parse_equality
parse_equality ::= parse_comparison (('!=' | '==') parse_comparison)*
parse_comparison ::= parse_term (('>' | '>=' | '<' | '<=') parse_term)*
parse_term ::= parse_factor (('-' | '+') parse_factor)*
parse_factor ::= parse_unary (('/' | '*') parse_unary)*
parse_unary ::= ('!' | '-') parse_unary | parse_primary
parse_primary ::= parse_fn_call | NUMBER | STRING | CHAR | 'true' | 'false' | '(' parse_expression ')'
```

## Function call
```
parse_fn_call ::= FN_NAME '(' parse_expr_list ')
parse_expr_list ::= E | (parse_expression) (',' parse_expression)*
```

## Control flow
```
parse_ctrl_flow ::= parse_if | parse_for | parse_while | parse_loop
```
### Selection statements
```
parse_if ::= 'if' parse_expression parse_scope ('else' 'if' parse_expression parse_scope)* ('else' parse_expression parse_scope)?
```
### Iteration statements
```
parse_for ::= 'for' VAR_NAME 'in' parse_expression parse_scope
parse_while ::= 'while' parse_expression parse_scope
parse_loop ::= 'loop' parse_scope
```

## Variable definition
```
parse_var ::= parse_mut_var | parse_const_var
parse_const_var ::= 'let' VAR_NAME (':' parse_type)? ('=' parse_expression)? ';'
parse_mut_var ::= 'let' 'mut' VAR_NAME (':' parse_type)? ('=' parse_expression)? ';'
```

## Assignment
```
parse_assign ::= VAR_NAME '=' parse_expression ';'
```
