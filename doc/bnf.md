# BNF-like representation of the kulfon parser

## Program
```
parse_prog ::= parse_prog_statement*
parse_prog_statement ::= parse_fun | parse_var | parse_mut_var | parse_struct | parse_enum | parse_impl
```

## Function definition
```
parse_fun ::= 'fn' FN_NAME '(' parse_arg_list (',' '...')? ')' ('->' parse_type)? parse_scope
parse_arg_list ::= E | (ARG_NAME ':' parse_type) (',' ARG_NAME ':' parse_type)*
```

## Type parsing
```
parse_type ::= '&' parse_type | '&' 'mut' parse_type | '[' parse_type ']' | '[' parse_type ';' parse_expression ']' | parse_type_name 
// this is optimized out - recursice calls replaced with iterative approach
parse_typename ::= '::'? parse_type_name | parse_typename_with_generics 
parse_typename_with_generics ::= TYPE_LITERAL ('<' parse_type_list '>')?
parse_type_list ::= E | parse_type (',' parse_type)*
```

## Struct parsing
```
parse_struct ::= 'struct' parse_typename_with_generics '{' parse_struct_fields '}'
parse_struct_fields ::= E | (FIELD_NAME ':' parse_type) (',' FIELD_NAME ':' parse_type)*

```

## Enum parsing
```
parse_enum ::= 'enum' parse_typename_with_generics '{' parse_enum_fields* '}'
parse_enum_fields ::= E | (ENUM_FIELD_NAME | ENUM_FIELD_NAME '(' parse_type ')') (',' (ENUM_FIELD_NAME | ENUM_FIELD_NAME '(' parse_type')' ))
```

## Parse struct or enum impl

```
parse_impl ::= 'impl' STRUCT_ENUM_NAME '{' parse_fun* '}'
```

## Scope
```
// break and continue should be loop-specific. This is fine for now.
parse_scope ::= '{' ((parse_assign | parse_ctrl_flow | parse_var | parse_expression | 'break' | 'continue') ';')* '}'
```

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
