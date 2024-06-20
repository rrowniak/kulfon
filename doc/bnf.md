# BNF-like representation of the kulfon parser

parse_prog ::= parse_fun*
parse_fun ::= 'fn' FN_NAME '(' parse_arg_list ')' ('->' parse_type)? parse_scope
parse_arg_list ::= (ARG_NAME ':' parse_type)? (',' parse_arg_list)*
parse_scope ::= '{' parse_expression* '}'

