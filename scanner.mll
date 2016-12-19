{ open Parser }

let Exp = 'e'['+' '-']?['0'-'9']+ 

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Comments *)
| ';'      { SEMI }
| ','      { COMMA }
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| '['	   { LBRACKET }
| ']'	   { RBRACKET }	
| "[["	   { LLBRACKET }
| "]]"	   { RRBRACKET }
| '>'      { GT }
| '<'      { LT }
| '%'	   { MODULUS }
| "++"	   { PLUSONE }
| "--"	   { MINUSONE }
| "|"	   { VB }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '='      { ASSIGN }
| "sqrt"   { SQRT }
| "=="     { EQ }
| "!="     { NEQ }
| "<="     { LEQ }
| ">="     { GEQ }
| "&&"     { AND }
| "||"     { OR } 
| '!'      { NOT }
| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR }
| "while"  { WHILE }
| "return" { RETURN }
| "int"    { INT }
| "bool"   { BOOL }
| "void"   { VOID }
| "true"   { TRUE }
| "false"  { FALSE }
| "float"  { FLOAT }
| "complex"   { COMPLEX }
| "string" { STRING }
| "poly"   { POLY }
| "order"  { ORDER }
| "pass"   { PASS }
| "break"  { BREAK }
| ['0'-'9']+ as lxm { INTLIT(int_of_string lxm) }
| ('.'['0'-'9']+Exp? | ['0'-'9']+('.'['0'-'9']*Exp? | Exp ) ) as lxm { FLOATLIT(float_of_string lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| '"'[^'\n']*'"' as lxm { STRINGLIT(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }
