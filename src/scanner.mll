{ open Parser }

let whitespace = [' ' '\t' '\r']
let alpha = ['a'-'z' 'A'-'Z']
let escape = '\\' ['\\' ''' '"' 'n' 'r' 't']
let ascii = ([' '-'!' '#'-'[' ']'-'~'])
let digit = ['0'-'9']
let id = alpha (alpha | digit )*
let string = '"' ((ascii | escape)* as s) '"'
let float = (digit+) ['.'] digit+
let int = digit+


rule token = parse
 whitespace     { token lexbuf } (* Whitespace *)
| "#"   { singleComment lexbuf } (* comments *)
| "##"  { multiComment lexbuf }
| '('   { LPAREN }  
| ')'   { RPAREN }  
| '['   { LBRACKET }  
| ']'   { RBRACKET }  
| ','   { COMMA }   
| "+"   { PLUS }    
| "-"   { MINUS }   
| "*"   { TIMES }   
| "/"   { DIVIDE }  
| '%'   { MOD }     
| "="   { ASSIGN }  
| "=="  { EQ }      
| "!="  { NEQ }     
| "<"   { LT }      
| "<="  { LEQ }     
| ">"   { GT }      
| ">="  { GEQ }     
| "and" { AND }     
| "or"  { OR }      
| "float" { FLOAT } 
| "False" { FALSE } 
| ":"   { COLON }   
| "+="  { PLUSEQ }     
| "/="  { DIVIDEEQ }     
| "%="  { MINUSEQ }
| "*="  { TIMESEQ }      
| "-="      { MODEQ }
| "int" { INT }     
| "dataframe" { DATAFRAME }
| "string" { STRING }  
| "True"    { TRUE }
| "in"  { IN }      
| "if"      { IF }
| "else"    { ELSE } 
| "elif"    { ELIF }
| "for"     { FOR }
| "while"   { WHILE }
| "return"  { RETURN }
| "break"   { BREAK }
| "continue" { CONTINUE }
| "not"     { NOT }
| "bool"    { BOOL }
| "None"    { NONE }
| "end"     { END }
| "def"     { FUNCTION }
| "\n"      { EOL }
| int as lxm { INT_LITERAL(int_of_string lxm) }
| float as lxm { FLOAT_LITERAL(float_of_string lxm) }
| string as lxm { STRING_LITERAL( lxm ) }
| id as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character" ^
                            Char.escapted char)) }

and comment = parse 
"##"    { token lexbuf }
| _     { comment lexbuf }
