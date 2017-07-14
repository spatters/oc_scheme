{
  open Parser
  exception SyntaxError of string
}

let digit = ['0'-'9']
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+

let int = '-'? digit+
let float = digit* frac? exp?

let bool = "#t" | "#f"

let white = [' ' '\t' '\n' '\r']+

let symbol_chars = ['a'-'z' 'A'-'Z' '0'-'9' '_' '-' '?' '+' '*' '/' '%' '>' '<' '=']
let symbol = symbol_chars+
let string = '"' [^ '"']* '"'


rule read =
  parse
  | white    { read lexbuf }
  | int      { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | "#t"   { BOOL(true) }
  | "#f"  { BOOL(false) } 
  | '('      { LEFT_PAREN }
  | ')'      { RIGHT_PAREN }
  | "if"      { IF }
  | "define"      { DEFINE }

  (* | float    { FLOAT (float_of_string (Lexing.lexeme lexbuf)) } *)
  | symbol    { SYMBOL (Lexing.lexeme lexbuf) }
  (* | string    { STRING (Lexing.lexeme lexbuf) } *)
  | eof      { EOF }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
