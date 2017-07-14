{
  open Parser
  exception SyntaxError of string
}

let digit = ['0'-'9']
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+

let int = '-'? digit+
let float = digit* frac? exp?

let white = [' ' '\t' '\n' '\r']+
(*let newline = '\r' | '\n' | "\r\n"*)

(*let symbol = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '-']*)
let symbol_chars = ['a'-'z' 'A'-'Z' '0'-'9' '_' '-' '?' '+' '*' '/' '%']
let symbol = symbol_chars+
let string = '"' [^ '"']* '"'

rule read =
  parse
  | white    { read lexbuf }
  | int      { INT (int_of_string (Lexing.lexeme lexbuf)) }
(*
  | float    { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | symbol    { SYMBOL (Lexing.lexeme lexbuf) }
  | string    { STRING (Lexing.lexeme lexbuf) }
*)
(*  | "\#t"   { TRUE }
  | "\#f"  { FALSE } *)
  | '('      { LEFT_PAREN }
  | ')'      { RIGHT_PAREN }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof      { EOF }
