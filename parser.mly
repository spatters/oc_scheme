%token<int> INT
%token<bool> BOOL


(* %token<float> FLOAT *)
%token<string> SYMBOL
(* %token<string> STRING *)
%token LEFT_PAREN
%token RIGHT_PAREN
%token IF
%token DEFINE
%token EOF


%start <Types.expr option> prog
%%

prog:
  | EOF { None }
  | e = expr { Some e}

expr:
  | i = INT { Types.Atom (Types.Int i) }
  | b = BOOL { Types.Atom (Types.Bool b) }
  (* | f = FLOAT { Scheme.Float f } *)
(*   | s = SYMBOL { Types.Symbol s } *)
  (* | s = STRING { Scheme.String s } *)
  | LEFT_PAREN; IF; pred = expr; consq = expr; alt = expr; RIGHT_PAREN
    { Types.If (pred, consq, alt) }
  | LEFT_PAREN; DEFINE; var = symbol; value = expr; RIGHT_PAREN
    { Types.Define (var, value) }
  | LEFT_PAREN; expr_list = process_list; RIGHT_PAREN
    { Types.List expr_list }
  | s = symbol {Types.Symbol s}
;

symbol: t = SYMBOL {  t };

process_list: expr_list = rev_process_list { List.rev expr_list };

rev_process_list:
  | (* Empty *) { [] }
  | expr_list = rev_process_list; v = expr { v :: expr_list }

