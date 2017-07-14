%token<int> INT


(* %token<float> FLOAT *)
%token<string> SYMBOL
(* %token<string> STRING *)
%token LEFT_PAREN
%token RIGHT_PAREN
%token EOF


%start <Types.expr option> prog
%%

prog:
  | EOF { None }
  | e = expr { Some e}

expr:
  | i = INT { Types.Atom (Types.Int i) }

  (* | f = FLOAT { Scheme.Float f } *)
  | s = SYMBOL { Types.Symbol s }
  (* | s = STRING { Scheme.String s } *)
  | LEFT_PAREN; expr_list = process_list; RIGHT_PAREN
    { Types.List expr_list }
;

process_list: expr_list = rev_process_list { List.rev expr_list };

rev_process_list:
  | (* Empty *) { [] }
  | expr_list = rev_process_list; v = expr { v :: expr_list }

