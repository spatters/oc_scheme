%token<int> INT
%token<bool> BOOL
%token<string> SYMBOL
%token LEFT_PAREN
%token RIGHT_PAREN
%token DOT
%token IF
%token DEFINE
%token ASSIGN
%token QUOTE
%token SINGLE_QUOTE
%token LAMBDA
%token EOF
(* %token<float> FLOAT *)
(* %token<string> STRING *)


%start <Types.expr option> prog
%%

prog:
  | EOF { None }
  | e = expr { Some e}

expr:
  | i = INT { Types.Atom (Types.Int i) }
  | b = BOOL { Types.Atom (Types.Bool b) }
  | LEFT_PAREN; IF; pred = expr; consq = expr; alt = expr; RIGHT_PAREN
    { Types.If (pred, consq, alt) }
  | LEFT_PAREN; DEFINE; var = symbol; value = expr; RIGHT_PAREN
    { Types.Define (Types.VarDef (var, value)) }
  | LEFT_PAREN; ASSIGN; var = symbol; value = expr; RIGHT_PAREN
    { Types.Assign (Types.VarDef (var, value)) }
  | LEFT_PAREN; QUOTE; quoted_expr = expr; RIGHT_PAREN
    { Types.Quote quoted_expr }
  | SINGLE_QUOTE; quoted_expr = expr
    { Types.Quote quoted_expr }
  | LEFT_PAREN; DEFINE; LEFT_PAREN;  name = symbol; args = symbol_list; RIGHT_PAREN; body = expr_list; RIGHT_PAREN
    { Types.Define (Types.FuncDef (name, args, body)) }
  | LEFT_PAREN; LAMBDA; LEFT_PAREN; args = symbol_list; RIGHT_PAREN; body = expr_list; RIGHT_PAREN
  { Types.Lambda (args, body) }
  | LEFT_PAREN; expr_list_ = expr_list ; DOT ; last_expr = expr ; RIGHT_PAREN
    { Types.dotted_list expr_list_ last_expr }
  | LEFT_PAREN; expr_list_ = expr_list ; RIGHT_PAREN
    { Types.scheme_list expr_list_ }
  | s = symbol {Types.Symbol s}
  (* | f = FLOAT { Types.Float f } *)
(*   | s = SYMBOL { Types.Symbol s } *)
  (* | s = STRING { Types.String s } *)
;

symbol: t = SYMBOL {  t };

expr_list:
  exprs = list(expr) {exprs};

symbol_list:
  symbols = list(symbol) {symbols};

