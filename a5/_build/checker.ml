open Types
open Stdlib.Result
open Ast_factory

exception TypeError of string

type typ =
  | TUnit
  | TInt
  | TBool
  | TString
  | THandle  of typ
  | TRef     of typ
  | TPromise of typ
  | TList    of typ
  | TProd    of typ * typ
  | TFun     of typ * typ
  | TBot

type pre_error =
  | UnboundVariable   of string
  | ExpectedFound     of typ * typ
  | FstEqArg          of typ
  | HandleNotConcrete

type error = pre_error info

let sprintf = Printf.sprintf

let rec str_of_typ = function
  | TUnit -> "unit"
  | TInt -> "int"
  | TBool -> "bool"
  | TString -> "string"
  | THandle (TProd (x, y)) -> sprintf "(%s) handle" (str_of_typ (TProd (x, y)))
  | THandle (TFun (x, y)) -> sprintf "(%s) handle" (str_of_typ (TFun (x, y)))
  | THandle t -> sprintf "%s handle" (str_of_typ t)
  | TRef (TProd (x, y)) -> sprintf "(%s) ref" (str_of_typ (TProd (x, y)))
  | TRef (TFun (x, y)) -> sprintf "(%s) ref" (str_of_typ (TFun (x, y)))
  | TRef t -> sprintf "%s ref" (str_of_typ t)
  | TPromise (TProd (x, y)) ->
    sprintf "(%s) promise" (str_of_typ (TProd (x, y)))
  | TPromise (TFun (x, y)) -> sprintf "(%s) promise" (str_of_typ (TFun (x, y)))
  | TPromise t -> sprintf "%s promise" (str_of_typ t)
  | TList (TProd (x, y)) -> sprintf "(%s) list" (str_of_typ (TProd (x, y)))
  | TList (TFun (x, y)) -> sprintf "(%s) list" (str_of_typ (TFun (x, y)))
  | TList t -> sprintf "%s list" (str_of_typ t)
  | TProd (TProd (w, x), TProd (y, z)) ->
    sprintf "(%s) * (%s)"
      (str_of_typ (TProd (w, x)))
      (str_of_typ (TProd (y, z)))
  | TProd (TProd (x, y), r) ->
    sprintf "(%s) * %s" (str_of_typ (TProd (x, y))) (str_of_typ r)
  | TProd (l, TProd (x, y)) ->
    sprintf "%s * (%s)" (str_of_typ l) (str_of_typ (TProd (x, y)))
  | TProd (l, r) -> sprintf "%s * %s" (str_of_typ l) (str_of_typ r)
  | TFun (TFun (x, y), r) ->
    sprintf "(%s) -> %s" (str_of_typ (TFun (x, y))) (str_of_typ r)
  | TFun (i, r) -> sprintf "%s -> %s" (str_of_typ i) (str_of_typ r)
  | TBot -> "'a"

let str_of_info (i : pre_info) : string =
  let lstart = string_of_int i.start_lin in
  let lend = string_of_int i.end_lin in
  let cstart = string_of_int i.start_col in
  let cend = string_of_int i.end_col in
  sprintf "error in %s at line%s, character%s\n" i.filename
    (if lstart = lend then " " ^ lstart else sprintf "s %s-%s" lstart lend)
    (if cstart = cend then " " ^ cstart else sprintf "s %s-%s" cstart cend)

let unbound_msg (i : pre_info) (v : string) : string =
  sprintf "%sError: Unbound value %s" (str_of_info i) v

let typing_msg (i : pre_info) (exp : typ) (fnd : typ) : string =
  sprintf
    "%sError: This expression has type %s, but an expression was expected of \
     type %s\n"
    (str_of_info i) (str_of_typ fnd) (str_of_typ exp)

let polyeq_msg (i : pre_info) (t : typ) : string =
  sprintf
    "%sError: This expression has type %s, but an expression was expected of \
     type string, int, or bool\n"
    (str_of_info i) (str_of_typ t)

let self_msg (i : pre_info) : string =
  str_of_info i ^ "Error: Unable to infer type of handle\n"
  ^ "Hint: trying putting the handle in an intermediary variable with a type \
     annotation"

let str_of_error (i, e) =
  match e with
  | UnboundVariable v -> unbound_msg i v
  | ExpectedFound (expected, found) -> typing_msg i expected found
  | FstEqArg t -> polyeq_msg i t
  | HandleNotConcrete -> self_msg i

let ( >>= ) = bind

let rec typ_of_gtyp (t : gtyp info) : typ =
  match snd t with
  | GUnit          -> TUnit
  | GInt           -> TInt
  | GBool          -> TBool
  | GString        -> TString
  | GHandle t      -> THandle (typ_of_gtyp t)
  | GRef t         -> TRef (typ_of_gtyp t)
  | GPromise t     -> TPromise (typ_of_gtyp t)
  | GList t        -> TList (typ_of_gtyp t)
  | GProd (t1, t2) -> TProd (typ_of_gtyp t1, typ_of_gtyp t2)
  | GFun (t1, t2)  -> TFun (typ_of_gtyp t1, typ_of_gtyp t2)
  | GBot           -> TBot

module Context = struct
  type t = (string info * typ) list

  let update (v : string info) (t : typ) (ctxt : t) : t = (v, t) :: ctxt

  let prepend = (@)

  let rec find v ctxt =
    match ctxt with
    | []          -> error (fst v, UnboundVariable (snd v))
    | (a, b) :: t -> if snd a = snd v then ok b else find v t

  let empty = [
    ((Types.dummy_info, "print"), TFun (TBot, TUnit));
    ((Types.dummy_info, "println"), TFun (TBot, TUnit));
  ]
end

let rec ( <~ ) (t1 : typ) (t2 : typ) : bool =
  match (t1, t2) with
  | TBot, _ | TUnit, TUnit | TInt, TInt | TBool, TBool | TString, TString ->
    true
  | THandle t1', THandle t2' -> t1' <~ t2'
  | TRef t1', TRef t2' -> t1' <~ t2'
  | TPromise t1', TPromise t2' -> t1' <~ t2'
  | TList t1', TList t2' -> t1' <~ t2'
  | TProd (t1', t1''), TProd (t2', t2'') -> t1' <~ t2' && t1'' <~ t2''
  | TFun (t1', t1''), TFun (t2', t2'') -> t1' <~ t2' && t1'' <~ t2''
  | _ -> false

let max_typ (t1 : typ) (t2 : typ) (msg : error) : (typ, error) result =
  if t1 <~ t2 then ok t2 else if t2 <~ t1 then ok t1 else error msg

let rec type_of_pattern (p : pat info) : (typ * Ast.pat, error) result =
  match snd p with
  | PUnit          -> ok (TUnit, make_unit_pattern ())
  | PWild          -> ok (TBot, make_wild_pattern ())
  | PBool (_, b)   -> ok (TBool, make_bool_pattern b)
  | PInt (_, n)    -> ok (TInt, make_int_pattern n)
  | PString (_, s) -> ok (TString, make_string_pattern s)
  | PVar (_, x)    -> ok (TBot, make_var_pattern x)
  | PPair (p1, p2) -> type_of_pair_pattern p1 p2
  | PNil           -> ok (TList TBot, make_nil_pattern ())
  | PCons (p1, p2) -> type_of_cons_pattern p1 p2

and type_of_pair_pattern (p1 : pat info) (p2 : pat info) :
  (typ * Ast.pat, error) result =
  type_of_pattern p1 >>= fun (t1, p1') ->
  type_of_pattern p2 >>= fun (t2, p2') ->
  ok (TProd (t1, t2), make_pair_pattern p1' p2')

and type_of_cons_pattern (p1 : pat info) (p2 : pat info) :
  (typ * Ast.pat, error) result =
  type_of_pattern p1 >>= fun (t1, p1') ->
  type_of_pattern p2 >>= fun (t2, p2') ->
  match t2 with
  | TList t2' ->
    max_typ t1 t2' (fst p1, ExpectedFound (t2', t1)) >>= fun tl ->
    ok (TList tl, make_cons_pattern p1' p2')
  | TBot      -> ok (TList t1, make_cons_pattern p1' p2')
  | _         -> error (fst p2, ExpectedFound (TList t1, t2))

let rec check_pattern (p : pat info) (t : typ) :
  (Context.t * Ast.pat, error) result =
  type_of_pattern p >>= fun (pt, p') ->
  if not (pt <~ t) then error (fst p, ExpectedFound (t, pt))
  else
    match (snd p, t) with
    | PUnit, TUnit | PWild, _ | PBool _, TBool | PInt _, TInt | PString _, TString | PNil, _ ->
      ok (Context.empty, p')
    | PVar x, _ -> ok ([ (x, t) ], p')
    | PPair (p1, p2), TProd (t1, t2) ->
      check_pattern p2 t2 >>= fun (ctxt1, _) ->
      check_pattern p1 t1 >>= fun (ctxt2, _) -> ok (ctxt2 @ ctxt1, p')
    | PCons (p1, p2), TList t' ->
      check_pattern p2 t >>= fun (ctxt1, _) ->
      check_pattern p1 t' >>= fun (ctxt2, _) -> ok (ctxt2 @ ctxt1, p')
    | _ -> error (fst p, ExpectedFound (t, pt))

let rec check_prog (ctxt : Context.t) (prog : prog) :
  (Context.t * Ast.prog, error) result =
  let ds = snd prog in
  let h acc d =
    acc >>= fun (accctxt, ds) ->
    check_defn accctxt (snd d) >>= fun (accctxt', d') -> ok (accctxt', d' :: ds)
  in
  List.fold_left h (ok (ctxt, [])) ds >>= fun (ctxt', ds') ->
  ok (ctxt', List.rev ds')

and check_defn (ctxt : Context.t) (d : defn) :
  (Context.t * Ast.defn, error) result =
  match d with
  | DLet ((_, (p, t)), e)    -> check_let_defn ctxt p t e
  | DLetRec ((_, (p, t)), e) -> check_let_rec_defn ctxt p t e

and check_let_defn (ctxt : Context.t) (p : pat info) (t : gtyp info)
    (e : expr info) : (Context.t * Ast.defn, error) result =
  let t = typ_of_gtyp t in
  check_pattern p t >>= fun (ctxt', p') ->
  check_expr ctxt (snd e) >>= fun (t', e') ->
  if not (t' <~ t || t <~ t') then error (fst e, ExpectedFound (t, t'))
  else ok (ctxt' @ ctxt, make_let_defn p' e')

and check_let_rec_defn (ctxt : Context.t) (p : pat info) (t : gtyp info)
    (e : expr info) : (Context.t * Ast.defn, error) result =
  let t = typ_of_gtyp t in
  check_pattern p t >>= fun (ctxt', p') ->
  check_expr (ctxt' @ ctxt) (snd e) >>= fun (t', e') ->
  if not (t' <~ t || t <~ t') then error (fst e, ExpectedFound (t, t'))
  else ok (ctxt' @ ctxt, make_let_rec_defn p' e')

and check_expr (ctxt : Context.t) (e : expr) : (typ * Ast.expr, error) result =
  match e with
  | Unit -> ok (TUnit, make_unit ())
  | Bool (_, b) -> ok (TBool, make_bool b)
  | Pair (e1, e2) -> check_pair ctxt e1 e2
  | Int (_, n) -> ok (TInt, make_int n)
  | String (_, s) -> ok (TString, make_string s)
  | Self -> ok (THandle TBot, make_self ())
  | Var v -> check_var ctxt v
  | Fun ((_, (p, t)), e) -> check_fun ctxt p t e
  | App (e1, e2) -> check_app ctxt e1 e2
  | Let ((_, (p, t)), e1, e2) -> check_let ctxt p t e1 e2
  | LetRec ((_, (p, t)), e1, e2) -> check_letrec ctxt p t e1 e2
  | Nil -> ok (TList TBot, make_nil ())
  | Bop (b, e1, e2) -> check_bop ctxt b e1 e2
  | Uop (u, e) -> check_uop ctxt u e
  | Seq (e1, e2) -> check_seq ctxt e1 e2
  | IfElse (e1, e2, e3) -> check_if ctxt e1 e2 e3
  | Match (e, cases) -> check_match ctxt e cases
  | Await ((_, (p, t)), e1, e2) -> check_await ctxt p t e1 e2
  | Spawn (e1, e2) -> check_spawn ctxt e1 e2
  | Send (e1, e2) -> check_send ctxt e1 e2
  | Recv e -> check_recv ctxt e
  | Return e -> check_return ctxt e

and check_pair (ctxt : Context.t) (e1 : expr info) (e2 : expr info) :
  (typ * Ast.expr, error) result =
  check_expr ctxt (snd e1) >>= fun (t1, e1') ->
  check_expr ctxt (snd e2) >>= fun (t2, e2') ->
  ok (TProd (t1, t2), make_pair e1' e2')

and check_var (ctxt : Context.t) (v : string info) :
  (typ * Ast.expr, error) result =
  Context.find v ctxt >>= fun t1 -> ok (t1, make_var (snd v))

and check_fun (ctxt : Context.t) (p : pat info) (t : gtyp info) (e : expr info)
  : (typ * Ast.expr, error) result =
  check_pattern p (typ_of_gtyp t) >>= fun (ctxt', p') ->
  check_expr (ctxt' @ ctxt) (snd e) >>= fun (t', e') ->
  ok (TFun (typ_of_gtyp t, t'), make_fun p' e')

and check_app (ctxt : Context.t) (e1 : expr info) (e2 : expr info) :
  (typ * Ast.expr, error) result =
  check_expr ctxt (snd e1) >>= fun (t, e1') ->
  check_expr ctxt (snd e2) >>= fun (t', e2') ->
  match t with
  | TFun (t1, t2) ->
    max_typ t1 t' (fst e2, ExpectedFound (t1, t')) >>= fun _ ->
    ok (t2, make_app e1' e2')
  | _             -> error (fst e1, ExpectedFound (TFun (t', TBot), t))

and check_let (ctxt : Context.t) (p : pat info) (t : gtyp info) (e1 : expr info)
    (e2 : expr info) : (typ * Ast.expr, error) result =
  let t = typ_of_gtyp t in
  check_pattern p t >>= fun (ctxt', p') ->
  check_expr ctxt (snd e1) >>= fun (t1, e1') ->
  if not (t1 <~ t || t <~ t1) then error (fst e1, ExpectedFound (t, t1))
  else
    check_expr (ctxt' @ ctxt) (snd e2) >>= fun (t2, e2') ->
    ok (t2, make_let p' e1' e2')

and check_letrec (ctxt : Context.t) (p : pat info) (t : gtyp info)
    (e1 : expr info) (e2 : expr info) : (typ * Ast.expr, error) result =
  let t = typ_of_gtyp t in
  check_pattern p t >>= fun (ctxt', p') ->
  check_expr (ctxt' @ ctxt) (snd e1) >>= fun (t1, e1') ->
  if not (t1 <~ t || t <~ t1) then error (fst e1, ExpectedFound (t, t1))
  else
    check_expr (ctxt' @ ctxt) (snd e2) >>= fun (t2, e2') ->
    ok (t2, make_let_rec p' e1' e2')

and check_bop (ctxt : Context.t) (b : bop info) (e1 : expr info)
    (e2 : expr info) : (typ * Ast.expr, error) result =
  let expected (input : typ) ~(ret : typ) : (typ * Ast.expr, error) result =
    check_expr ctxt (snd e1) >>= fun (t1, e1') ->
    check_expr ctxt (snd e2) >>= fun (t2, e2') ->
    if not (t1 <~ input) then error (fst e1, ExpectedFound (input, t1))
    else if not (t2 <~ input) then error (fst e2, ExpectedFound (input, t2))
    else ok (ret, make_bop (snd b) e1' e2')
  in
  match snd b with
  | Add | Sub | Mul | Div | Mod -> expected TInt ~ret:TInt
  | And | Or -> expected TBool ~ret:TBool
  | Lt | Le | Gt | Ge -> expected TInt ~ret:TBool
  | Eq -> check_eq ctxt true e1 e2
  | Ne -> check_eq ctxt false e1 e2
  | Cat -> expected TString ~ret:TString
  | Pipe -> check_app ctxt e2 e1
  | Cons -> check_cons ctxt e1 e2
  | Assign -> check_assign ctxt e1 e2
  | Bind -> check_bind ctxt e1 e2

and check_eq (ctxt : Context.t) (eq : bool) (e1 : expr info) (e2 : expr info) :
  (typ * Ast.expr, error) result =
  check_expr ctxt (snd e1) >>= fun (t1, e1') ->
  check_expr ctxt (snd e2) >>= fun (t2, e2') ->
  let both_string = t1 <~ TString && t2 <~ TString in
  let both_bool = t1 <~ TBool && t2 <~ TBool in
  let both_int = t1 <~ TInt && t2 <~ TInt in
  if both_string || both_int || both_bool
  then ok (TBool, make_bop (if eq then Eq else Ne) e1' e2')
  else
    match t1 with
    | TInt | TString | TBool -> error (fst e2, ExpectedFound (t1, t2))
    | _                      -> error (fst e1, FstEqArg t1)

and check_cons (ctxt : Context.t) (e1 : expr info) (e2 : expr info) :
  (typ * Ast.expr, error) result =
  check_expr ctxt (snd e1) >>= fun (t1, e1') ->
  check_expr ctxt (snd e2) >>= fun (t2, e2') ->
  match t2 with
  | TList t2' ->
    max_typ t1 t2' (fst e1, ExpectedFound (t2', t1)) >>= fun tl ->
    ok (TList tl, make_bop Cons e1' e2')
  | TBot      -> ok (TList t1, make_bop Cons e1' e2')
  | _         -> error (fst e2, ExpectedFound (TList t1, t2))

and check_assign (ctxt : Context.t) (e1 : expr info) (e2 : expr info) :
  (typ * Ast.expr, error) result =
  check_expr ctxt (snd e1) >>= fun (t1, e1') ->
  check_expr ctxt (snd e2) >>= fun (t2, e2') ->
  match t1 with
  | TRef t1' ->
    max_typ t1' t2 (fst e2, ExpectedFound (t1', t2)) >>= fun _ ->
    ok (TUnit, make_bop Assign e1' e2')
  | TBot     -> ok (TUnit, make_bop Assign e1' e2')
  | _        -> error (fst e1, ExpectedFound (TRef t2, t1))

and check_bind (ctxt : Context.t) (e1 : expr info)
    (e2 : expr info) : (typ * Ast.expr, error) result =
  check_expr ctxt (snd e1) >>= fun (t1, e1') ->
  check_expr ctxt (snd e2) >>= fun (t2, e2') ->
  match t1, t2 with 
  | TPromise t1', TFun (t2a, TPromise t2r) -> 
    if not (t1' <~ t2a) then error (fst e1, ExpectedFound (TPromise t2a, t1))
    else ok (TPromise t2r, make_bop Bind e1' e2')
  | TPromise t1', _ -> 
    error (fst e2, ExpectedFound (TFun (t1', TPromise TBot), t2))
  | _, _ -> error (fst e1, ExpectedFound (TPromise TBot, t1 ))

and check_uop (ctxt : Context.t) (u : uop info) (e : expr info) :
  (typ * Ast.expr, error) result =
  let expected (input : typ) ~(ret : typ) : (typ * Ast.expr, error) result =
    check_expr ctxt (snd e) >>= fun (t, e') ->
    if not (t <~ input) then error (fst e, ExpectedFound (input, t))
    else ok (ret, make_uop (snd u) e')
  in
  match snd u with
  | Neg   -> expected TInt ~ret:TInt
  | Not   -> expected TBool ~ret:TBool
  | Ref   -> check_ref ctxt e
  | Deref -> check_deref ctxt e

and check_ref (ctxt : Context.t) (e : expr info) :
  (typ * Ast.expr, error) result =
  check_expr ctxt (snd e) >>= fun (t, e') -> ok (TRef t, make_uop Ref e')

and check_deref (ctxt : Context.t) (e : expr info) :
  (typ * Ast.expr, error) result =
  check_expr ctxt (snd e) >>= fun (t, e') ->
  match t with
  | TRef t' -> ok (t', make_uop Deref e')
  | TBot    -> ok (TBot, make_uop Deref e')
  | _       -> error (fst e, ExpectedFound (TRef TBot, t))

and check_seq (ctxt : Context.t) (e1 : expr info) (e2 : expr info) :
  (typ * Ast.expr, error) result =
  check_expr ctxt (snd e1) >>= fun (t1, e1') ->
  if not (t1 <~ TUnit) then error (fst e1, ExpectedFound (TUnit, t1))
  else check_expr ctxt (snd e2) >>= fun (t2, e2') -> ok (t2, make_seq e1' e2')

and check_if (ctxt : Context.t) (e1 : expr info) (e2 : expr info)
    (e3 : expr info) : (typ * Ast.expr, error) result =
  check_expr ctxt (snd e1) >>= fun (t1, e1') ->
  if not (t1 <~ TBool) then error (fst e1, ExpectedFound (TBool, t1))
  else
    check_expr ctxt (snd e2) >>= fun (t2, e2') ->
    check_expr ctxt (snd e3) >>= fun (t3, e3') ->
    max_typ t2 t3 (fst e3, ExpectedFound (t2, t3)) >>= fun ft ->
    ok (ft, make_ifelse e1' e2' e3')

and check_match (ctxt : Context.t) (e : expr info)
    (cases : (pat info * expr info) list) : (typ * Ast.expr, error) result =
  check_expr ctxt (snd e) >>= fun (t, e') ->
  let h acct (pi, (i, ei)) =
    acct >>= fun (acct, acccs) ->
    check_pattern pi t >>= fun (ctxt', p') ->
    check_expr (ctxt' @ ctxt) ei >>= fun (ti, e') ->
    max_typ acct ti (i, ExpectedFound (acct, ti)) >>= fun nt ->
    ok (nt, (p', e') :: acccs)
  in
  List.fold_left h (ok (TBot, [])) cases >>= fun (ft, cs) ->
  ok (ft, make_match e' (List.rev cs))

and check_await (ctxt : Context.t) (p : pat info) (t : gtyp info)
    (e1 : expr info) (e2 : expr info) : (typ * Ast.expr, error) result =
  let t = typ_of_gtyp t in
  check_expr ctxt (snd e1) >>= fun (t1, e1') ->
  check_pattern p t >>= fun (ctxt', p') ->
  check_expr (ctxt' @ ctxt) (snd e2) >>= fun (t2, e2') ->
  match t1, t2 with
  | TPromise t1', TPromise t2' ->
    if t1' <~ t then ok (TPromise t2', make_await p' e1' e2')
    else error (fst e1, ExpectedFound (t1', t))
  | TBot, TPromise t2'         -> ok (TPromise t2', make_await p' e1' e2')
  | TPromise _, _ | TBot, _  -> error (fst e2, ExpectedFound (TPromise TBot, t2))
  | _                          -> error (fst e1, ExpectedFound (TPromise TBot, t1))

and check_spawn (ctxt : Context.t) (e1 : expr info) (e2 : expr info) :
  (typ * Ast.expr, error) result =
  check_expr ctxt (snd e1) >>= fun (t1, e1') ->
  check_expr ctxt (snd e2) >>= fun (t2, e2') ->
  let ret = ok (THandle TBot, make_spawn e1' e2') in
  match t1 with
  | TFun (t, _) -> if t2 <~ t then ret else error (fst e2, ExpectedFound (t, t2))
  | TBot        -> ret
  | _           -> error (fst e1, ExpectedFound (TFun (t2, TBot), t1))

and check_send (ctxt : Context.t) (e1 : expr info) (e2 : expr info) :
  (typ * Ast.expr, error) result =
  check_expr ctxt (snd e1) >>= fun (t1, e1') ->
  check_expr ctxt (snd e2) >>= fun (t2, e2') ->
  match t2 with
  | THandle TBot -> error (fst e2, HandleNotConcrete)
  | THandle t2'  ->
    if t1 <~ t2' then ok (TUnit, make_send e1' e2')
    else error (fst e1, ExpectedFound (t2', t1))
  | _            -> error (fst e2, ExpectedFound (THandle TBot, t2))

and check_recv (ctxt : Context.t) (e : expr info) :
  (typ * Ast.expr, error) result =
  check_expr ctxt (snd e) >>= fun (t, e') ->
  match t with
  | THandle TBot -> error (fst e, HandleNotConcrete)
  | THandle t'   -> ok (TPromise t', make_recv e')
  | _            -> error (fst e, ExpectedFound (THandle TBot, t))

and check_return (ctxt : Context.t) (e : expr info) :
  (typ * Ast.expr, error) result =
  check_expr ctxt (snd e) >>= fun (t, e') -> ok (TPromise t, make_return e')
