open Ast

let make_let_defn p e =
  p, e

let make_let_rec_defn p e =
  failwith "Unimplemented"

let make_unit_pattern () =
  failwith "Unimplemented"

let make_wild_pattern () =
  failwith "Unimplemented"

let make_bool_pattern b =
  failwith "Unimplemented"

let make_int_pattern n =
  failwith "Unimplemented"

let make_string_pattern s =
  failwith "Unimplemented"

let make_var_pattern x =
  Var x

let make_pair_pattern p1 p2 =
  failwith "Unimplemented"

let make_nil_pattern () =
  failwith "Unimplemented"

let make_cons_pattern p1 p2 =
  failwith "Unimplemented"

let make_unit () =
  Unit ()

let make_bool b =
  Bool b

let make_pair e1 e2 =
  failwith "Unimplemented"

let make_int n =
  Int n

let make_string s =
  String s

let make_self () =
  failwith "Unimplemented"

let make_var x =
  VarE x

let make_fun p e =
  Fun (p, e)

let make_app e1 e2 =
  App (e1, e2)

let make_let p e1 e2 =
  Let (p, e1, e2)

let make_let_rec p e1 e2 =
  failwith "Unimplemented"

let make_nil () =
  failwith "Unimplemented"

let make_bop b e1 e2 =
  Bop (b, e1, e2)

let make_uop u e =
  failwith "Unimplemented"

let make_seq e1 e2 =
  failwith "Unimplemented"

let make_ifelse e1 e2 e3 =
  If (e1, e2, e3)

let make_match e cs =
  failwith "Unimplemented"

let make_await p e1 e2 =
  failwith "Unimplemented"

let make_spawn e1 e2 =
  failwith "Unimplemented"

let make_send e1 e2 =
  failwith "Unimplemented"

let make_recv e =
  failwith "Unimplemented"

let make_return e =
  failwith "Unimplemented"