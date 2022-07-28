open Promise.Infix
open Ast

exception HandleNotFound of string 
exception InexhaustivePatterns

type value =
  | Unit of unit
  | Int of int
  | String of string
  | Bool of bool
  | Closure of pat * expr * env

and env = (pat * value) list

(*****************************************************************************
   Below are a few simple helper functions you need to implement. These are used 
   in various places throughout the system in working with your value and 
   environment types. Be sure you remember to fill them in. This is also a good
   spot to add any additional helper functions you might need involving values
   and environments.
 ******************************************************************************)

(** [assert_function v] is the parameter pattern, body expression, and closure
 * environment from [v]. 
 * Raises if [v] is not a function or a recursive function. Built-in functions
 * will also raise. *)
let assert_function (v : value) : pat * expr * env = match v with
  | Closure (p, ex, en) -> p, ex, en
  | _ -> failwith "Not a function"

(** [function_option] is either a closure, recursive closure, 
 * or not a closure. These correspond to value type constructors
 * in some way depending on how you implement the value type.
 * 
 * See the formal spec for more details on value.*)
type function_option = 
  | Func of pat * expr * env 
  | FuncRec of pat * expr * env ref 
  | NotAFunction 

(** [assert_function_option v] is the function_option with paramater pattern, 
 * body expression, and either closure environment or closure environment ref 
 * from [v]. 
 * 
 * Similar to assert_function, but will not raise. *)
let assert_function_option (v: value): function_option = match v with
  | Closure (p, ex, en) -> Func (p, ex, en)
  | _ -> NotAFunction

(** [make_handle h] is the value representing the handle [h]. *)
let make_handle (h : handle) : value =
  failwith "Unimplemented" (* TODO *)

(** [make_closure p e env] is the closure representing the pattern [p], 
 * expression [e], and closure environment [env]. *)
let make_closure (p: pat) (e: expr) (env: env) : value = 
  Closure (p, e, env)

(** [make_recursive_closure p e env] is the closure representing the pattern
 * [p], expression [e], and closure environment reference [env]. *)
let make_recursive_closure (p: pat) (e: expr) (env: env ref) : value = 
  failwith "Unimplemented" (* TODO *)

(* TODO: fill in built in functions print and println.
   Also, be sure to include in the initial environment a mapping from
   ["_SELF"] to the expression [make_handle 0]. If you do not do this, the 
   helper functions we have provided for you will not work correctly. *)
let initial_env : env = [
  ((Var "print"), make_closure (Var "s") (Print (VarE "s")) []);
  ((Var "println"), make_closure (Var "s") (Println (VarE "s")) []);
]

let update_env env x v =
  (Var x,v)::env

(** [pop_env_opt env] is the the tuple option [Some (var, value, env')] 
 *  where id [var] maps to value [v] and env' is the rest [env] not including 
 *  the mapping [var] -> [v]. [pop_env_opt env] is [None] if the [env] is empty.
 *  
 *  [var] -> [v] must be the latest mapping in [env]. 
 *  That is, [update_env env x1 v1 |> update_env x2 v2 |> pop_env_opt] 
 *  must equal [Some (x2,v2, env)]. 
 *  @return None if the environment is empty.*)
let pop_env_opt (env: env) : (id * value * env) option = 
  match env with
  | (a,b)::t -> Some ((match a with Var b -> b), b, t)
  | [] -> None

(** [rev_env env] is the the environment [env] with the order of 
 *  mappings reversed. 
 *  
 *  That is, 
 *  [update_env empty_env x1 v1 |> update_env x2 v2 |> rev_env |> pop_env_opt] 
 *  must equal [Some (x1,v1, env')] if [env'] equals 
 *  [update_env empty_env x1 v1]*)
let rev_env : env -> env = List.rev

(** [find_env env x] is the value to which [env] maps [x].
 *  Raises if there is no such value. *)
let find_env (env:env) x =
  List.assoc (Var x) env

let prepend_env env1 env2 =
  env1 @ env2

let string_of_value (v : value) : string =
  match v with
  | Unit _ -> "()"
  | Int i -> Int.to_string i
  | String s -> "\"" ^ String.escaped s ^ "\""
  | Bool b -> Bool.to_string b
  | Closure _ -> "<function>"

let string_of_env (env : env) : string =
  Int.to_string (List.length env)

(****************************************************************************
   These next few functions are helper functions we have implemented to help you
   implement the concurrency features of RML. You must use (send, recv, spawn, self) 
   in your implementation to ensure it behaves correctly. You do not need to 
   understand how they work, but feel free to look at them if you are interested.
   Under no circumstances should you change any of the following implementations,
   nor should you use any of the below functions except (send, recv, spawn, self).
 ******************************************************************************)

(** Tracks whether the main thread has been initialized *)
let main_thread_initialized = ref false

(** We don't expect more than 20 threads, on average. *)
(** [mailboxes] is a map from handles to a [Hashtabl.t] which maps
    senders to values. 

    If a thread with handle [h1] wants to send a message to a 
    thread with handle [h2]. It must send the mesage to 
    [Hashtable.find mailboxes h2 |> (fun mailbox -> Hashtabl.find mailbox h1)]*)
let mailboxes = Hashtbl.create 20

let fresh_handle =
  let counter = ref 1 in
  fun () -> 
    incr counter; 
    !counter - 1 

(** [init_main_mailbox ()] opens a mailbox on 
  * thread_id 0 for the main thread if one is not already open. 
  * 
  * DO NOT USE THIS IN YOUR CODE. *)
let init_main_mailbox () = 
  if !main_thread_initialized
  then ()
  else
    begin 
      Hashtbl.create 5 |> Hashtbl.add mailboxes 0; 
      main_thread_initialized := true
    end 

(** [send v h sender_handle] 
    sends [v] asynchronously to the robot at handle [h] with 
    [sender_handle] being the sender robot's handle.
  * Requires: [h] is a valid handle obtained from [spawn] or [self] 
  * Requires: [sender_handle] is a valid handle refering to the 
    calling thread's own handle. The environment should keep 
    track of the calling thread's own handle. *)
let send (s : value) (h : int) (sender_handle: int) : unit =
  init_main_mailbox (); 
  let find_mailbox h sender_handle = 
    match Hashtbl.find_opt mailboxes h with 
    | None -> 
      raise (HandleNotFound (string_of_int h))
    | Some address ->            
      match Hashtbl.find_opt address sender_handle with 
      | Some stream_push -> 
        stream_push
      | None -> 
        let stream_push = Lwt_stream.create () in 
        Hashtbl.add address sender_handle stream_push;              
        stream_push in
  let _,push = find_mailbox h sender_handle in
  push (Some s)

(** [recv h receiver_handle] 
    is a promise representing the next value sent from the robot at 
  * handle [h]. The promise will resolve at some time after such a value is sent
  * from the handle. 
  * Requires: [h] is a valid handle obtained from [spawn] or [self]. 
  * Requires: [reciever_handle] is a valid handle refering to the 
  * calling thread's own handle. The environment should keep 
  * track of the calling thread's own handle. *)
let recv (h : int) (receiver_handle: int) : value Promise.t =  
  init_main_mailbox ();
  let find_mailbox h receiver_handle = 
    match Hashtbl.find_opt mailboxes receiver_handle with 
    | None -> 
      raise (HandleNotFound (string_of_int h))
    | Some address -> 
      match Hashtbl.find_opt address h with 
      | Some stream_push -> 
        stream_push
      | None ->  
        let stream_push = (Lwt_stream.create ()) in
        Hashtbl.add address h stream_push;
        stream_push in
  let stream,_ = find_mailbox h receiver_handle in 
  let rec poll stream : value Lwt.t = 
    let lwt_bind = Lwt.(>>=) in 
    lwt_bind 
      (Lwt_stream.get stream)
      (fun opt -> 
         match opt with 
         | Some v -> 
           Lwt.return v
         | None ->
           poll stream) in
  poll stream |>
  Promise.danger_danger_if_you_call_this_function_you_will_get_a_zero_on_a5_inverse 

let spawn_expr (f : value) (arg : value) : expr * env =
  let open Ast_factory in
  let (p, b, clenv) = assert_function f in
  let app = make_app (make_fun p b) (make_var "_spawn_arg") in
  let env = update_env clenv "_spawn_arg" arg in
  app, env

(** [empty_env ()] is a helper function for spawn_env and returns an 
  * environment with no elements. Initial environment elements are also absent 
  * from [empty_env ()]. 
  * 
  * DO NOT USE THIS IN YOUR CODE. 
  * Requires: pop_initial is implemented correctly. *)
let empty_env () =
  let rec pop_initial env =  
    match pop_env_opt env with 
    | Some (_,_,env_tail) -> pop_initial env_tail
    | None -> env 
  in pop_initial initial_env

(** [backpatch_new_env env new_env target_var acc_env] 
  * correctly updates the [env ref] new_env to have value env 
  * where every value which is bound to the [id] [target_var] 
  * and where the value is a recursive closure will have its 
  * closure environment changed to new_env, correctly backpatching 
  * the new_env. This is meant as a helper function for [spawn_env]
  *
  * DO NOT USE THIS IN YOUR CODE. 
  * Typical use case is 
  * [backpatch_new_env !closure_env closure_env closure_id (empty_env ())] *)
let rec backpatch_new_env 
    (env: env) 
    (new_env : env ref) 
    (target_var : string) 
    (acc_env : env) : unit = 
  match pop_env_opt env with 
  | Some (var, v, env_tail) when var = target_var -> 
    begin 
      match assert_function_option v with 
      | Func _ -> 
        let acc' = update_env acc_env var v in 
        backpatch_new_env env_tail new_env target_var acc' 
      | FuncRec (p, e, _) -> 
        let closure_env' = new_env in 
        let new_closure = make_recursive_closure p e closure_env' in
        let acc' = update_env acc_env var new_closure in 
        backpatch_new_env env_tail new_env target_var acc' 
      | NotAFunction -> 
        let acc' = update_env acc_env var v in 
        backpatch_new_env env_tail new_env target_var acc' 
    end
  | Some (var, v, env_tail) -> 
    let acc' = update_env acc_env var v in 
    backpatch_new_env env_tail new_env target_var acc' 
  | None -> new_env := (rev_env acc_env)

(** [spawn_env env handle_value acc excluded_vars] is the environment [env'] 
  * which is the environment [env] with all mappings from key ["_SELF"] updated 
  * to handle_value. With keys in [excluded_vars] ignored for the recursive call. 
  * 
  * DO NOT USE THIS IN YOUR CODE. This helper function is meant to be 
  * called in spawn only.  
  * Typical usage is [spawn_env env handle_value (empty_env ()) []]*)
let rec spawn_env 
    (env: env) 
    (handle_value: int) 
    (acc: env) 
    (excluded_vars: id list) : env = 
  match pop_env_opt env with 
  | Some (var, v, env_tail) -> 
    if List.mem var excluded_vars then 
      let acc' = update_env acc var v in
      spawn_env env_tail handle_value acc' excluded_vars
    else 
      begin 
        match assert_function_option v with 
        | Func (p, e, closure_env) -> 
          let closure_env' = spawn_env closure_env handle_value (empty_env ()) excluded_vars in 
          let new_closure = 
            make_closure p e (update_env 
                                closure_env'
                                "_SELF" 
                                (make_handle handle_value)) in 
          let acc' = update_env acc var new_closure in 
          spawn_env env_tail handle_value acc' excluded_vars
        | FuncRec (p, e, closure_env) -> 
          let closure_env' = spawn_env 
              !closure_env 
              handle_value 
              (empty_env ()) 
              (var::excluded_vars)
          in 
          let new_env_ref = ref (update_env 
                                   closure_env'
                                   "_SELF" 
                                   (make_handle handle_value)) 
          in  
          backpatch_new_env !new_env_ref new_env_ref var (empty_env ()); 
          let new_closure = make_recursive_closure p e new_env_ref in 
          let acc' = update_env acc var new_closure in 
          spawn_env env_tail handle_value acc' excluded_vars
        | NotAFunction -> 
          let acc' = update_env acc var v in
          if var <> "_SELF" then 
            spawn_env env_tail handle_value acc' excluded_vars
          else spawn_env env_tail handle_value acc excluded_vars
      end
  | None -> 
    let env' = update_env (rev_env acc) "_SELF" (make_handle handle_value) in 
    env'

(** [spawn eval f a] is the handle [h] of a new robot running the function [f]
  * with argument [a]. The argument [eval] is used to evaluate this 
  * function application.
  *
  * This call is blocking, guaranteeing that [send] and [recv] calls to [h] will
  * be valid after it returns.
  * 
  * Requires: [eval] is [eval_expr]. *)
let spawn (eval : env -> expr -> value) (f : value)  (a : value) : handle =
  let open Lwt.Infix in
  let thread_id = fresh_handle () in 
  Hashtbl.create 5 |> Hashtbl.add mailboxes thread_id;
  let eval_promise () =
    let (app, env) = spawn_expr f a in 
    Lwt.return () >>= fun () ->
    (* let env' = update_env env "_SELF" (make_handle thread_id) in *)
    let env' = spawn_env env thread_id (empty_env ()) [] in
    let _ = eval env' app in 
    Lwt.return () in 
  let _ = Lwt.async eval_promise in 
  thread_id

(** [self env] uses the environment [env] to produce the [self] handle value. *)
let self (env : env) : value = find_env env "_SELF"

(***************************************************************************
   The rest of the functions in this file compose the main part of the RML
   interpreter. It is your task to implement this functions. Good luck!
 *****************************************************************************)

let bind_pattern (p : pat) (v : value) : env option =
  Some [(p,v)]

let eval_add v1 v2 = match v1 with
  | Int i -> (match v2 with
      | Int o -> Int (i + o)
      | _ -> failwith "faulty bop")
  | _ -> failwith "faulty bop"

let eval_and v1 v2 = match v1 with
  | Bool i -> (match v2 with
      | Bool o -> Bool (i && o)
      | _ -> failwith "faulty bop")
  | _ -> failwith "faulty bop"

let eval_or v1 v2 = match v1 with
  | Bool i -> (match v2 with
      | Bool o -> Bool (i && o)
      | _ -> failwith "faulty bop")
  | _ -> failwith "faulty bop"

let rec eval_expr (env : env) (expr : expr) : value =
  match expr with
  | Unit u -> Unit u
  | Int i -> Int i
  | String s -> String s
  | Bool b -> Bool b
  | VarE p -> List.assoc (Var p) env
  | Bop (b,e1,e2) -> (match b with
      | Add -> eval_add (eval_expr env e1) (eval_expr env e2)
      | And -> eval_and (eval_expr env e1) (eval_expr env e2)
      | Or -> eval_or (eval_expr env e1) (eval_expr env e2)
      | _ -> failwith "Unimplemented")
  | Let (s,e1,e2) -> eval_expr ((s, eval_expr env e1)::env) e2
  | Fun (p,e) -> make_closure p e env
  | App (e1,e2) -> (let clos = assert_function (eval_expr env e1) in
                    let v2 = eval_expr env e2 in
                    match clos with
                    | (p,ex,en) -> eval_expr ((p,v2)::en) ex)
  | If (e1,e2,e3) -> if (eval_expr env e1) = Bool true then eval_expr env e2
    else eval_expr env e3
  | Print s -> print_newline ();
    (eval_expr env s |> string_of_value |> print_string); Unit ()
  | Println s -> print_newline ();
    (eval_expr env s |> string_of_value |> print_endline); Unit ()


let eval_defn (env : env) (defn : defn) : env =
  let defn' = (fst defn, eval_expr env (snd defn)) in
  defn'::env

let eval_program (env : env) (prog : prog) : env =
  prog |> List.fold_left eval_defn env
