type 'a t

(** [return v] returns a promise that is already resolved with [v]. *)
val return : 'a -> 'a t

(** Exactly the same specification as Lwt.bind. (You may use the infix operator [>>=].)
  * If [p] is already resolved, then [f] is run immediately on the contents of [p].
  * If [p] is pending, then [bind] registers [f] as a callback to eventually be run
  * when (or if) the promise is resolved. A new promise is immediately returned that
  * will in the future resolve to the result of the callback. *)
val bind : 'a t -> ('a -> 'b t) -> 'b t

(** Convenience module for providing infix syntax. *)
module Infix : sig
  (** Infix operator for [bind]. *)
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t 
end

(** Staff use only *)
val danger_danger_if_you_call_this_function_you_will_get_a_zero_on_a5 : 'a t -> 'a Lwt.t

val danger_danger_if_you_call_this_function_you_will_get_a_zero_on_a5_inverse : 'a Lwt.t -> 'a t

