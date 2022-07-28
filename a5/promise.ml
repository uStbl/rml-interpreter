type 'a t = 'a Lwt.t

let danger_danger_if_you_call_this_function_you_will_get_a_zero_on_a5 p = p

let danger_danger_if_you_call_this_function_you_will_get_a_zero_on_a5_inverse p = p

let return = 
  Lwt.return

let bind =
  Lwt.bind

module Infix = struct
  let (>>=) = Lwt.bind
end
