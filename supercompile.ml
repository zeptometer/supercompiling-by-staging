let rec contrived zs =
  let f xs ys = match xs with [] -> length xs + length ys
                            | w::ws -> length xs
  in f zs (1::zs)
and length ws = match ws with [] -> 0
                            | _::ws -> 1 + length ws

type ('s, 'd) sd = {
  mutable dynamic : 'd code;
  mutable static : 's option;
}

type ('s, 'd) ps_cell =
  | Nil
  | Cons of ('s,'d) sd * ('s,'d) psl
and ('s, 'd) psl = (('s,'d) ps_cell, 'd list) sd

let unknown (x : 'd code): ('s, 'd) sd = { dynamic = x; static = None }

let forget (x : ('a, 'b) sd) : 'b code = x.dynamic

let assuming_eq (x : ('a, 'b) sd)(v : 'a )(thunk : unit -> 'c) : 'c =
  let saved = x.static in
  try x.static <- Some v;
    let ret = thunk () in
    x.static <- saved;
    ret
  with e -> x.static <- saved;
    raise e

let dfun (f : ('a, 'b) sd -> 'c code) : ('b -> 'c) code =
  .<fun x -> .~(f (unknown .<x>.))>.

let match_ls (ls : (('a,'b) ps_cell, 'b list) sd)
    (for_nil : unit -> 'c code)
    (for_cons : ('a,'b) sd -> ('a,'b) psl -> 'c code)
  : 'c code =
  match ls.static with
  | Some Nil -> for_nil ()
  | Some (Cons (x,xs)) -> for_cons x xs
  | None ->
    .<match .~(forget ls) with
      | [] -> .~(assuming_eq ls Nil for_nil)
      | x::xs -> .~(let x = unknown .<x>. and xs = unknown .<xs>. in assuming_eq ls (Cons (x,xs)) (fun () -> for_cons x xs))>.
