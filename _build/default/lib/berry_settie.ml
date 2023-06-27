type 'a regex =
  | Epsilon
  | Const of 'a
  | Sum of 'a regex * 'a regex
  | Concat of 'a regex * 'a regex
  | Kleene of 'a regex

let rec contains_eps = function
  | Epsilon -> true
  | Const _ -> false
  | Sum (e1, e2) -> contains_eps e1 || contains_eps e2
  | Concat (e1, e2) -> contains_eps e1 && contains_eps e2
  | Kleene _ -> true

let rec union l1 l2 = match l2 with
  | [] -> l1
  | h::t when List.mem h l1 -> union l1 t
  | h::t -> h::union l1 t

let rec prefixes = function
  | Epsilon -> []
  | Const a -> [a]
  | Sum (e1, e2) -> union (prefixes e1) (prefixes e2)
  | Concat (e1, e2) when contains_eps e1 -> union (prefixes e1) (prefixes e2)
  | Concat (e1, _) -> prefixes e1
  | Kleene e -> prefixes e

let rec suffixes = function
  | Epsilon -> []
  | Const a -> [a]
  | Sum (e1, e2) -> union (suffixes e1) (suffixes e2)
  | Concat (e1, e2) when contains_eps e2 -> union (suffixes e1) (suffixes e2)
  | Concat (_, e2) -> suffixes e2
  | Kleene e -> suffixes e

let rec cart_prod l1 l2 = match (l1, l2) with
  | [], _ | _, [] -> []
  | h1::t1, h2::t2 -> (h1, h2)::(cart_prod t1 [h2]) @ (cart_prod (h1::t1) t2)

let rec factors_oft = function
  | Epsilon
  | Const _ -> []
  | Sum (e1, e2) -> union (factors_oft e1) (factors_oft e2)
  | Concat (e1, e2) -> union (union (factors_oft e1) (factors_oft e2)) (cart_prod (suffixes e1) (prefixes e2))
  | Kleene e -> factors_oft (Concat (e, e))

let linearisation e' =
  let rec aux i = function
    | Epsilon -> Epsilon
    | Const a -> Const (a, i)
    | Sum (e1, e2) -> Sum (aux (2 * i) e1, aux (2 * i + 1) e2)
    | Concat (e1, e2) -> Concat (aux (2 * i) e1, aux (2 * i + 1) e2)
    | Kleene e -> aux i e in
  aux 1 e'

type ('a, 'b) automaton = {
  states: 'b list;
  init: 'b list;
  fin: 'b list;
  trans: ('b * 'a * 'b) list
}

