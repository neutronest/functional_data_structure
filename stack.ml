exception Empty
exception Subscript
open Printf

module type STACK =
  sig
    type 'a stack

    val empty : 'a stack
    val is_empty : 'a stack -> bool
    val cons : 'a -> 'a stack -> 'a stack
    val head : 'a stack -> 'a
    val tail : 'a stack -> 'a stack
    val push : 'a stack -> 'a -> 'a stack
    val pop : 'a stack -> 'a stack
    val top : 'a stack -> 'a
    val (++):  'a stack -> 'a stack -> 'a stack
    val update: 'a stack -> int -> 'a -> 'a stack
  end

module ListStack : STACK =
  struct
    type 'a stack = 'a list

    let empty = []

    let is_empty s = (s = [])

    let cons x s = x :: s

    let head = function
      | [] -> raise Empty
      | h :: _ -> h

    let tail = function
      | [] -> raise Empty
      | _ :: t -> t

    let push s x =
      match s with
      | [] -> [x]
      | h :: t -> x :: (h :: t)

    let pop s =
      match s with
      | [] -> raise Empty
      | h :: t -> t

    let top s =
      match s with
      | [] -> raise Empty
      | h :: _  -> h

    let rec (++) xs ys =
      match xs with
      | [] -> ys
      | h :: t -> h :: ((++) t ys)

    let rec update s i x =
      match s with
      | [] -> raise Subscript
      | h :: t ->
         if i = 0 then x :: t else update t (i-1) x

  end
