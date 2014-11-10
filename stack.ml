exception Empty
exception Subscript

module type STACK =
  sig
    type 'a stack

    val empty : 'a stack
    val is_empty : 'a stack -> bool
    val cons : 'a stack -> 'a
    val head : 'a stack -> 'a
    val tail : 'a stack -> 'a stack
    val (++) 'a stack -> 'a stack -> 'a stack
    val update 'a stack -> int -> 'a -> 'a stack
  end

module ListStack : STACK =
  struct
    type 'a stack = 'a list

    let empty = []

    let is_empty s = (s = [])

    let cons x s = x :: s

    let head = function
      | [] -> raise empty
      | h :: _ -> h

    let tail = function
      | [] -> raise empty
      | _ :: t -> t

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

module CustomStack :: STACK =

end
