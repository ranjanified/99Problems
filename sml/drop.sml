(*
	https://ocaml.org/learn/tutorials/99problems.html#16-Drop-every-N-39-th-element-from-a-list-medium
	16. Drop every N'th element from a list.+
*)

fun drop lst num =
    let
	fun accumulate acc index =
	    fn [] => List.rev acc
	  | (next as x :: xs) => if index = 1 then accumulate acc num xs else accumulate (x::acc) (index - 1) xs
    in
	accumulate [] num lst
    end;
