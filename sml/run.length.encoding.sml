(* https://ocaml.org/learn/tutorials/99problems.html#10-Run-length-encoding-of-a-list-easy *)

fun encode lst =
    let
	fun count_consecutives acc = fn count =>
					fn [] => []
	  | [x] => (count + 1, x) :: acc
	  | (x :: y :: xs) =>
	    if x = y
	    then count_consecutives acc (count + 1) (y :: xs)
	    else count_consecutives ((count + 1, x) :: acc) 0 (y::xs)
    in
	List.rev (count_consecutives [] 0 lst)
    end;
