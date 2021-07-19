(*
	https://ocaml.org/learn/tutorials/99problems.html#17-Split-a-list-into-two-parts-the-length-of-the-first-part-is-given-easy
	
	17. Split a list into two parts; the length of the first part is given. (easy)
	If the length of the first part is longer than the entire list, then the first part is the list and the second part is empty	
*)

fun split lst len =
    let
	fun accumulate fst tail index =
	    fn [] => [List.rev fst, tail]
	  | (full as (x :: xs)) =>
	    if index > len
	    then accumulate fst full index []
	    else accumulate (x :: fst) xs (index + 1) xs
    in
	accumulate [] [] 1 lst
    end;
