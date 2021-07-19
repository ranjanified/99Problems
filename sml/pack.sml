(* https://ocaml.org/learn/tutorials/99problems.html#9-Pack-consecutive-duplicates-of-list-elements-into-sublists-medium *)

fun pack lst =
    let
	fun accumulate_packs acc packs =
	    fn
		[] => []
	  | [x] => (x :: packs) :: acc
	  | (x :: y :: xs) =>
	    if x = y
	    then accumulate_packs acc (x :: packs) (y::xs)
	    else accumulate_packs ((x :: packs) :: acc) [] (y::xs)
    in
	List.rev (accumulate_packs [] [] lst)
    end;
