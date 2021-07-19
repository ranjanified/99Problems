(*
	https://ocaml.org/learn/tutorials/99problems.html#18-Extract-a-slice-from-a-list-medium
	18. Extract a slice from a list. (medium)
	Given two indices, i and k, the slice is the list containing the elements between the i'th and k'th element of the original list (both limits included). Start counting the elements with 0 (this is the way the List module numbers elements).

*)

fun slice lst startIndex endIndex =
    let
	fun reifyStart startIdx endIdx = if startIndex > endIndex then endIndex else startIndex;
	fun reifyEnd startIdx endIdx = if startIndex < endIndex then endIndex else startIndex;
	val start = reifyStart startIndex endIndex;
	val endIdx = reifyEnd startIndex endIndex;
			     
	fun accumulate acc curr =
	    fn [] => if startIndex <= endIndex then List.rev acc else acc
	  | (whole as x::xs) =>
	    if curr >= start then
		if curr <= endIdx then
		    accumulate (x :: acc) (curr + 1) xs
		else accumulate acc curr []
	    else accumulate acc (curr + 1) xs;

    in
	accumulate [] 0 lst
    end;
