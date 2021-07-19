(* 
   https://ocaml.org/learn/tutorials/99problems.html#14-Duplicate-the-elements-of-a-list-easy
   14. Duplicate the elements of a list 

*)

val rec dup_list =
 fn lst =>
    let
	val rec accumulate =
	 fn acc =>
	    fn [] => []
	  | [x] => (x :: x :: acc)
	  | (x :: xs) => accumulate (x :: x :: acc) xs
				    
    in
	accumulate [] (List.rev lst)
    end;
