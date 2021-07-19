(* 
   https://ocaml.org/learn/tutorials/99problems.html#15-Replicate-the-elements-of-a-list-a-given-number-of-times-medium

   15. Replicate the elements of a list a given number of times. 

 *)

val rec replicate =
 fn lst => fn cnt =>
    let
	val rec accumulate =
	 fn acc => fn count =>
	    fn [] => []
	  | [x] => if count = 0 then acc else accumulate (x :: acc) (count -1) [x]
	  | (x :: xs) => accumulate (accumulate acc count [x]) count xs
    in
	accumulate [] cnt (List.rev lst)
    end;
