(*

https://ocaml.org/learn/tutorials/99problems.html#11-Modified-run-length-encoding-easy

11. Modified run-length encoding. (easy)
Modify the result of the previous problem in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.

*)

datatype 'a encoded = One of 'a | Many of (int * 'a); 

fun mod_encode lst =
    let
	fun create_val x count =
	    if count > 1
	    then Many (count, x)
	    else One (x);
	
	fun accumulate_consecutives acc =
	    fn count =>
	       fn [] => []
	  | [x] => (create_val x (count + 1)) :: acc
	  | (x :: (next as y :: _)) =>
	    if x = y
	    then accumulate_consecutives acc (count + 1) next
	    else accumulate_consecutives ((create_val x (count + 1)) :: acc) 0 next
    in
	List.rev (accumulate_consecutives [] 0 lst)
    end;
