(*
https://ocaml.org/learn/tutorials/99problems.html#12-Decode-a-run-length-encoded-list-medium

Given a run-length code list generated as specified in the previous problem, construct its uncompressed version.

*)

datatype 'a encoded = One of 'a | Many of (int * 'a)

fun decode lst =
    let
	fun extract_value_count chr =
	    case chr of
		One value => (1, value)
	      | Many (count, value) => (count, value);

	fun acc_list acc (0, _) = acc
	  | acc_list acc (count, value) = acc_list (value :: acc) (count - 1, value);
	
	fun generate_list chr =	acc_list [] (extract_value_count chr)

	fun accumulate_decode acc =
	    fn [] => []
	  | [x] => acc @ (generate_list x)
	  | (x :: xs) => accumulate_decode (acc @ (generate_list x)) xs
    in
	accumulate_decode [] lst
    end;

fun opt_decode lst =
    let
	val rec accumulate = fn acc =>
	 fn [] => []
	  | [One x] => (x :: acc)
	  | [Many (0, value)] => acc
	  | [Many (count, value)] => accumulate (value :: acc) [Many (count - 1, value)]
	  | (x :: ys) => accumulate (accumulate acc [x]) ys
    in
	accumulate [] (List.rev lst)
    end;
