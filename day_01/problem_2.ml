let filename = "/Users/jls83/other_projects/advent_2023/day_01/example2.txt";;

let lines = In_channel.with_open_text filename In_channel.input_all
  |> String.split_on_char '\n'
  |> List.filter (fun s -> s <> "");;

let to_digit c = Char.code c - 48;;

type trie = Trie of int option * char_to_children
    and char_to_children = (char * trie) list;;

let example =
  Trie (None,
	[('i', Trie (Some 11,
                     [('n', Trie (Some 5, [('n', Trie (Some 9, []))]))]));
	 ('t',
	  Trie (None,
		[('e',
		  Trie (None,
			[('n', Trie (Some 12, [])); ('d', Trie (Some 4, []));
			 ('a', Trie (Some 3, []))]));
		 ('o', Trie (Some 7, []))]));
     ('A', Trie (Some 15, []))]);;

(*
    "one":      1,
    "two":      2,
    "three":    3,
    "four":     4,
    "five":     5,
    "six":      6,
    "seven":    7,
    "eight":    8,
    "nine":     9,

    *)

let number_trie = Trie (None, 
[
 ('1', Trie (Some 1, []));
 ('2', Trie (Some 2, []));
 ('3', Trie (Some 3, []));
 ('4', Trie (Some 4, []));
 ('5', Trie (Some 5, []));
 ('6', Trie (Some 6, []));
 ('7', Trie (Some 7, []));
 ('8', Trie (Some 8, []));
 ('9', Trie (Some 9, []));

 ('e', Trie (None,
  [('i', Trie (None,
    [('g', Trie (None,
      [('h', Trie (None,
        [('t', Trie (Some 8, []))]))]))]))]));

 ('f', Trie (None, 
  [('i', Trie (None, 
    [('v', Trie (None, 
      [('e', Trie (Some 5, []))]))]));
   ('o', Trie (None, 
     [('u', Trie (None,
       [('r', Trie (Some 4, []))]))]))]));

 ('n', Trie (None,
  [('i', Trie (None,
    [('n', Trie (None,
      [('e', Trie (Some 9, []))]))]))]));

 ('o', Trie (None,
  [('n', Trie (None,
    [('e', Trie (Some 1,[]))]))]));

 ('s', Trie (None,
  [('i', Trie (None,
    [('x', Trie (Some 6, []))]));
   ('e', Trie (None,
     [('v', Trie (None,
       [('e', Trie (None,
         [('n', Trie (Some 7, []))]))]))]))]));

 ('t', Trie (None,
  [('w', Trie (None,
    [('o', Trie (Some 2, []))]));
   ('h', Trie (None,
     [('r', Trie (None,
       [('e', Trie (None,
         [('e', Trie (Some 3, []))]))]))]))]))]);;


let rec check_for_target children target_char = 
  match children with 
    |(parent_char, possible_value) :: tl -> 
      if parent_char = target_char then Some possible_value
      else check_for_target tl target_char 
    |_ -> None;; 

let rec lookup_with_index trie w index = 
  match trie with 
    | Trie (key, children) -> 
      if (index >= String.length w) then key
      else match (check_for_target children w.[index]) with
        | Some trie -> lookup_with_index trie w (index + 1)
        | None -> None
      ;;

let rec lookup trie w = 
  lookup_with_index trie w 0;;

(* TODO: is this messy? *)
let bar a = (Array.get a 0 * 10) + Array.get a (Array.length a - 1);;

(* TODO: filter_map? *)
let foo s = String.to_seq s
  |> Seq.map to_digit
  |> Seq.filter (fun n -> n > 0 && n < 10)
  |> Array.of_seq
  |> bar;;

let res = List.map foo lines |> List.fold_left (+) 0;;

Printf.printf "%d\n" res;;
