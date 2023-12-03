let filename = "/Users/jls83/other_projects/advent_2023/day_01/input.txt";;

let lines = In_channel.with_open_text filename In_channel.input_all
  |> String.split_on_char '\n'
  |> List.filter (fun s -> s <> "");;

let to_digit c = Char.code c - 48;;

type trie = Trie of int option * char_to_children
    and char_to_children = (char * trie) list;;

(* This is bonkers. *)

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

 ('e', Trie (Some 0,
  [('i', Trie (Some 0,
    [('g', Trie (Some 0,
      [('h', Trie (Some 0,
        [('t', Trie (Some 8, []))]))]))]))]));

 ('f', Trie (Some 0, 
  [('i', Trie (Some 0, 
    [('v', Trie (Some 0, 
      [('e', Trie (Some 5, []))]))]));
   ('o', Trie (Some 0, 
     [('u', Trie (Some 0,
       [('r', Trie (Some 4, []))]))]))]));

 ('n', Trie (Some 0,
  [('i', Trie (Some 0,
    [('n', Trie (Some 0,
      [('e', Trie (Some 9, []))]))]))]));

 ('o', Trie (Some 0,
  [('n', Trie (Some 0,
    [('e', Trie (Some 1,[]))]))]));

 ('s', Trie (Some 0,
  [('i', Trie (Some 0,
    [('x', Trie (Some 6, []))]));
   ('e', Trie (Some 0,
     [('v', Trie (Some 0,
       [('e', Trie (Some 0,
         [('n', Trie (Some 7, []))]))]))]))]));

 ('t', Trie (Some 0,
  [('w', Trie (Some 0,
    [('o', Trie (Some 2, []))]));
   ('h', Trie (Some 0,
     [('r', Trie (Some 0,
       [('e', Trie (Some 0,
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

let rec parse_line s pos len res =
  if (len > (String.length s) - pos) || (pos >= String.length s) then res
  else
    match lookup number_trie (String.sub s pos len) with
    | Some 0 -> parse_line s pos (len + 1) res
    | Some n -> let next = res @ [n] in parse_line s (pos + 1) 1 next
    | None ->   parse_line s (pos + 1) 1 res;;

let what s = parse_line s 0 1 []
  |> Array.of_list
  |> bar;;

let res = List.map what lines |> List.fold_left (+) 0;;

Printf.printf "%d\n" res;;
