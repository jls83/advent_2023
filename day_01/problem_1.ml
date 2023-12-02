let filename = "/Users/jls83/other_projects/advent_2023/day_01/input.txt";;

let lines = In_channel.with_open_text filename In_channel.input_all
  |> String.split_on_char '\n'
  |> List.filter (fun s -> s <> "");;

let to_digit c = Char.code c - 48;;

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
