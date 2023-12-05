let filename = "/Users/jls83/other_projects/advent_2023/day_02/input.txt";;

let lines = In_channel.with_open_text filename In_channel.input_all
  |> String.split_on_char '\n'
  |> List.filter (fun s -> s <> "");;

type game =
  { game_id: int;
    results: (string, int) Hashtbl.t;
  };;

let construct_result p =
  match p with
    | game_count :: game_color :: _ -> game_color, int_of_string game_count
    (* Well this is annoying. *)
    | _ -> failwith "Not possible"
;;

let parse_scores scores = String.split_on_char ',' scores
  |> List.map String.trim
  |> List.map (String.split_on_char ' ')
  |> List.map construct_result
;;

let rec set_max_value tbl items =
  match items with
    | [] -> tbl
    | (k, v) :: tl ->
        let new_v = match Hashtbl.find_opt tbl k with
          | Some n -> max n v
          | None -> v
        in
        Hashtbl.replace tbl k new_v;
        set_max_value tbl tl
;;

let parse_line line =
  let game_id_str, scores = match String.split_on_char ':' line with
    | a :: b :: _ -> a, b
    | _ -> failwith "Not possible"
  in
  let game_id = match String.split_on_char ' ' game_id_str with
    | "Game" :: n :: _ -> int_of_string n
    | _ -> failwith "Not possible"
  in
  let results = String.split_on_char ';' scores
    |> List.map parse_scores
    |> List.flatten
    |> set_max_value (Hashtbl.create 3)
  in { game_id ; results }
;;

let get_cube_score g =
  Hashtbl.to_seq_values g.results
    |> Seq.fold_left (fun a c -> a * c) 1;;

let res =
  List.map parse_line lines
    |> List.map get_cube_score
    |> List.fold_left (+) 0;;

Printf.printf "%d\n" res;;
