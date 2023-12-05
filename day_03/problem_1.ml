include Re;;

let filename = "/Users/jls83/other_projects/advent_2023/day_03/example_1.txt";;

let lines = In_channel.with_open_text filename In_channel.input_all
  |> String.split_on_char '\n'
  |> List.filter (fun s -> s <> "");;

let make_matrix ls =
  List.map Array.of_list ls
    |> Array.of_list;;
  
let my_re =
  let open Re in
  alt [rg '0' '9'; rg '\\' '.']
  |> compile;;

let get_symbol_indices s =
  String.to_seq s
    |> Seq.map (String.make 1)
    |> Seq.mapi (fun i c -> (i, Re.execp my_re c))
    |> Seq.filter (fun (i, c) -> not c)
    |> Seq.map (fun (i, _) -> i)
    |> List.of_seq;;

(* Convoluted as all get out, but it works. *)
let ugh ls =
  List.mapi (fun i l ->
    match get_symbol_indices l with
      | [] -> None
      | xs -> Some (List.map (fun j -> (i, j)) xs)) ls
    |> List.filter_map (fun x -> x);;
