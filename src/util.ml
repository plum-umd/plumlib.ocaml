(* Some helpers for lists, nats. *)

let fold_left3 f a l1 l2 l3 =
  let rec loop a = function
    | x::xs, y::ys, z::zs -> loop (f a x y z) (xs, ys, zs)
    | [], [], [] -> a
    | _ -> failwith "Lists given to fold_left3 must be the same length!"
  in
  loop a (l1, l2, l3)

let map3  f = fold_left3 (fun acc a b c -> (f a b c)::acc) []
let iter3 f = fold_left3 (fun _ a b c -> f a b c) ()

let string_of_list ?(sep="; ") ?(border=(fun s -> "[" ^ s ^ "]")) sox = function
  | [] -> border ""
  |  l ->
    let elts = List.fold_right
        (fun x a -> Printf.sprintf "%s%s%s" (sox x) sep a)
        l
        ""
    in
    border (String.sub elts 0 (String.length elts - String.length sep))

let take =
  let rec loop a = function
    |     _, 0 -> a
    | x::xs, n -> loop (x::a) (xs, n-1)
    |     _    -> assert false
  in
  fun n l -> List.rev (loop [] (l, n))

let drop =
  let rec loop = function
    |    xs, 0 -> xs
    | x::xs, n -> loop (xs, n-1)
    | _        -> assert false
  in
  fun n l -> loop (l, n)

let rec foldn f a = function
  | 0 -> a
  | n -> let n' = n-1 in foldn f (f a n') n'

let range n max =
  let rec loop a m =
    if m < max+n then loop (m::a) (m+1) else List.rev a in
  loop [] n

let multiply x =
  let rec loop a = function
    | 0 -> a
    | n -> loop (x::a) (n-1)
  in
  loop []

let combs xss =
  let rec loop suff acc x = match suff with
    |      [] -> [[x]]
    | ys::xss -> List.fold_left
                   (fun acc y ->
                      List.fold_left
                        (fun acc comb -> (x::comb)::acc)
                        acc
                        (loop xss [] y))
                   acc
                   ys
  in match xss with
  | xs::xss -> List.fold_left (loop xss) [] xs
  | _ -> []

let mk_count () : unit -> int =
  let n = ref 0 in
  fun () -> let n0 = !n in n := n0 + 1 ; n0


(* IO / file manipulation *)

let write_file (content : string) (path : string) : bool =
  let chan = open_out_gen [Open_append; Open_creat; Open_binary] 0o644 path in
  let out =
    try output_string chan content; flush chan; true
    with exc -> print_endline (Printexc.to_string exc); false in
  close_out chan;
  (if not out then Printf.printf "Failed to write %s.\n" path) ;
  out

let read_file (path : string) : string option =
  if Sys.file_exists path
  then 
    let chan = open_in path in
    let rec slurp acc = match input_line chan with
      | line -> slurp (acc ^ "\n" ^ line)
      | exception End_of_file -> acc
    in
    let content = slurp "" in
    close_in chan ;
    Some content
  else
    None

let delete_file (path : string) : bool =
  not (Sys.file_exists path) ||
    (Sys.remove path; not (Sys.file_exists path))

let file_of_path path =
  try
    let len  = String.length path in
    let ind  = String.rindex_from path (len-1) '/' in
    String.sub path (ind+1) (len-ind-1)
  with Not_found -> path

let run_capture (command : string) : string option =
  let rec lines (ic : in_channel) (acc : string) : string =
    try  lines ic (acc ^ (input_line ic))
    with End_of_file -> acc in
  let stdout, stdin, stderr =
    Unix.open_process_full command (Array.make 0 "")
  in
  let outstr = lines stdout "" and errstr = lines stderr "" in
  match Unix.close_process_full (stdout, stdin, stderr) with
  | Unix.WEXITED 0 -> Some outstr
  | Unix.WEXITED n ->
    Printf.printf "Command (%s) exited with non-zero exit code (%i):\n%s\n%!"
      command n errstr;
    None
  | _ -> assert false

let bin_exists (bin : string) : bool =
  match run_capture ("command -v " ^ bin) with
  | Some _ -> true
  | None   -> false

let mkdirp ?(perm=0o775) str =
  List.fold_left
    (fun prefix dir ->
       let full = prefix^"/"^dir in
       if Sys.file_exists dir && Sys.is_directory dir
       then full
       else if Sys.file_exists dir
       then failwith "Cannot create directory with same name as file: %s."
       else (Unix.mkdir full perm ; full))
    "."
    (Str.split (Str.regexp "/") str)
