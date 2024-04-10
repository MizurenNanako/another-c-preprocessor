open Preprocessor_token

let run lexbuf out_ch =
  let rec loop (t : PPToken.pp_token) =
    match t with
    | Eof -> Printf.fprintf out_ch "\n"
    | _ as x ->
        Printf.fprintf out_ch "%s" (PPToken.token_repr x);
        loop (Lexer.pp_token lexbuf)
  in
  try loop (Lexer.pp_token lexbuf)
  with _ as e ->
    Out_channel.flush out_ch;
    raise e

let () =
  let in_ch = In_channel.open_text Sys.argv.(1) in
  let lexbuf = Lexing.from_channel in_ch in
  let outch = Out_channel.open_text "output.out" in
  run lexbuf outch
