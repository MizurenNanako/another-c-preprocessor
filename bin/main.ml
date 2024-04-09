open Preprocessor_token

let run lexbuf outch =
  let rec loop (t : PPType.pp_token) =
    match t with
    | Eof -> Printf.fprintf outch "%s\n" (PPType.token_ctx Eof)
    | _ as x ->
        Printf.fprintf outch "%s\n" (PPType.token_ctx x);
        loop (Lexer.pp_token lexbuf)
  in
  try loop (Lexer.pp_token lexbuf) with _ -> Out_channel.flush outch

let () =
  let inch = In_channel.open_text Sys.argv.(1) in
  let lexbuf = Lexing.from_channel inch in
  let outch = Out_channel.open_text "output.out" in
  run lexbuf outch
