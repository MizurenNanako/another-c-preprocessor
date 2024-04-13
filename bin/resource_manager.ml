module Resource = struct
  type t = { mutable include_path : string list }

  let make () = { include_path = [ "/usr/local/include"; "/usr/include" ] }

  let fetch_filename self is_sys name =
    let _vindicated s =
      if Sys.file_exists s then s else raise (Failure ("file not found: " ^ s))
    in
    if not (Filename.is_implicit name) then _vindicated name
    else
      let search_path =
        match is_sys with
        | true -> self.include_path
        | false -> "./" :: self.include_path
      in
      let path =
        List.find
          (fun p -> Sys.file_exists (Filename.concat p name))
          search_path
      in
      Filename.concat path name

  let reader_of_channel input_channel =
    let _g buf n =
      let rec _loop count =
        if count = n then count
        else
          match In_channel.input_char input_channel with
          | Some c ->
              Bytes.set buf count c;
              _loop (count + 1)
          | None -> count
      in
      let r = _loop 0 in
      r
    in
    _g

  let lexbuf_from_filename self ?(system_header = false) filename =
    let fname = fetch_filename self system_header filename in
    let in_ch = In_channel.open_text fname in
    let reader = reader_of_channel in_ch in
    let lexbuf = Lexing.from_function reader in
    Lexing.set_filename lexbuf fname;
    lexbuf
end
