module Resource = struct
  type t = {
    mutable include_path : string list;
    mutable linescached : int;
        (* 0: no caches; >0: extra newlines after next newline*)
    mutable enabled : bool; (* true: normal; false: comments *)
  }

  let make () =
    {
      include_path = [ "/usr/local/include"; "/usr/include" ];
      linescached = 0;
      enabled = true;
    }

  let fetch_filename self is_sys name =
    let _vindicated s =
      if Sys.file_exists s then s else raise (Failure ("file not found: " ^ s))
    in
    if not (Filename.is_implicit name) then _vindicated name
    else
      let search_path =
        match is_sys with
        | true -> self.include_path
        | false -> Sys.getcwd () :: self.include_path
      in
      let path =
        List.find
          (fun p -> Sys.file_exists (Filename.concat p name))
          search_path
      in
      Filename.concat path name

  let get_reader_of_channel self input_channel buf n =
    let rec _loop_comment count =
      self.enabled <- false;
      if count = n then count
      else
        match In_channel.input_char input_channel with
        | Some '*' -> (
            match In_channel.input_char input_channel with
            | Some '/' ->
                self.enabled <- true;
                count
            | Some '\n' ->
                Bytes.set buf count '\n';
                _loop_comment (count + 1)
            | Some _ ->
                Bytes.set buf count ' ';
                Bytes.set buf (count + 1) ' ';
                _loop_comment (count + 2)
            | None -> count)
        | Some '\n' ->
            Bytes.set buf count '\n';
            _loop_comment (count + 1)
        | Some _ ->
            Bytes.set buf count ' ';
            _loop_comment (count + 1)
        | None -> count
    in
    let rec _loop count =
      if count = n then count
      else if not self.enabled then _loop_comment count
      else
        match In_channel.input_char input_channel with
        | Some '\\' -> (
            match In_channel.input_char input_channel with
            | Some '\n' ->
                self.linescached <- self.linescached + 1;
                _loop count
            | Some c ->
                Bytes.set buf count '\\';
                Bytes.set buf (count + 1) c;
                _loop (count + 2)
            | None -> count)
        | Some '/' -> (
            match In_channel.input_char input_channel with
            | Some '*' -> _loop (_loop_comment count)
            | Some c ->
                Bytes.set buf count '/';
                Bytes.set buf (count + 1) c;
                _loop (count + 2)
            | None -> count)
        | Some '\n' ->
            (* expand all cached lines *)
            let m = self.linescached in
            for i = count to count + m do
              Bytes.set buf i '\n'
            done;
            self.linescached <- 0;
            _loop (count + m + 1)
        | Some c ->
            Bytes.set buf count c;
            _loop (count + 1)
        | None -> count
    in
    _loop 0

  let lexbuf_from_filename self ?(system_header = false) filename =
    let fname = fetch_filename self system_header filename in
    let in_ch = In_channel.open_text fname in
    let reader = get_reader_of_channel self in_ch in
    let lexbuf = Lexing.from_function reader in
    Lexing.set_filename lexbuf fname;
    lexbuf
end
