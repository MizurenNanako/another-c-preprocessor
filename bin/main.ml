open Preprocessor_context
open Resource_manager
open Parsing

let () =
  let usage_msg = "append [OPTIONS] <file1> [<file2>] ... -o <output>" in
  let input_files = ref [] in
  let output_file_name = ref "output.out" in
  let verbose_symbol_table = ref false in

  let speclist =
    [
      ( "-verbose-symbol-table",
        Arg.Set verbose_symbol_table,
        "Output macro information" );
      ("-o", Arg.Set_string output_file_name, "Set output file name");
    ]
  in

  let anon_fun filename = input_files := filename :: !input_files in

  Arg.parse speclist anon_fun usage_msg;
  let acppo input_file_name =
    let out_ch = Out_channel.open_text output_file_name.contents in
    let context = PPCtx.make () in
    let resource = Resource.make () in
    (try Parser.run context resource input_file_name out_ch
     with _ as e ->
       Out_channel.flush out_ch;
       raise e);
    if verbose_symbol_table.contents then (
      Printf.eprintf "Parsing done, Remaining macros: \n";
      PPCtx.SymbolTable.iter
        (fun macro -> Printf.eprintf "macro%s\n" @@ PPCtx.Macro.dump macro)
        context.sym_table)
  in
  List.iter acppo !input_files
