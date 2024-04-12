open Preprocessor_context
open Parsing

let () =
  let out_ch = Out_channel.open_text "output.out" in
  let context = PPCtx.make () in
  (try Parser.run context Sys.argv.(1) out_ch
   with _ as e ->
     Out_channel.flush out_ch;
     raise e);
  PPCtx.SymbolTable.iter
    (fun macro -> Printf.eprintf "macro%s\n" @@ PPCtx.Macro.repr macro)
    context.sym_table
