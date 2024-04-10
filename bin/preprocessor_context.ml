module PPCtx = struct
  module Macro = struct
    type t = {
      m_trigger : Preprocessor_token.PPToken.pp_token;
      m_body : body list;
    }

    and body =
      | PLACEHOLDER of int (* Will be replaced when expand *)
      | TOKEN of Preprocessor_token.PPToken.pp_token (* Will remain *)

    let compare m1 m2 = Stdlib.compare m1.m_trigger m2.m_trigger

    let parse parameters replacement =
      let rec check_param param (nth : int) token =
        match param with
        | [] -> TOKEN token
        | hd :: tl ->
            if hd = token then PLACEHOLDER nth
            else check_param tl (nth + 1) token
      in
      let f = check_param parameters 1 in
      List.map f replacement
  end

  module SymbolTable = Set.Make (Macro)

  type t = { sym_table : SymbolTable.t }

  let make () = { sym_table = SymbolTable.empty }

  let has_symbol (self : t) sym =
    match
      SymbolTable.find_opt { m_trigger = sym; m_body = [] } self.sym_table
    with
    | Some _ -> true
    | None -> false

  let add_symbol (self : t) sym =
    {
      sym_table =
        SymbolTable.add { m_trigger = sym; m_body = [] } self.sym_table;
    }

  let add_macro_parsed (self : t) sym syms =
    {
      sym_table =
        SymbolTable.add { m_trigger = sym; m_body = syms } self.sym_table;
    }

  let add_macro (self : t) sym param reps =
    {
      sym_table =
        SymbolTable.add
          { m_trigger = sym; m_body = Macro.parse param reps }
          self.sym_table;
    }
end
