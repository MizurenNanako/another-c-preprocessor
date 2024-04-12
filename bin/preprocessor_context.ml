module PPCtx = struct
  open Preprocessor_token

  module Macro = struct
    exception ExpansionError of string

    type t = {
      m_trigger : string;
      m_body : body_token list;
      m_param_cnt : int;
      pos : Lexing.position;
    }

    and body_token =
      | PLACEHOLDER of int (* Will be replaced when expand *)
      | TOKEN of PPToken.pp_token (* Will remain *)
      | CONCATE
      | STRINGFY

    let dummy =
      { m_trigger = ""; m_body = []; m_param_cnt = 0; pos = Lexing.dummy_pos }

    let compare m1 m2 = Stdlib.compare m1.m_trigger m2.m_trigger

    let parse_from_tokens parameters replacement =
      let rec check_param param (nth : int) (token : PPToken.pp_token) =
        match param with
        | [] -> (
            match token with
            | Punctuator "##" -> CONCATE
            | Punctuator "#" -> STRINGFY
            | _ -> TOKEN token)
        | hd :: tl ->
            if hd = token then PLACEHOLDER nth
            else check_param tl (nth + 1) token
      in
      let f = check_param parameters 1 in
      List.map f replacement

    let parse_tokens_from_raw raw =
      let lexbuf = Lexing.from_string raw in
      let rec loop (tok : PPToken.pp_token) lst =
        match tok with
        | Eof -> lst
        | x -> loop (Lexer.pp_token lexbuf) (x :: lst)
      in
      loop (Lexer.pp_token lexbuf) []

    let _body_repr b =
      match b with
      | PLACEHOLDER n -> Printf.sprintf "_%d" n
      | TOKEN k -> Printf.sprintf "\"%s\"" (PPToken.token_repr k)
      | CONCATE -> "_CON_"
      | STRINGFY -> "_STR"

    let rec _body_list_repr body =
      match body with
      | [] -> ""
      | [ lst ] -> Printf.sprintf "%s" (_body_repr lst)
      | hd :: tl -> Printf.sprintf "%s, %s" (_body_repr hd) (_body_list_repr tl)

    let repr t =
      Printf.sprintf "<sym:\"%s\", body:[%s], len:%i, pos:\"%s:%i:%i\">"
        t.m_trigger (_body_list_repr t.m_body) t.m_param_cnt t.pos.pos_fname
        t.pos.pos_lnum
        (t.pos.pos_cnum - t.pos.pos_bol + 1)

    (** From params to body *)
    let _expand_to_body (t : t) (ptoks : PPToken.pp_token list list) =
      (* List.iter
         (fun l ->
           List.iter
             (fun k ->
               print_string (PPToken.token_repr k);
               print_string ",")
             l;
           print_newline ())
         ptoks; *)
      let f b =
        match b with
        | PLACEHOLDER n ->
            (* Printf.eprintf "(%i)" n; *)
            let target = List.nth ptoks (n - 1) in
            List.map (fun tk -> TOKEN tk) target
        | k -> [ k ]
      in
      List.concat_map f t.m_body

    let _body_to_tokens (body : body_token list) =
      let rec f b acc =
        match b with
        | [] -> acc
        | TOKEN k1 :: CONCATE :: TOKEN k2 :: tl ->
            f tl
              (match PPToken.token_concate k1 k2 with
              | Some k -> k :: acc
              | None -> k2 :: k1 :: acc)
        | STRINGFY :: TOKEN k :: tl -> (
            match k with
            | Identifier i -> f tl (PPstr ('\"', i) :: acc)
            | _ -> raise (ExpansionError "Not Stringfy-able"))
        | TOKEN k :: tl -> f tl (k :: acc)
        | b :: _ -> raise (ExpansionError (_body_repr b))
      in
      List.rev (f body [])

    let expand (t : t) (ptoks : PPToken.pp_token list list) =
      let tmp = _expand_to_body t ptoks in
      _body_to_tokens tmp
  end

  module SymbolTable = Set.Make (Macro)

  type t = { mutable sym_table : SymbolTable.t }

  let make () = { sym_table = SymbolTable.empty }

  let has_symbol (self : t) sym =
    SymbolTable.exists (fun x -> x.m_trigger = sym) self.sym_table

  let add_symbol (self : t) sym pos =
    self.sym_table <-
      SymbolTable.add
        { m_trigger = sym; m_body = []; m_param_cnt = 0; pos }
        self.sym_table

  let add_macro_parsed (self : t) sym syms pos =
    self.sym_table <-
      SymbolTable.add
        {
          m_trigger = sym;
          m_body = List.map (fun tk -> Macro.TOKEN tk) syms;
          m_param_cnt = 0;
          pos;
        }
        self.sym_table

  let add_macro (self : t) sym param reps pos =
    self.sym_table <-
      SymbolTable.add
        {
          m_trigger = sym;
          m_body = Macro.parse_from_tokens param reps;
          m_param_cnt = List.length param;
          pos;
        }
        self.sym_table

  let remove_symbol (self : t) sym =
    self.sym_table <-
      SymbolTable.filter (fun x -> x.m_trigger <> sym) self.sym_table
end
