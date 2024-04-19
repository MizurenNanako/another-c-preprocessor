module Context = struct
  (** Inneral representation for a macro *)
  module Macro = struct
    type body_t =
      | Concate of body_t * body_t (* Concatenation *)
      | Expand of string * body_t list
        (* Macro expand list, equiv to Tokenlist after expansion *)
      | Token of string (* Single token *)
      | SToken of body_t * char (* Stringfied token *)
      | Tokenlist of body_t list (* Single token *)
      | Whitespace (* Whitespace *)
      | Placeholder of int (* inneral rep, can be replaced with a Token *)
      | SPlaceholder of int (* inneral rep, can be replaced with a SToken *)
      | VA_ARGS (* inneral rep *)

    type t = { m_param_cnt : int; m_body : body_t; m_variadic : bool }

    (** simplify possible structure,
        such as flattening Tokenlists,
        returing a simplified [self]. *)
    let simplified (self : t) =
      let rec _sim (m : body_t) =
        match m with
        | Concate (l, r) -> Concate (_sim l, _sim r)
        | Expand (n, blst) -> Expand (n, List.map (fun k -> _sim k) blst)
        | SToken (blst, ch) -> SToken (_sim blst, ch)
        | Tokenlist lst ->
            let rec expand_lst (l' : body_t list) =
              let expand_elm (k' : body_t) =
                match k' with
                | Tokenlist l'' -> expand_lst l''
                | _ as k' -> [ k' ]
              in
              List.concat_map expand_elm l'
            in
            Tokenlist (expand_lst lst)
        | _ as k -> k
      in
      { self with m_body = _sim self.m_body }
  end

  module StringMap = Map.Make (String)

  type sym_table_t = Macro.t StringMap.t
  (** Type representing Map of pair (string, Macro.t) *)

  type t = { mutable sym_table : sym_table_t; mutable is_active : bool }
  (** Context.t, representing a preprocessing context.
      [sym_table]: (macro_name, macro) map.
      [is_active]: true->accept input | false->ignore all input *)

  exception MacroExpansionError of string
end
