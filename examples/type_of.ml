(* open Typed_ppxlib_ocaml_parsing *)
open Typed_ppxlib_ocaml_typing
module Tt = Typedtree

let is_type_of ((name, _) : Tt.extension) = name.txt = "type_of"

let type_of_extension ~loc env (_, payload) =
  let open Parsetree in
  let expr =
    match payload with
    | PStr [ { pstr_desc = Pstr_eval (expr, _); _ } ] -> expr
    | _ -> failwith "%%type_of: invalid extension"
  in
  let texpr = Typecore.type_exp env expr in
  let typ = Format.asprintf "%a" Printtyp.type_expr texpr.exp_type in
  Typecore.type_exp
    env
    { pexp_desc = Pexp_constant (Pconst_string (typ, loc, None))
    ; pexp_loc = loc
    ; pexp_loc_stack = []
    ; pexp_attributes = []
    }


let type_of =
  let open Tast_mapper in
  let mapper =
    { default with
      expr =
        (fun this expr ->
          match expr.exp_desc with
          | Texp_extension ext when is_type_of ext ->
            type_of_extension ~loc:expr.exp_loc expr.exp_env ext
          | _ -> default.expr this expr)
    }
  in
  mapper.structure mapper


let () = Typed_ppxlib.register ~impl:type_of "ppx_type_of"
