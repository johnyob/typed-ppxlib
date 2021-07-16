open Ppxlib
open Typed_ppxlib_ocaml_typing
include Expansion_context

module Typed_extension = struct
  type t =
    { extension : Extension.t
    ; in_function : (Location.t * Types.type_expr) option
    ; recarg : Typecore.recarg option
    ; expected_type : Typecore.type_expected
    ; env : Env.t
    }

  let make ?in_function ?recarg ~expected_type ~env ~extension () =
    { extension; in_function; recarg; expected_type; env }


  let extension_point_loc t = Extension.extension_point_loc t.extension
  let code_path t = Extension.code_path t.extension
  let tool_name t = Extension.tool_name t.extension
  let in_function t = t.in_function
  let recarg t = t.recarg
  let expected_type t = t.expected_type 
  let env t = t.env
end