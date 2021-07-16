open Ppxlib
open Typed_ppxlib_ocaml_typing
include module type of Expansion_context

module Typed_extension : sig
  (** Type of expansion context for typed extensions *)
  type t

  (** Return the location of the extension point being expanded *)
  val extension_point_loc : t -> Location.t

  (** Return the code path for the given context *)
  val code_path : t -> Code_path.t

  (** Can be used within a ppx preprocessor to know which tool is
      calling it ["ocamlc"], ["ocamlopt"], ["ocamldep"], ["ocaml"], ... . *)
  val tool_name : t -> string

  (** Return whether the current context is within a function. 
      [None] if false, [Some (fun_loc, fun_typ)] if true, 
      where [fun_loc] is the location of the function and [fun_typ] is the 
      expected type of the function.
  *)
  val in_function : t -> (Location.t * Types.type_expr) option

  (* TODO: Understand [recarg] type. *)
  val recarg : t -> Typecore.recarg option

  (** Return the expected type of the typed context. *)
  val expected_type : t -> Typecore.type_expected

  (** Returns the local typing environment of the typed context. *)
  val env : t -> Env.t

  (** Build a new typed expansion context with the given typing information (See [Typed_ppxlib_ocaml_typing.Typecore])
      and an expansion context.
  *)
  val make
    :  ?in_function:Location.t * Types.type_expr
    -> ?recarg:Typecore.recarg
    -> expected_type:Typecore.type_expected
    -> env:Env.t
    -> extension:Extension.t
    -> unit
    -> t
end