open Typed_ppxlib_ocaml_typing

(** [register name hook] *)
val register :
  ?impl:(Typedtree.structure -> Typedtree.structure)
  -> string
  -> unit
