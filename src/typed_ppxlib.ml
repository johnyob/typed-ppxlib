module Transform = struct
  open Typed_ppxlib_ocaml_driver
  open Typed_ppxlib_ocaml_typing

  let env =
    lazy
      (Compmisc.init_path ();
       Compmisc.initial_env ())


  let instance = ref (fun tstr -> tstr)

  let register transform =
    let super = !instance in
    instance := fun tstr -> super (transform tstr)


  let transform str =
    let env = Lazy.force_val env in
    let tstr, _, _, _ = Typemod.type_structure env str in
    let transform = !instance in
    Untypeast.untype_structure (transform tstr)
end

open Ppxlib

let registered = ref false

let register ?impl _name =
  if not !registered
  then (
    registered := true;
    Driver.register_transformation
      ~instrument:(Driver.Instrument.make ~position:After Transform.transform)
      ("typed_ppxlib"));
  match impl with
  | Some impl -> Transform.register impl
  | None -> () 
