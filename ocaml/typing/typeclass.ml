(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*          Jerome Vouillon, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Parsetree
open Asttypes
open Path
open Types
open Typecore
open Typetexp
open Format

type 'a class_info = {
  cls_id : Ident.t;
  cls_id_loc : string loc;
  cls_decl : class_declaration;
  cls_ty_id : Ident.t;
  cls_ty_decl : class_type_declaration;
  cls_obj_id : Ident.t;
  cls_obj_abbr : type_declaration;
  cls_typesharp_id : Ident.t;
  cls_abbr : type_declaration;
  cls_arity : int;
  cls_pub_methods : string list;
  cls_info : 'a;
}

type class_type_info = {
  clsty_ty_id : Ident.t;
  clsty_id_loc : string loc;
  clsty_ty_decl : class_type_declaration;
  clsty_obj_id : Ident.t;
  clsty_obj_abbr : type_declaration;
  clsty_typesharp_id : Ident.t;
  clsty_abbr : type_declaration;
  clsty_info : Typedtree.class_type_declaration;
}

type 'a full_class = {
  id : Ident.t;
  id_loc : tag loc;
  clty: class_declaration;
  ty_id: Ident.t;
  cltydef: class_type_declaration;
  obj_id: Ident.t;
  obj_abbr: type_declaration;
  cl_id: Ident.t;
  cl_abbr: type_declaration;
  arity: int;
  pub_meths: string list;
  coe: Warnings.loc list;
  expr: 'a;
  req: 'a Typedtree.class_infos;
}

type class_env = { val_env : Env.t; met_env : Env.t; par_env : Env.t }

type error =
    Unconsistent_constraint of Ctype.Unification_trace.t
  | Field_type_mismatch of string * string * Ctype.Unification_trace.t
  | Structure_expected of class_type
  | Cannot_apply of class_type
  | Apply_wrong_label of arg_label
  | Pattern_type_clash of type_expr
  | Repeated_parameter
  | Unbound_class_2 of Longident.t
  | Unbound_class_type_2 of Longident.t
  | Abbrev_type_clash of type_expr * type_expr * type_expr
  | Constructor_type_mismatch of string * Ctype.Unification_trace.t
  | Virtual_class of bool * bool * string list * string list
  | Parameter_arity_mismatch of Longident.t * int * int
  | Parameter_mismatch of Ctype.Unification_trace.t
  | Bad_parameters of Ident.t * type_expr * type_expr
  | Class_match_failure of Ctype.class_match_failure list
  | Unbound_val of string
  | Unbound_type_var of (formatter -> unit) * Ctype.closed_class_failure
  | Non_generalizable_class of Ident.t * Types.class_declaration
  | Cannot_coerce_self of type_expr
  | Non_collapsable_conjunction of
      Ident.t * Types.class_declaration * Ctype.Unification_trace.t
  | Final_self_clash of Ctype.Unification_trace.t
  | Mutability_mismatch of string * mutable_flag
  | No_overriding of string * string
  | Duplicate of string * string
  | Closing_self_type of type_expr

exception Error of Location.t * Env.t * error
exception Error_forward of Location.error

(* typed_ppxlib *)

(* TODO: Remove duplicate definitions? *)
type class_field_acc =
  { class_env : class_env
  ; fields : Typedtree.class_field Lazy.t list
  ; concr_methods : Types.Concr.t
  ; warn_vals : Types.Concr.t
  ; inher : (Path.t * Types.type_expr list) list
  ; local_methods : Types.Concr.t
  ; local_vals : Types.Concr.t
  }

type class_type_field_acc =
  { fields : Typedtree.class_type_field list
  ; val_sig : (Asttypes.mutable_flag * Asttypes.virtual_flag * Types.type_expr) Types.Vars.t
  ; concr_methods : Types.Concr.t
  ; inher : (Path.t * Types.type_expr list) list
  }

let typed_ppxlib_class_expr_extension_ref = 
  ref (fun ~val_env:_ ~method_env:_ ext -> raise (Error_forward (Builtin_attributes.error_of_extension ext))
    : val_env:Env.t -> method_env:Env.t -> Parsetree.extension -> Typedtree.class_expr)

let typed_ppxlib_class_field_extension_ref = 
  ref (fun ~self:_ ~methods:_ ~vars:_ ~acc:_ ext -> raise (Error_forward (Builtin_attributes.error_of_extension ext))
    : self:Types.type_expr
    -> methods:(Ident.t * Types.type_expr) Types.Meths.t ref
    -> vars:
        (Ident.t * Asttypes.mutable_flag * Asttypes.virtual_flag * Types.type_expr) Types.Vars.t
        ref
    -> acc:class_field_acc
    -> Parsetree.extension
    -> class_field_acc)

let typed_ppxlib_class_type_extension_ref = 
  ref (fun ~env:_ ext ->  raise (Error_forward (Builtin_attributes.error_of_extension ext))
    : env:Env.t -> Parsetree.extension -> Typedtree.class_type)


let typed_ppxlib_class_type_field_extension_ref = 
  ref (fun ~env:_ ~self:_ ~methods:_ ~acc:_ ext -> raise (Error_forward (Builtin_attributes.error_of_extension ext))
    : env:Env.t
    -> self:Types.type_expr
    -> methods:(Ident.t * Types.type_expr) Types.Meths.t ref
    -> acc:class_type_field_acc
    -> Parsetree.extension
    -> class_type_field_acc)

open Typedtree

let type_open_descr :
  (?used_slot:bool ref -> Env.t -> Parsetree.open_description
   -> open_description * Env.t) ref =
  ref (fun ?used_slot:_ _ -> assert false)

let ctyp desc typ env loc =
  { ctyp_desc = desc; ctyp_type = typ; ctyp_loc = loc; ctyp_env = env;
    ctyp_attributes = [] }

                       (**********************)
                       (*  Useful constants  *)
                       (**********************)


(*
   Self type have a dummy private method, thus preventing it to become
   closed.
*)
let dummy_method = Btype.dummy_method

(*
   Path associated to the temporary class type of a class being typed
   (its constructor is not available).
*)
let unbound_class =
  Path.Pident (Ident.create_local "*undef*")


                (************************************)
                (*  Some operations on class types  *)
                (************************************)


(* Fully expand the head of a class type *)
let rec scrape_class_type =
  function
    Cty_constr (_, _, cty) -> scrape_class_type cty
  | cty                     -> cty

(* Generalize a class type *)
let rec generalize_class_type gen =
  function
    Cty_constr (_, params, cty) ->
      List.iter gen params;
      generalize_class_type gen cty
  | Cty_signature {csig_self = sty; csig_vars = vars; csig_inher = inher} ->
      gen sty;
      Vars.iter (fun _ (_, _, ty) -> gen ty) vars;
      List.iter (fun (_,tl) -> List.iter gen tl) inher
  | Cty_arrow (_, ty, cty) ->
      gen ty;
      generalize_class_type gen cty

let generalize_class_type vars =
  let gen = if vars then Ctype.generalize else Ctype.generalize_structure in
  generalize_class_type gen

(* Return the virtual methods of a class type *)
let virtual_methods sign =
  let (fields, _) =
    Ctype.flatten_fields (Ctype.object_fields sign.Types.csig_self)
  in
  List.fold_left
    (fun virt (lab, _, _) ->
       if lab = dummy_method then virt else
       if Concr.mem lab sign.csig_concr then virt else
       lab::virt)
    [] fields

(* Return the constructor type associated to a class type *)
let rec constructor_type constr cty =
  match cty with
    Cty_constr (_, _, cty) ->
      constructor_type constr cty
  | Cty_signature _ ->
      constr
  | Cty_arrow (l, ty, cty) ->
      Ctype.newty (Tarrow (l, ty, constructor_type constr cty, Cok))

let rec class_body cty =
  match cty with
    Cty_constr _ ->
      cty (* Only class bodies can be abbreviated *)
  | Cty_signature _ ->
      cty
  | Cty_arrow (_, _, cty) ->
      class_body cty

let extract_constraints cty =
  let sign = Ctype.signature_of_class_type cty in
  (Vars.fold (fun lab _ vars -> lab :: vars) sign.csig_vars [],
   begin let (fields, _) =
     Ctype.flatten_fields (Ctype.object_fields sign.csig_self)
   in
   List.fold_left
     (fun meths (lab, _, _) ->
        if lab = dummy_method then meths else lab::meths)
     [] fields
   end,
   sign.csig_concr)

let rec abbreviate_class_type path params cty =
  match cty with
    Cty_constr (_, _, _) | Cty_signature _ ->
      Cty_constr (path, params, cty)
  | Cty_arrow (l, ty, cty) ->
      Cty_arrow (l, ty, abbreviate_class_type path params cty)

(* Check that all type variables are generalizable *)
(* Use Env.empty to prevent expansion of recursively defined object types;
   cf. typing-poly/poly.ml *)
let rec closed_class_type =
  function
    Cty_constr (_, params, _) ->
      List.for_all (Ctype.closed_schema Env.empty) params
  | Cty_signature sign ->
      Ctype.closed_schema Env.empty sign.csig_self
        &&
      Vars.fold (fun _ (_, _, ty) cc -> Ctype.closed_schema Env.empty ty && cc)
        sign.csig_vars
        true
  | Cty_arrow (_, ty, cty) ->
      Ctype.closed_schema Env.empty ty
        &&
      closed_class_type cty

let closed_class cty =
  List.for_all (Ctype.closed_schema Env.empty) cty.cty_params
    &&
  closed_class_type cty.cty_type

let rec limited_generalize rv =
  function
    Cty_constr (_path, params, cty) ->
      List.iter (Ctype.limited_generalize rv) params;
      limited_generalize rv cty
  | Cty_signature sign ->
      Ctype.limited_generalize rv sign.csig_self;
      Vars.iter (fun _ (_, _, ty) -> Ctype.limited_generalize rv ty)
        sign.csig_vars;
      List.iter (fun (_, tl) -> List.iter (Ctype.limited_generalize rv) tl)
        sign.csig_inher
  | Cty_arrow (_, ty, cty) ->
      Ctype.limited_generalize rv ty;
      limited_generalize rv cty

(* Record a class type *)
let rc node =
  Cmt_format.add_saved_type (Cmt_format.Partial_class_expr node);
  node


                (***********************************)
                (*  Primitives for typing classes  *)
                (***********************************)


(* Enter a value in the method environment only *)
let enter_met_env ?check loc lab kind unbound_kind ty class_env =
  let {val_env; met_env; par_env} = class_env in
  let val_env = Env.enter_unbound_value lab unbound_kind val_env in
  let par_env = Env.enter_unbound_value lab unbound_kind par_env in
  let (id, met_env) =
    Env.enter_value ?check lab
      {val_type = ty; val_kind = kind;
       val_attributes = []; Types.val_loc = loc;
       val_uid = Uid.mk ~current_unit:(Env.get_unit_name ()); } met_env
  in
  let class_env = {val_env; met_env; par_env} in
  (id,class_env )

(* Enter an instance variable in the environment *)
let enter_val cl_num vars inh lab mut virt ty class_env loc =
  let val_env = class_env.val_env in
  let (id, virt) =
    try
      let (id, mut', virt', ty') = Vars.find lab !vars in
      if mut' <> mut then
        raise (Error(loc, val_env, Mutability_mismatch(lab, mut)));
      Ctype.unify val_env (Ctype.instance ty) (Ctype.instance ty');
      (if not inh then Some id else None),
      (if virt' = Concrete then virt' else virt)
    with
      Ctype.Unify tr ->
        raise (Error(loc, val_env,
                     Field_type_mismatch("instance variable", lab, tr)))
    | Not_found -> None, virt
  in
  let (id, _) as result =
    match id with Some id -> (id, class_env)
    | None ->
        enter_met_env Location.none lab (Val_ivar (mut, cl_num))
          Val_unbound_instance_variable ty class_env
  in
  vars := Vars.add lab (id, mut, virt, ty) !vars;
  result

let concr_vals vars =
  Vars.fold
    (fun id (_, vf, _) s -> if vf = Virtual then s else Concr.add id s)
    vars Concr.empty

let inheritance self_type env ovf concr_meths warn_vals loc parent =
  match scrape_class_type parent with
    Cty_signature cl_sig ->

      (* Methods *)
      begin try
        Ctype.unify env self_type cl_sig.csig_self
      with Ctype.Unify trace ->
        let open Ctype.Unification_trace in
        match trace with
        | Diff _ :: Incompatible_fields {name = n; _ } :: rem ->
            raise(Error(loc, env, Field_type_mismatch ("method", n, rem)))
        | _ -> assert false
      end;

      (* Overriding *)
      let over_meths = Concr.inter cl_sig.csig_concr concr_meths in
      let concr_vals = concr_vals cl_sig.csig_vars in
      let over_vals = Concr.inter concr_vals warn_vals in
      begin match ovf with
        Some Fresh ->
          let cname =
            match parent with
              Cty_constr (p, _, _) -> Path.name p
            | _ -> "inherited"
          in
          if not (Concr.is_empty over_meths) then
            Location.prerr_warning loc
              (Warnings.Method_override (cname :: Concr.elements over_meths));
          if not (Concr.is_empty over_vals) then
            Location.prerr_warning loc
              (Warnings.Instance_variable_override
                 (cname :: Concr.elements over_vals));
      | Some Override
        when Concr.is_empty over_meths && Concr.is_empty over_vals ->
        raise (Error(loc, env, No_overriding ("","")))
      | _ -> ()
      end;

      let concr_meths = Concr.union cl_sig.csig_concr concr_meths
      and warn_vals = Concr.union concr_vals warn_vals in

      (cl_sig, concr_meths, warn_vals)

  | _ ->
      raise(Error(loc, env, Structure_expected parent))

let virtual_method val_env meths self_type lab priv sty loc =
  let (_, ty') =
     Ctype.filter_self_method val_env lab priv meths self_type
  in
  let sty = Ast_helper.Typ.force_poly sty in
  let cty = transl_simple_type val_env false sty in
  let ty = cty.ctyp_type in
  begin
    try Ctype.unify val_env ty ty' with Ctype.Unify trace ->
        raise(Error(loc, val_env, Field_type_mismatch ("method", lab, trace)));
  end;
  cty

let delayed_meth_specs = ref []

let declare_method val_env meths self_type lab priv sty loc =
  let (_, ty') =
     Ctype.filter_self_method val_env lab priv meths self_type
  in
  let unif ty =
    try Ctype.unify val_env ty ty' with Ctype.Unify trace ->
      raise(Error(loc, val_env, Field_type_mismatch ("method", lab, trace)))
  in
  let sty = Ast_helper.Typ.force_poly sty in
  match sty.ptyp_desc, priv with
    Ptyp_poly ([],sty'), Public ->
(* TODO: we moved the [transl_simple_type_univars] outside of the lazy,
so that we can get an immediate value. Is that correct ? Ask Jacques. *)
      let returned_cty = ctyp Ttyp_any (Ctype.newty Tnil) val_env loc in
      delayed_meth_specs :=
      Warnings.mk_lazy (fun () ->
            let cty = transl_simple_type_univars val_env sty' in
            let ty = cty.ctyp_type in
            unif ty;
            returned_cty.ctyp_desc <- Ttyp_poly ([], cty);
            returned_cty.ctyp_type <- ty;
          ) ::
      !delayed_meth_specs;
      returned_cty
  | _ ->
      let cty = transl_simple_type val_env false sty in
      let ty = cty.ctyp_type in
      unif ty;
      cty

let type_constraint val_env sty sty' loc =
  let cty  = transl_simple_type val_env false sty in
  let ty = cty.ctyp_type in
  let cty' = transl_simple_type val_env false sty' in
  let ty' = cty'.ctyp_type in
  begin
    try Ctype.unify val_env ty ty' with Ctype.Unify trace ->
        raise(Error(loc, val_env, Unconsistent_constraint trace));
  end;
  (cty, cty')

let make_method loc cl_num expr =
  let open Ast_helper in
  let mkid s = mkloc s loc in
  Exp.fun_ ~loc:expr.pexp_loc Nolabel None
    (Pat.alias ~loc (Pat.var ~loc (mkid "self-*")) (mkid ("self-" ^ cl_num)))
    expr

(*******************************)

let add_val lab (mut, virt, ty) val_sig =
  let virt =
    try
      let (_mut', virt', _ty') = Vars.find lab val_sig in
      if virt' = Concrete then virt' else virt
    with Not_found -> virt
  in
  Vars.add lab (mut, virt, ty) val_sig

let rec class_type_field env self_type meths arg ctf =
  Builtin_attributes.warning_scope ctf.pctf_attributes
    (fun () -> class_type_field_aux env self_type meths arg ctf)

and class_type_field_aux env self_type meths
    (fields, val_sig, concr_meths, inher) ctf =

  let loc = ctf.pctf_loc in
  let mkctf desc =
    { ctf_desc = desc; ctf_loc = loc; ctf_attributes = ctf.pctf_attributes }
  in
  match ctf.pctf_desc with
    Pctf_inherit sparent ->
      let parent = class_type env sparent in
      let inher =
        match parent.cltyp_type with
          Cty_constr (p, tl, _) -> (p, tl) :: inher
        | _ -> inher
      in
      let (cl_sig, concr_meths, _) =
        inheritance self_type env None concr_meths Concr.empty sparent.pcty_loc
          parent.cltyp_type
      in
      let val_sig =
        Vars.fold add_val cl_sig.csig_vars val_sig in
      (mkctf (Tctf_inherit parent) :: fields,
       val_sig, concr_meths, inher)

  | Pctf_val ({txt=lab}, mut, virt, sty) ->
      let cty = transl_simple_type env false sty in
      let ty = cty.ctyp_type in
      (mkctf (Tctf_val (lab, mut, virt, cty)) :: fields,
      add_val lab (mut, virt, ty) val_sig, concr_meths, inher)

  | Pctf_method ({txt=lab}, priv, virt, sty)  ->
      let cty =
        declare_method env meths self_type lab priv sty  ctf.pctf_loc in
      let concr_meths =
        match virt with
        | Concrete -> Concr.add lab concr_meths
        | Virtual -> concr_meths
      in
      (mkctf (Tctf_method (lab, priv, virt, cty)) :: fields,
        val_sig, concr_meths, inher)

  | Pctf_constraint (sty, sty') ->
      let (cty, cty') = type_constraint env sty sty'  ctf.pctf_loc in
      (mkctf (Tctf_constraint (cty, cty')) :: fields,
        val_sig, concr_meths, inher)

  | Pctf_attribute x ->
      Builtin_attributes.warning_attribute x;
      (mkctf (Tctf_attribute x) :: fields,
        val_sig, concr_meths, inher)

  | Pctf_extension ext ->
      (* typed_ppxlib *)
      let {fields; val_sig; concr_methods; inher} = 
        !typed_ppxlib_class_type_field_extension_ref 
        ~env
        ~self:self_type 
        ~methods:meths 
        ~acc:{fields; val_sig; concr_methods=concr_meths; inher } 
        ext 
      in (fields, val_sig, concr_methods, inher)


and class_signature env {pcsig_self=sty; pcsig_fields=sign} =
  let meths = ref Meths.empty in
  let self_cty = transl_simple_type env false sty in
  let self_cty = { self_cty with
    ctyp_type = Ctype.expand_head env self_cty.ctyp_type } in
  let self_type =  self_cty.ctyp_type in

  (* Check that the binder is a correct type, and introduce a dummy
     method preventing self type from being closed. *)
  let dummy_obj = Ctype.newvar () in
  Ctype.unify env (Ctype.filter_method env dummy_method Private dummy_obj)
    (Ctype.newty (Ttuple []));
  begin try
    Ctype.unify env self_type dummy_obj
  with Ctype.Unify _ ->
    raise(Error(sty.ptyp_loc, env, Pattern_type_clash self_type))
  end;

  (* Class type fields *)
  let (rev_fields, val_sig, concr_meths, inher) =
    Builtin_attributes.warning_scope []
      (fun () ->
         List.fold_left (class_type_field env self_type meths)
           ([], Vars.empty, Concr.empty, [])
           sign
      )
  in
  let cty =   {csig_self = self_type;
   csig_vars = val_sig;
   csig_concr = concr_meths;
   csig_inher = inher}
  in
  { csig_self = self_cty;
    csig_fields = List.rev rev_fields;
    csig_type = cty;
  }

and class_type env scty =
  Builtin_attributes.warning_scope scty.pcty_attributes
    (fun () -> class_type_aux env scty)

and class_type_aux env scty =
  let cltyp desc typ =
    {
     cltyp_desc = desc;
     cltyp_type = typ;
     cltyp_loc = scty.pcty_loc;
     cltyp_env = env;
     cltyp_attributes = scty.pcty_attributes;
    }
  in
  match scty.pcty_desc with
    Pcty_constr (lid, styl) ->
      let (path, decl) = Env.lookup_cltype ~loc:scty.pcty_loc lid.txt env in
      if Path.same decl.clty_path unbound_class then
        raise(Error(scty.pcty_loc, env, Unbound_class_type_2 lid.txt));
      let (params, clty) =
        Ctype.instance_class decl.clty_params decl.clty_type
      in
      if List.length params <> List.length styl then
        raise(Error(scty.pcty_loc, env,
                    Parameter_arity_mismatch (lid.txt, List.length params,
                                                   List.length styl)));
      let ctys = List.map2
        (fun sty ty ->
          let cty' = transl_simple_type env false sty in
          let ty' = cty'.ctyp_type in
          begin
           try Ctype.unify env ty' ty with Ctype.Unify trace ->
                  raise(Error(sty.ptyp_loc, env, Parameter_mismatch trace))
            end;
            cty'
        )       styl params
      in
      let typ = Cty_constr (path, params, clty) in
      cltyp (Tcty_constr ( path, lid , ctys)) typ

  | Pcty_signature pcsig ->
      let clsig = class_signature env pcsig in
      let typ = Cty_signature clsig.csig_type in
      cltyp (Tcty_signature clsig) typ

  | Pcty_arrow (l, sty, scty) ->
      let cty = transl_simple_type env false sty in
      let ty = cty.ctyp_type in
      let ty =
        if Btype.is_optional l
        then Ctype.newty (Tconstr(Predef.path_option,[ty], ref Mnil))
        else ty in
      let clty = class_type env scty in
      let typ = Cty_arrow (l, ty, clty.cltyp_type) in
      cltyp (Tcty_arrow (l, cty, clty)) typ

  | Pcty_open (od, e) ->
      let (od, newenv) = !type_open_descr env od in
      let clty = class_type newenv e in
      cltyp (Tcty_open (od, clty)) clty.cltyp_type

  | Pcty_extension ext ->
      (* typed_ppxlib *)
      !typed_ppxlib_class_type_extension_ref ~env ext

let class_type env scty =
  delayed_meth_specs := [];
  let cty = class_type env scty in
  List.iter Lazy.force (List.rev !delayed_meth_specs);
  delayed_meth_specs := [];
  cty

(*******************************)

let rec class_field self_loc cl_num self_type meths vars arg cf =
  Builtin_attributes.warning_scope cf.pcf_attributes
    (fun () -> class_field_aux self_loc cl_num self_type meths vars arg cf)

and class_field_aux self_loc cl_num self_type meths vars
    (class_env, fields, concr_meths, warn_vals, inher,
     local_meths, local_vals) cf =
  let loc = cf.pcf_loc in
  let mkcf desc =
    { cf_desc = desc; cf_loc = loc; cf_attributes = cf.pcf_attributes }
  in
  let {val_env; met_env; par_env} = class_env in
  match cf.pcf_desc with
    Pcf_inherit (ovf, sparent, super) ->
      let parent = class_expr cl_num val_env par_env sparent in
      let inher =
        match parent.cl_type with
          Cty_constr (p, tl, _) -> (p, tl) :: inher
        | _ -> inher
      in
      let (cl_sig, concr_meths, warn_vals) =
        inheritance self_type val_env (Some ovf) concr_meths warn_vals
          sparent.pcl_loc parent.cl_type
      in
      (* Variables *)
      let (class_env, inh_vars) =
        Vars.fold
          (fun lab info (class_env, inh_vars) ->
             let mut, vr, ty = info in
             let (id, class_env) =
                enter_val cl_num vars true lab mut vr ty class_env
                 sparent.pcl_loc ;
             in
             (class_env, (lab, id) :: inh_vars))
          cl_sig.csig_vars (class_env, [])
      in
      (* Inherited concrete methods *)
      let inh_meths =
        Concr.fold (fun lab rem -> (lab, Ident.create_local lab)::rem)
          cl_sig.csig_concr []
      in
      (* Super *)
      let (class_env,super) =
        match super with
          None ->
            (class_env,None)
        | Some {txt=name} ->
            let (_id, class_env) =
              enter_met_env ~check:(fun s -> Warnings.Unused_ancestor s)
                sparent.pcl_loc name (Val_anc (inh_meths, cl_num))
                Val_unbound_ancestor self_type class_env
            in
            (class_env,Some name)
      in
      (class_env,
       lazy (mkcf (Tcf_inherit (ovf, parent, super, inh_vars, inh_meths)))
       :: fields,
       concr_meths, warn_vals, inher, local_meths, local_vals)

  | Pcf_val (lab, mut, Cfk_virtual styp) ->
      if !Clflags.principal then Ctype.begin_def ();
      let cty = Typetexp.transl_simple_type val_env false styp in
      let ty = cty.ctyp_type in
      if !Clflags.principal then begin
        Ctype.end_def ();
        Ctype.generalize_structure ty
      end;
      let (id, class_env') =
        enter_val cl_num vars false lab.txt mut Virtual ty
        class_env loc
      in
      (class_env',
       lazy (mkcf (Tcf_val (lab, mut, id, Tcfk_virtual cty,
                            met_env == class_env'.met_env)))
             :: fields,
             concr_meths, warn_vals, inher, local_meths, local_vals)

  | Pcf_val (lab, mut, Cfk_concrete (ovf, sexp)) ->
      if Concr.mem lab.txt local_vals then
        raise(Error(loc, val_env, Duplicate ("instance variable", lab.txt)));
      if Concr.mem lab.txt warn_vals then begin
        if ovf = Fresh then
          Location.prerr_warning lab.loc
            (Warnings.Instance_variable_override[lab.txt])
      end else begin
        if ovf = Override then
          raise(Error(loc, val_env,
                      No_overriding ("instance variable", lab.txt)))
      end;
      if !Clflags.principal then Ctype.begin_def ();
      let exp = type_exp val_env sexp in
      if !Clflags.principal then begin
        Ctype.end_def ();
        Ctype.generalize_structure exp.exp_type
       end;
      let (id, class_env') =
        enter_val cl_num vars false lab.txt mut Concrete exp.exp_type
        class_env loc
      in
      (class_env',
       lazy (mkcf (Tcf_val (lab, mut, id,
                    Tcfk_concrete (ovf, exp), met_env == class_env'.met_env)))
       :: fields,
       concr_meths, Concr.add lab.txt warn_vals, inher, local_meths,
       Concr.add lab.txt local_vals)

  | Pcf_method (lab, priv, Cfk_virtual sty) ->
      let cty = virtual_method val_env meths self_type lab.txt priv sty loc in
      (class_env,
        lazy (mkcf(Tcf_method (lab, priv, Tcfk_virtual cty)))
       ::fields,
        concr_meths, warn_vals, inher, local_meths, local_vals)

  | Pcf_method (lab, priv, Cfk_concrete (ovf, expr))  ->
      let expr =
        match expr.pexp_desc with
        | Pexp_poly _ -> expr
        | _ -> Ast_helper.Exp.poly ~loc:expr.pexp_loc expr None
      in
      if Concr.mem lab.txt local_meths then
        raise(Error(loc, val_env, Duplicate ("method", lab.txt)));
      if Concr.mem lab.txt concr_meths then begin
        if ovf = Fresh then
          Location.prerr_warning loc (Warnings.Method_override [lab.txt])
      end else begin
        if ovf = Override then
          raise(Error(loc, val_env, No_overriding("method", lab.txt)))
      end;
      let (_, ty) =
        Ctype.filter_self_method val_env lab.txt priv meths self_type
      in
      begin try match expr.pexp_desc with
        Pexp_poly (sbody, sty) ->
          begin match sty with None -> ()
                | Some sty ->
                    let sty = Ast_helper.Typ.force_poly sty in
                    let cty' = Typetexp.transl_simple_type val_env false sty in
                    let ty' = cty'.ctyp_type in
              Ctype.unify val_env ty' ty
          end;
          begin match (Ctype.repr ty).desc with
            Tvar _ ->
              let ty' = Ctype.newvar () in
              Ctype.unify val_env (Ctype.newty (Tpoly (ty', []))) ty;
              Ctype.unify val_env (type_approx val_env sbody) ty'
          | Tpoly (ty1, tl) ->
              let _, ty1' = Ctype.instance_poly false tl ty1 in
              let ty2 = type_approx val_env sbody in
              Ctype.unify val_env ty2 ty1'
          | _ -> assert false
          end
      | _ -> assert false
      with Ctype.Unify trace ->
        raise(Error(loc, val_env,
                    Field_type_mismatch ("method", lab.txt, trace)))
      end;
      let meth_expr = make_method self_loc cl_num expr in
      (* backup variables for Pexp_override *)
      let vars_local = !vars in

      let field =
        Warnings.mk_lazy
          (fun () ->
             (* Read the generalized type *)
             let (_, ty) = Meths.find lab.txt !meths in
             let meth_type = mk_expected (
               Btype.newgenty (Tarrow(Nolabel, self_type, ty, Cok))
             ) in
             Ctype.raise_nongen_level ();
             vars := vars_local;
             let texp = type_expect met_env meth_expr meth_type in
             Ctype.end_def ();
             mkcf (Tcf_method (lab, priv, Tcfk_concrete (ovf, texp)))
          )
      in
      (class_env, field::fields,
       Concr.add lab.txt concr_meths, warn_vals, inher,
       Concr.add lab.txt local_meths, local_vals)

  | Pcf_constraint (sty, sty') ->
      let (cty, cty') = type_constraint val_env sty sty' loc in
      (class_env,
        lazy (mkcf (Tcf_constraint (cty, cty'))) :: fields,
        concr_meths, warn_vals, inher, local_meths, local_vals)

  | Pcf_initializer expr ->
      let expr = make_method self_loc cl_num expr in
      let vars_local = !vars in
      let field =
        lazy begin
          Ctype.raise_nongen_level ();
          let meth_type = mk_expected (
            Ctype.newty
              (Tarrow (Nolabel, self_type,
                       Ctype.instance Predef.type_unit, Cok))
          ) in
          vars := vars_local;
          let texp = type_expect met_env expr meth_type in
          Ctype.end_def ();
          mkcf (Tcf_initializer texp)
        end in
      (class_env, field::fields, concr_meths, warn_vals,
       inher, local_meths, local_vals)
  | Pcf_attribute x ->
      Builtin_attributes.warning_attribute x;
      (class_env,
        lazy (mkcf (Tcf_attribute x)) :: fields,
        concr_meths, warn_vals, inher, local_meths, local_vals)
  | Pcf_extension ext ->
      (* typed_ppxlib *)
      let { class_env; fields; concr_methods; warn_vals; inher; local_methods; local_vals } = 
        !typed_ppxlib_class_field_extension_ref
        ~self:self_type
        ~methods:meths
        ~vars
        ~acc:{ class_env; fields; concr_methods=concr_meths; warn_vals; inher; local_methods=local_meths; local_vals }
        ext
      in (class_env, fields, concr_methods, warn_vals, inher, local_methods, local_vals) 

(* N.B. the self type of a final object type doesn't contain a dummy method in
   the beginning.
   We only explicitly add a dummy method to class definitions (and class (type)
   declarations)), which are later removed (made absent) by [final_decl].

   If we ever find a dummy method in a final object self type, it means that
   somehow we've unified the self type of the object with the self type of a not
   yet finished class.
   When this happens, we cannot close the object type and must error. *)
and class_structure cl_num final val_env met_env loc
  { pcstr_self = spat; pcstr_fields = str } =
  (* Environment for substructures *)
  let par_env = met_env in

  (* Location of self. Used for locations of self arguments *)
  let self_loc = {spat.ppat_loc with Location.loc_ghost = true} in

  let self_type = Ctype.newobj (Ctype.newvar ()) in

  (* Adding a dummy method to the self type prevents it from being closed /
     escaping.
     That isn't needed for objects though. *)
  if not final then
    Ctype.unify val_env
      (Ctype.filter_method val_env dummy_method Private self_type)
      (Ctype.newty (Ttuple []));

  (* Private self is used for private method calls *)
  let private_self = if final then Ctype.newvar () else self_type in

  (* Self binder *)
  let (pat, meths, vars, val_env, met_env, par_env) =
    type_self_pattern cl_num private_self val_env met_env par_env spat
  in
  let public_self = pat.pat_type in

  (* Check that the binder has a correct type *)
  let ty =
    if final then Ctype.newobj (Ctype.newvar()) else self_type in
  begin try Ctype.unify val_env public_self ty with
    Ctype.Unify _ ->
      raise(Error(spat.ppat_loc, val_env, Pattern_type_clash public_self))
  end;
  let get_methods ty =
    (fst (Ctype.flatten_fields
            (Ctype.object_fields (Ctype.expand_head val_env ty)))) in
  if final then begin
    (* Copy known information to still empty self_type *)
    List.iter
      (fun (lab,kind,ty) ->
        let k =
          if Btype.field_kind_repr kind = Fpresent then Public else Private in
        try Ctype.unify val_env ty
            (Ctype.filter_method val_env lab k self_type)
        with _ -> assert false)
      (get_methods public_self)
  end;

  (* Typing of class fields *)
  let class_env = {val_env; met_env; par_env} in
  let (_, fields, concr_meths, _, inher, _local_meths, _local_vals) =
    Builtin_attributes.warning_scope []
      (fun () ->
         List.fold_left (class_field self_loc cl_num self_type meths vars)
           ( class_env,[], Concr.empty, Concr.empty, [],
            Concr.empty, Concr.empty)
           str
      )
  in
  Ctype.unify val_env self_type (Ctype.newvar ()); (* useless ? *)
  let sign =
    {csig_self = public_self;
     csig_vars = Vars.map (fun (_id, mut, vr, ty) -> (mut, vr, ty)) !vars;
     csig_concr = concr_meths;
      csig_inher = inher} in
  let methods = get_methods self_type in
  let priv_meths =
    List.filter (fun (_,kind,_) -> Btype.field_kind_repr kind <> Fpresent)
      methods in
  (* ensure that inherited methods are listed too *)
  List.iter (fun (met, _kind, _ty) ->
      if Meths.mem met !meths then () else
      ignore (Ctype.filter_self_method val_env met Private meths self_type))
    methods;
  if final then begin
    (* Unify private_self and a copy of self_type. self_type will not
       be modified after this point *)
    if not (Ctype.close_object self_type) then
      raise(Error(loc, val_env, Closing_self_type self_type));
    let mets = virtual_methods {sign with csig_self = self_type} in
    let vals =
      Vars.fold
        (fun name (_mut, vr, _ty) l -> if vr = Virtual then name :: l else l)
        sign.csig_vars [] in
    if mets <> [] || vals <> [] then
      raise(Error(loc, val_env, Virtual_class(true, final, mets, vals)));
    let self_methods =
      List.fold_right
        (fun (lab,kind,ty) rem ->
           Ctype.newty(Tfield(lab, Btype.copy_kind kind, ty, rem)))
        methods (Ctype.newty Tnil) in
    begin try
      Ctype.unify val_env private_self
        (Ctype.newty (Tobject(self_methods, ref None)));
      Ctype.unify val_env public_self self_type
    with Ctype.Unify trace -> raise(Error(loc, val_env, Final_self_clash trace))
    end;
  end;

  (* Typing of method bodies *)
  (* if !Clflags.principal then *) begin
    let ms = !meths in
    (* Generalize the spine of methods accessed through self *)
    Meths.iter (fun _ (_,ty) -> Ctype.generalize_spine ty) ms;
    meths :=
      Meths.map (fun (id,ty) -> (id, Ctype.generic_instance ty)) ms;
    (* But keep levels correct on the type of self *)
    Meths.iter (fun _ (_,ty) -> Ctype.unify val_env ty (Ctype.newvar ())) ms
  end;
  let fields = List.map Lazy.force (List.rev fields) in
  let meths = Meths.map (function (id, _ty) -> id) !meths in

  (* Check for private methods made public *)
  let pub_meths' =
    List.filter (fun (_,kind,_) -> Btype.field_kind_repr kind = Fpresent)
      (get_methods public_self) in
  let names = List.map (fun (x,_,_) -> x) in
  let l1 = names priv_meths and l2 = names pub_meths' in
  let added = List.filter (fun x -> List.mem x l1) l2 in
  if added <> [] then
    Location.prerr_warning loc (Warnings.Implicit_public_methods added);
  let sign = if final then sign else
      {sign with Types.csig_self = Ctype.expand_head val_env public_self} in
  {
    cstr_self = pat;
    cstr_fields = fields;
    cstr_type = sign;
    cstr_meths = meths}, sign (* redondant, since already in cstr_type *)

and class_expr cl_num val_env met_env scl =
  Builtin_attributes.warning_scope scl.pcl_attributes
    (fun () -> class_expr_aux cl_num val_env met_env scl)

and class_expr_aux cl_num val_env met_env scl =
  match scl.pcl_desc with
    Pcl_constr (lid, styl) ->
      let (path, decl) = Env.lookup_class ~loc:scl.pcl_loc lid.txt val_env in
      if Path.same decl.cty_path unbound_class then
        raise(Error(scl.pcl_loc, val_env, Unbound_class_2 lid.txt));
      let tyl = List.map
          (fun sty -> transl_simple_type val_env false sty)
          styl
      in
      let (params, clty) =
        Ctype.instance_class decl.cty_params decl.cty_type
      in
      let clty' = abbreviate_class_type path params clty in
      if List.length params <> List.length tyl then
        raise(Error(scl.pcl_loc, val_env,
                    Parameter_arity_mismatch (lid.txt, List.length params,
                                                   List.length tyl)));
      List.iter2
        (fun cty' ty ->
          let ty' = cty'.ctyp_type in
           try Ctype.unify val_env ty' ty with Ctype.Unify trace ->
             raise(Error(cty'.ctyp_loc, val_env, Parameter_mismatch trace)))
        tyl params;
      let cl =
        rc {cl_desc = Tcl_ident (path, lid, tyl);
            cl_loc = scl.pcl_loc;
            cl_type = clty';
            cl_env = val_env;
            cl_attributes = scl.pcl_attributes;
           }
      in
      let (vals, meths, concrs) = extract_constraints clty in
      rc {cl_desc = Tcl_constraint (cl, None, vals, meths, concrs);
          cl_loc = scl.pcl_loc;
          cl_type = clty';
          cl_env = val_env;
          cl_attributes = []; (* attributes are kept on the inner cl node *)
         }
  | Pcl_structure cl_str ->
      let (desc, ty) =
        class_structure cl_num false val_env met_env scl.pcl_loc cl_str in
      rc {cl_desc = Tcl_structure desc;
          cl_loc = scl.pcl_loc;
          cl_type = Cty_signature ty;
          cl_env = val_env;
          cl_attributes = scl.pcl_attributes;
         }
  | Pcl_fun (l, Some default, spat, sbody) ->
      let loc = default.pexp_loc in
      let open Ast_helper in
      let scases = [
        Exp.case
          (Pat.construct ~loc
             (mknoloc (Longident.(Ldot (Lident "*predef*", "Some"))))
             (Some (Pat.var ~loc (mknoloc "*sth*"))))
          (Exp.ident ~loc (mknoloc (Longident.Lident "*sth*")));

        Exp.case
          (Pat.construct ~loc
             (mknoloc (Longident.(Ldot (Lident "*predef*", "None"))))
             None)
          default;
       ]
      in
      let smatch =
        Exp.match_ ~loc (Exp.ident ~loc (mknoloc (Longident.Lident "*opt*")))
          scases
      in
      let sfun =
        Cl.fun_ ~loc:scl.pcl_loc
          l None
          (Pat.var ~loc (mknoloc "*opt*"))
          (Cl.let_ ~loc:scl.pcl_loc Nonrecursive [Vb.mk spat smatch] sbody)
          (* Note: we don't put the '#default' attribute, as it
             is not detected for class-level let bindings.  See #5975.*)
      in
      class_expr cl_num val_env met_env sfun
  | Pcl_fun (l, None, spat, scl') ->
      if !Clflags.principal then Ctype.begin_def ();
      let (pat, pv, val_env', met_env) =
        Typecore.type_class_arg_pattern cl_num val_env met_env l spat
      in
      if !Clflags.principal then begin
        Ctype.end_def ();
        let gen {pat_type = ty} = Ctype.generalize_structure ty in
        iter_pattern gen pat
      end;
      let pv =
        List.map
          begin fun (id, id', _ty) ->
            let path = Pident id' in
            (* do not mark the value as being used *)
            let vd = Env.find_value path val_env' in
            (id,
             {exp_desc =
              Texp_ident(path, mknoloc (Longident.Lident (Ident.name id)), vd);
              exp_loc = Location.none; exp_extra = [];
              exp_type = Ctype.instance vd.val_type;
              exp_attributes = []; (* check *)
              exp_env = val_env'})
          end
          pv
      in
      let rec not_nolabel_function = function
        | Cty_arrow(Nolabel, _, _) -> false
        | Cty_arrow(_, _, cty) -> not_nolabel_function cty
        | _ -> true
      in
      let partial =
        let dummy = type_exp val_env (Ast_helper.Exp.unreachable ()) in
        Typecore.check_partial val_env pat.pat_type pat.pat_loc
          [{c_lhs = pat; c_guard = None; c_rhs = dummy}]
      in
      Ctype.raise_nongen_level ();
      let cl = class_expr cl_num val_env' met_env scl' in
      Ctype.end_def ();
      if Btype.is_optional l && not_nolabel_function cl.cl_type then
        Location.prerr_warning pat.pat_loc
          Warnings.Unerasable_optional_argument;
      rc {cl_desc = Tcl_fun (l, pat, pv, cl, partial);
          cl_loc = scl.pcl_loc;
          cl_type = Cty_arrow
            (l, Ctype.instance pat.pat_type, cl.cl_type);
          cl_env = val_env;
          cl_attributes = scl.pcl_attributes;
         }
  | Pcl_apply (scl', sargs) ->
      assert (sargs <> []);
      if !Clflags.principal then Ctype.begin_def ();
      let cl = class_expr cl_num val_env met_env scl' in
      if !Clflags.principal then begin
        Ctype.end_def ();
        generalize_class_type false cl.cl_type;
      end;
      let rec nonopt_labels ls ty_fun =
        match ty_fun with
        | Cty_arrow (l, _, ty_res) ->
            if Btype.is_optional l then nonopt_labels ls ty_res
            else nonopt_labels (l::ls) ty_res
        | _    -> ls
      in
      let ignore_labels =
        !Clflags.classic ||
        let labels = nonopt_labels [] cl.cl_type in
        List.length labels = List.length sargs &&
        List.for_all (fun (l,_) -> l = Nolabel) sargs &&
        List.exists (fun l -> l <> Nolabel) labels &&
        begin
          Location.prerr_warning
            cl.cl_loc
            (Warnings.Labels_omitted
               (List.map Printtyp.string_of_label
                         (List.filter ((<>) Nolabel) labels)));
          true
        end
      in
      let rec type_args args omitted ty_fun ty_fun0 sargs =
        match ty_fun, ty_fun0 with
        | Cty_arrow (l, ty, ty_fun), Cty_arrow (_, ty0, ty_fun0)
          when sargs <> [] ->
            let name = Btype.label_name l
            and optional = Btype.is_optional l in
            let use_arg sarg l' =
              Some (
                if not optional || Btype.is_optional l' then
                  type_argument val_env sarg ty ty0
                else
                  let ty' = extract_option_type val_env ty
                  and ty0' = extract_option_type val_env ty0 in
                  let arg = type_argument val_env sarg ty' ty0' in
                  option_some val_env arg
              )
            in
            let eliminate_optional_arg () =
              Some (option_none val_env ty0 Location.none)
            in
            let remaining_sargs, arg =
              if ignore_labels then begin
                match sargs with
                | [] -> assert false
                | (l', sarg) :: remaining_sargs ->
                    if name = Btype.label_name l' ||
                       (not optional && l' = Nolabel)
                    then
                      (remaining_sargs, use_arg sarg l')
                    else if
                      optional &&
                      not (List.exists (fun (l, _) -> name = Btype.label_name l)
                             remaining_sargs)
                    then
                      (sargs, eliminate_optional_arg ())
                    else
                      raise(Error(sarg.pexp_loc, val_env, Apply_wrong_label l'))
              end else
                match Btype.extract_label name sargs with
                | Some (l', sarg, _, remaining_sargs) ->
                    if not optional && Btype.is_optional l' then
                      Location.prerr_warning sarg.pexp_loc
                        (Warnings.Nonoptional_label
                           (Printtyp.string_of_label l));
                    remaining_sargs, use_arg sarg l'
                | None ->
                    sargs,
                    if Btype.is_optional l && List.mem_assoc Nolabel sargs then
                      eliminate_optional_arg ()
                    else
                      None
            in
            let omitted = if arg = None then (l,ty0) :: omitted else omitted in
            type_args ((l,arg)::args) omitted ty_fun ty_fun0 remaining_sargs
        | _ ->
            match sargs with
              (l, sarg0)::_ ->
                if omitted <> [] then
                  raise(Error(sarg0.pexp_loc, val_env, Apply_wrong_label l))
                else
                  raise(Error(cl.cl_loc, val_env, Cannot_apply cl.cl_type))
            | [] ->
                (List.rev args,
                 List.fold_left
                   (fun ty_fun (l,ty) -> Cty_arrow(l,ty,ty_fun))
                   ty_fun0 omitted)
      in
      let (args, cty) =
        let (_, ty_fun0) = Ctype.instance_class [] cl.cl_type in
        type_args [] [] cl.cl_type ty_fun0 sargs
      in
      rc {cl_desc = Tcl_apply (cl, args);
          cl_loc = scl.pcl_loc;
          cl_type = cty;
          cl_env = val_env;
          cl_attributes = scl.pcl_attributes;
         }
  | Pcl_let (rec_flag, sdefs, scl') ->
      let (defs, val_env) =
        Typecore.type_let In_class_def val_env rec_flag sdefs in
      let (vals, met_env) =
        List.fold_right
          (fun (id, _id_loc, _typ) (vals, met_env) ->
             let path = Pident id in
             (* do not mark the value as used *)
             let vd = Env.find_value path val_env in
             Ctype.begin_def ();
             let expr =
               {exp_desc =
                Texp_ident(path, mknoloc(Longident.Lident (Ident.name id)),vd);
                exp_loc = Location.none; exp_extra = [];
                exp_type = Ctype.instance vd.val_type;
                exp_attributes = [];
                exp_env = val_env;
               }
             in
             Ctype.end_def ();
             Ctype.generalize expr.exp_type;
             let desc =
               {val_type = expr.exp_type; val_kind = Val_ivar (Immutable,
                                                               cl_num);
                val_attributes = [];
                Types.val_loc = vd.Types.val_loc;
                val_uid = vd.val_uid;
               }
             in
             let id' = Ident.create_local (Ident.name id) in
             ((id', expr)
              :: vals,
              Env.add_value id' desc met_env))
          (let_bound_idents_full defs)
          ([], met_env)
      in
      let cl = class_expr cl_num val_env met_env scl' in
      let () = if rec_flag = Recursive then
        check_recursive_bindings val_env defs
      in
      rc {cl_desc = Tcl_let (rec_flag, defs, vals, cl);
          cl_loc = scl.pcl_loc;
          cl_type = cl.cl_type;
          cl_env = val_env;
          cl_attributes = scl.pcl_attributes;
         }
  | Pcl_constraint (scl', scty) ->
      Ctype.begin_class_def ();
      let context = Typetexp.narrow () in
      let cl = class_expr cl_num val_env met_env scl' in
      Typetexp.widen context;
      let context = Typetexp.narrow () in
      let clty = class_type val_env scty in
      Typetexp.widen context;
      Ctype.end_def ();

      limited_generalize (Ctype.row_variable (Ctype.self_type cl.cl_type))
          cl.cl_type;
      limited_generalize (Ctype.row_variable (Ctype.self_type clty.cltyp_type))
        clty.cltyp_type;

      begin match
        Includeclass.class_types val_env cl.cl_type clty.cltyp_type
      with
        []    -> ()
      | error -> raise(Error(cl.cl_loc, val_env, Class_match_failure error))
      end;
      let (vals, meths, concrs) = extract_constraints clty.cltyp_type in
      rc {cl_desc = Tcl_constraint (cl, Some clty, vals, meths, concrs);
          cl_loc = scl.pcl_loc;
          cl_type = snd (Ctype.instance_class [] clty.cltyp_type);
          cl_env = val_env;
          cl_attributes = scl.pcl_attributes;
         }
  | Pcl_open (pod, e) ->
      let used_slot = ref false in
      let (od, new_val_env) = !type_open_descr ~used_slot val_env pod in
      let ( _, new_met_env) = !type_open_descr ~used_slot met_env pod in
      let cl = class_expr cl_num new_val_env new_met_env e in
      rc {cl_desc = Tcl_open (od, cl);
          cl_loc = scl.pcl_loc;
          cl_type = cl.cl_type;
          cl_env = val_env;
          cl_attributes = scl.pcl_attributes;
         }
  | Pcl_extension ext ->
      (* typed_ppxlib *)
      !typed_ppxlib_class_expr_extension_ref ~val_env ~method_env:met_env ext

(*******************************)

(* Approximate the type of the constructor to allow recursive use *)
(* of optional parameters                                         *)

let var_option = Predef.type_option (Btype.newgenvar ())

let rec approx_declaration cl =
  match cl.pcl_desc with
    Pcl_fun (l, _, _, cl) ->
      let arg =
        if Btype.is_optional l then Ctype.instance var_option
        else Ctype.newvar () in
      Ctype.newty (Tarrow (l, arg, approx_declaration cl, Cok))
  | Pcl_let (_, _, cl) ->
      approx_declaration cl
  | Pcl_constraint (cl, _) ->
      approx_declaration cl
  | _ -> Ctype.newvar ()

let rec approx_description ct =
  match ct.pcty_desc with
    Pcty_arrow (l, _, ct) ->
      let arg =
        if Btype.is_optional l then Ctype.instance var_option
        else Ctype.newvar () in
      Ctype.newty (Tarrow (l, arg, approx_description ct, Cok))
  | _ -> Ctype.newvar ()

(*******************************)

let temp_abbrev loc env id arity uid =
  let params = ref [] in
  for _i = 1 to arity do
    params := Ctype.newvar () :: !params
  done;
  let ty = Ctype.newobj (Ctype.newvar ()) in
  let env =
    Env.add_type ~check:true id
      {type_params = !params;
       type_arity = arity;
       type_kind = Type_abstract;
       type_private = Public;
       type_manifest = Some ty;
       type_variance = Variance.unknown_signature ~injective:false ~arity;
       type_separability = Types.Separability.default_signature ~arity;
       type_is_newtype = false;
       type_expansion_scope = Btype.lowest_level;
       type_loc = loc;
       type_attributes = []; (* or keep attrs from the class decl? *)
       type_immediate = Unknown;
       type_unboxed = unboxed_false_default_false;
       type_uid = uid;
      }
      env
  in
  (!params, ty, env)

let initial_env define_class approx
    (res, env) (cl, id, ty_id, obj_id, cl_id, uid) =
  (* Temporary abbreviations *)
  let arity = List.length cl.pci_params in
  let (obj_params, obj_ty, env) = temp_abbrev cl.pci_loc env obj_id arity uid in
  let (cl_params, cl_ty, env) = temp_abbrev cl.pci_loc env cl_id arity uid in

  (* Temporary type for the class constructor *)
  let constr_type = approx cl.pci_expr in
  if !Clflags.principal then Ctype.generalize_spine constr_type;
  let dummy_cty =
    Cty_signature
      { csig_self = Ctype.newvar ();
        csig_vars = Vars.empty;
        csig_concr = Concr.empty;
        csig_inher = [] }
  in
  let dummy_class =
    {Types.cty_params = [];             (* Dummy value *)
     cty_variance = [];
     cty_type = dummy_cty;        (* Dummy value *)
     cty_path = unbound_class;
     cty_new =
       begin match cl.pci_virt with
       | Virtual  -> None
       | Concrete -> Some constr_type
       end;
     cty_loc = Location.none;
     cty_attributes = [];
     cty_uid = uid;
    }
  in
  let env =
    Env.add_cltype ty_id
      {clty_params = [];            (* Dummy value *)
       clty_variance = [];
       clty_type = dummy_cty;       (* Dummy value *)
       clty_path = unbound_class;
       clty_loc = Location.none;
       clty_attributes = [];
       clty_uid = uid;
      }
      (
        if define_class then
          Env.add_class id dummy_class env
        else
          env
      )
  in
  ((cl, id, ty_id,
    obj_id, obj_params, obj_ty,
    cl_id, cl_params, cl_ty,
    constr_type, dummy_class)::res,
   env)

let class_infos define_class kind
    (cl, id, ty_id,
     obj_id, obj_params, obj_ty,
     cl_id, cl_params, cl_ty,
     constr_type, dummy_class)
    (res, env) =

  reset_type_variables ();
  Ctype.begin_class_def ();

  (* Introduce class parameters *)
  let ci_params =
    let make_param (sty, v) =
      try
          (transl_type_param env sty, v)
      with Already_bound ->
        raise(Error(sty.ptyp_loc, env, Repeated_parameter))
    in
      List.map make_param cl.pci_params
  in
  let params = List.map (fun (cty, _) -> cty.ctyp_type) ci_params in

  (* Allow self coercions (only for class declarations) *)
  let coercion_locs = ref [] in

  (* Type the class expression *)
  let (expr, typ) =
    try
      Typecore.self_coercion :=
        (Path.Pident obj_id, coercion_locs) :: !Typecore.self_coercion;
      let res = kind env cl.pci_expr in
      Typecore.self_coercion := List.tl !Typecore.self_coercion;
      res
    with exn ->
      Typecore.self_coercion := []; raise exn
  in

  Ctype.end_def ();

  let sty = Ctype.self_type typ in

  (* First generalize the type of the dummy method (cf PR#6123) *)
  let (fields, _) = Ctype.flatten_fields (Ctype.object_fields sty) in
  List.iter (fun (met, _, ty) -> if met = dummy_method then Ctype.generalize ty)
    fields;
  (* Generalize the row variable *)
  let rv = Ctype.row_variable sty in
  List.iter (Ctype.limited_generalize rv) params;
  limited_generalize rv typ;

  (* Check the abbreviation for the object type *)
  let (obj_params', obj_type) = Ctype.instance_class params typ in
  let constr = Ctype.newconstr (Path.Pident obj_id) obj_params in
  begin
    let ty = Ctype.self_type obj_type in
    Ctype.hide_private_methods ty;
    if not (Ctype.close_object ty) then
      raise(Error(cl.pci_loc, env, Closing_self_type ty));
    begin try
      List.iter2 (Ctype.unify env) obj_params obj_params'
    with Ctype.Unify _ ->
      raise(Error(cl.pci_loc, env,
            Bad_parameters (obj_id, constr,
                            Ctype.newconstr (Path.Pident obj_id)
                                            obj_params')))
    end;
    begin try
      Ctype.unify env ty constr
    with Ctype.Unify _ ->
      raise(Error(cl.pci_loc, env,
        Abbrev_type_clash (constr, ty, Ctype.expand_head env constr)))
    end
  end;

  (* Check the other temporary abbreviation (#-type) *)
  begin
    let (cl_params', cl_type) = Ctype.instance_class params typ in
    let ty = Ctype.self_type cl_type in
    Ctype.hide_private_methods ty;
    Ctype.set_object_name obj_id (Ctype.row_variable ty) cl_params ty;
    begin try
      List.iter2 (Ctype.unify env) cl_params cl_params'
    with Ctype.Unify _ ->
      raise(Error(cl.pci_loc, env,
            Bad_parameters (cl_id,
                            Ctype.newconstr (Path.Pident cl_id)
                                            cl_params,
                            Ctype.newconstr (Path.Pident cl_id)
                                            cl_params')))
    end;
    begin try
      Ctype.unify env ty cl_ty
    with Ctype.Unify _ ->
      let constr = Ctype.newconstr (Path.Pident cl_id) params in
      raise(Error(cl.pci_loc, env, Abbrev_type_clash (constr, ty, cl_ty)))
    end
  end;

  (* Type of the class constructor *)
  begin try
    Ctype.unify env
      (constructor_type constr obj_type)
      (Ctype.instance constr_type)
  with Ctype.Unify trace ->
    raise(Error(cl.pci_loc, env,
                Constructor_type_mismatch (cl.pci_name.txt, trace)))
  end;

  (* Class and class type temporary definitions *)
  let cty_variance =
    Variance.unknown_signature ~injective:false ~arity:(List.length params) in
  let cltydef =
    {clty_params = params; clty_type = class_body typ;
     clty_variance = cty_variance;
     clty_path = Path.Pident obj_id;
     clty_loc = cl.pci_loc;
     clty_attributes = cl.pci_attributes;
     clty_uid = dummy_class.cty_uid;
    }
  and clty =
    {cty_params = params; cty_type = typ;
     cty_variance = cty_variance;
     cty_path = Path.Pident obj_id;
     cty_new =
       begin match cl.pci_virt with
       | Virtual  -> None
       | Concrete -> Some constr_type
       end;
     cty_loc = cl.pci_loc;
     cty_attributes = cl.pci_attributes;
     cty_uid = dummy_class.cty_uid;
    }
  in
  dummy_class.cty_type <- typ;
  let env =
    Env.add_cltype ty_id cltydef (
    if define_class then Env.add_class id clty env else env)
  in

  if cl.pci_virt = Concrete then begin
    let sign = Ctype.signature_of_class_type typ in
    let mets = virtual_methods sign in
    let vals =
      Vars.fold
        (fun name (_mut, vr, _ty) l -> if vr = Virtual then name :: l else l)
        sign.csig_vars [] in
    if mets <> []  || vals <> [] then
      raise(Error(cl.pci_loc, env, Virtual_class(define_class, false, mets,
                                                 vals)));
  end;

  (* Misc. *)
  let arity = Ctype.class_type_arity typ in
  let pub_meths =
    let (fields, _) =
      Ctype.flatten_fields (Ctype.object_fields (Ctype.expand_head env obj_ty))
    in
    List.map (function (lab, _, _) -> lab) fields
  in

  (* Final definitions *)
  let (params', typ') = Ctype.instance_class params typ in
  let cltydef =
    {clty_params = params'; clty_type = class_body typ';
     clty_variance = cty_variance;
     clty_path = Path.Pident obj_id;
     clty_loc = cl.pci_loc;
     clty_attributes = cl.pci_attributes;
     clty_uid = dummy_class.cty_uid;
    }
  and clty =
    {cty_params = params'; cty_type = typ';
     cty_variance = cty_variance;
     cty_path = Path.Pident obj_id;
     cty_new =
       begin match cl.pci_virt with
       | Virtual  -> None
       | Concrete -> Some (Ctype.instance constr_type)
       end;
     cty_loc = cl.pci_loc;
     cty_attributes = cl.pci_attributes;
     cty_uid = dummy_class.cty_uid;
    }
  in
  let obj_abbr =
    let arity = List.length obj_params in
    {
     type_params = obj_params;
     type_arity = arity;
     type_kind = Type_abstract;
     type_private = Public;
     type_manifest = Some obj_ty;
     type_variance = Variance.unknown_signature ~injective:false ~arity;
     type_separability = Types.Separability.default_signature ~arity;
     type_is_newtype = false;
     type_expansion_scope = Btype.lowest_level;
     type_loc = cl.pci_loc;
     type_attributes = []; (* or keep attrs from cl? *)
     type_immediate = Unknown;
     type_unboxed = unboxed_false_default_false;
     type_uid = dummy_class.cty_uid;
    }
  in
  let (cl_params, cl_ty) =
    Ctype.instance_parameterized_type params (Ctype.self_type typ)
  in
  Ctype.hide_private_methods cl_ty;
  Ctype.set_object_name obj_id (Ctype.row_variable cl_ty) cl_params cl_ty;
  let cl_abbr =
    let arity = List.length cl_params in
    {
     type_params = cl_params;
     type_arity = arity;
     type_kind = Type_abstract;
     type_private = Public;
     type_manifest = Some cl_ty;
     type_variance = Variance.unknown_signature ~injective:false ~arity;
     type_separability = Types.Separability.default_signature ~arity;
     type_is_newtype = false;
     type_expansion_scope = Btype.lowest_level;
     type_loc = cl.pci_loc;
     type_attributes = []; (* or keep attrs from cl? *)
     type_immediate = Unknown;
     type_unboxed = unboxed_false_default_false;
     type_uid = dummy_class.cty_uid;
    }
  in
  ((cl, id, clty, ty_id, cltydef, obj_id, obj_abbr, cl_id, cl_abbr, ci_params,
    arity, pub_meths, List.rev !coercion_locs, expr) :: res,
   env)

let final_decl env define_class
    (cl, id, clty, ty_id, cltydef, obj_id, obj_abbr, cl_id, cl_abbr, ci_params,
     arity, pub_meths, coe, expr) =

  begin try Ctype.collapse_conj_params env clty.cty_params
  with Ctype.Unify trace ->
    raise(Error(cl.pci_loc, env, Non_collapsable_conjunction (id, clty, trace)))
  end;

  (* make the dummy method disappear *)
  begin
    let self_type = Ctype.self_type clty.cty_type in
    let methods, _ =
      Ctype.flatten_fields
        (Ctype.object_fields (Ctype.expand_head env self_type))
    in
    List.iter (fun (lab,kind,_) ->
      if lab = dummy_method then
        match Btype.field_kind_repr kind with
          Fvar r -> Btype.set_kind r Fabsent
        | _ -> ()
    ) methods
  end;

  List.iter Ctype.generalize clty.cty_params;
  generalize_class_type true clty.cty_type;
  Option.iter  Ctype.generalize clty.cty_new;
  List.iter Ctype.generalize obj_abbr.type_params;
  Option.iter  Ctype.generalize obj_abbr.type_manifest;
  List.iter Ctype.generalize cl_abbr.type_params;
  Option.iter  Ctype.generalize cl_abbr.type_manifest;

  if not (closed_class clty) then
    raise(Error(cl.pci_loc, env, Non_generalizable_class (id, clty)));

  begin match
    Ctype.closed_class clty.cty_params
      (Ctype.signature_of_class_type clty.cty_type)
  with
    None        -> ()
  | Some reason ->
      let printer =
        if define_class
        then function ppf -> Printtyp.class_declaration id ppf clty
        else function ppf -> Printtyp.cltype_declaration id ppf cltydef
      in
      raise(Error(cl.pci_loc, env, Unbound_type_var(printer, reason)))
  end;
  { id; clty; ty_id; cltydef; obj_id; obj_abbr; cl_id; cl_abbr; arity;
    pub_meths; coe; expr;
    id_loc = cl.pci_name;
    req = { ci_loc = cl.pci_loc;
            ci_virt = cl.pci_virt;
            ci_params = ci_params;
        (* TODO : check that we have the correct use of identifiers *)
            ci_id_name = cl.pci_name;
            ci_id_class = id;
            ci_id_class_type = ty_id;
            ci_id_object = obj_id;
            ci_id_typehash = cl_id;
            ci_expr = expr;
            ci_decl = clty;
            ci_type_decl = cltydef;
            ci_attributes = cl.pci_attributes;
        }
  }
(*   (cl.pci_variance, cl.pci_loc)) *)

let class_infos define_class kind
    (cl, id, ty_id,
     obj_id, obj_params, obj_ty,
     cl_id, cl_params, cl_ty,
     constr_type, dummy_class)
    (res, env) =
  Builtin_attributes.warning_scope cl.pci_attributes
    (fun () ->
       class_infos define_class kind
         (cl, id, ty_id,
          obj_id, obj_params, obj_ty,
          cl_id, cl_params, cl_ty,
          constr_type, dummy_class)
         (res, env)
    )

let extract_type_decls { clty; cltydef; obj_id; obj_abbr; cl_abbr; req} decls =
  (obj_id, obj_abbr, cl_abbr, clty, cltydef, req) :: decls

let merge_type_decls decl (obj_abbr, cl_abbr, clty, cltydef) =
  {decl with obj_abbr; cl_abbr; clty; cltydef}

let final_env define_class env { id; clty; ty_id; cltydef; obj_id; obj_abbr;
    cl_id; cl_abbr } =
  (* Add definitions after cleaning them *)
  Env.add_type ~check:true obj_id
    (Subst.type_declaration Subst.identity obj_abbr) (
  Env.add_type ~check:true cl_id
    (Subst.type_declaration Subst.identity cl_abbr) (
  Env.add_cltype ty_id (Subst.cltype_declaration Subst.identity cltydef) (
  if define_class then
    Env.add_class id (Subst.class_declaration Subst.identity clty) env
  else env)))

(* Check that #c is coercible to c if there is a self-coercion *)
let check_coercions env { id; id_loc; clty; ty_id; cltydef; obj_id; obj_abbr;
    cl_id; cl_abbr; arity; pub_meths; coe; req } =
  begin match coe with [] -> ()
  | loc :: _ ->
      let cl_ty, obj_ty =
        match cl_abbr.type_manifest, obj_abbr.type_manifest with
          Some cl_ab, Some obj_ab ->
            let cl_params, cl_ty =
              Ctype.instance_parameterized_type cl_abbr.type_params cl_ab
            and obj_params, obj_ty =
              Ctype.instance_parameterized_type obj_abbr.type_params obj_ab
            in
            List.iter2 (Ctype.unify env) cl_params obj_params;
            cl_ty, obj_ty
        | _ -> assert false
      in
      begin try Ctype.subtype env cl_ty obj_ty ()
      with Ctype.Subtype (tr1, tr2) ->
        raise(Typecore.Error(loc, env, Typecore.Not_subtype(tr1, tr2)))
      end;
      if not (Ctype.opened_object cl_ty) then
        raise(Error(loc, env, Cannot_coerce_self obj_ty))
  end;
  {cls_id = id;
   cls_id_loc = id_loc;
   cls_decl = clty;
   cls_ty_id = ty_id;
   cls_ty_decl = cltydef;
   cls_obj_id = obj_id;
   cls_obj_abbr = obj_abbr;
   cls_typesharp_id = cl_id;
   cls_abbr = cl_abbr;
   cls_arity = arity;
   cls_pub_methods = pub_meths;
   cls_info=req}

(*******************************)

let type_classes define_class approx kind env cls =
  let scope = Ctype.create_scope () in
  let cls =
    List.map
      (function cl ->
         (cl,
          Ident.create_scoped ~scope cl.pci_name.txt,
          Ident.create_scoped ~scope cl.pci_name.txt,
          Ident.create_scoped ~scope cl.pci_name.txt,
          Ident.create_scoped ~scope ("#" ^ cl.pci_name.txt),
          Uid.mk ~current_unit:(Env.get_unit_name ())
         ))
      cls
  in
  Ctype.begin_class_def ();
  let (res, env) =
    List.fold_left (initial_env define_class approx) ([], env) cls
  in
  let (res, env) =
    List.fold_right (class_infos define_class kind) res ([], env)
  in
  Ctype.end_def ();
  let res = List.rev_map (final_decl env define_class) res in
  let decls = List.fold_right extract_type_decls res [] in
  let decls =
    try Typedecl_variance.update_class_decls env decls
    with Typedecl_variance.Error(loc, err) ->
      raise (Typedecl.Error(loc, Typedecl.Variance err))
  in
  let res = List.map2 merge_type_decls res decls in
  let env = List.fold_left (final_env define_class) env res in
  let res = List.map (check_coercions env) res in
  (res, env)

let class_num = ref 0
let class_declaration env sexpr =
  incr class_num;
  let expr = class_expr (Int.to_string !class_num) env env sexpr in
  (expr, expr.cl_type)

let class_description env sexpr =
  let expr = class_type env sexpr in
  (expr, expr.cltyp_type)

let class_declarations env cls =
  let info, env =
    type_classes true approx_declaration class_declaration env cls
  in
  let ids, exprs =
    List.split
      (List.map
         (fun ci -> ci.cls_id, ci.cls_info.ci_expr)
         info)
  in
  check_recursive_class_bindings env ids exprs;
  info, env

let class_descriptions env cls =
  type_classes true approx_description class_description env cls

let class_type_declarations env cls =
  let (decls, env) =
    type_classes false approx_description class_description env cls
  in
  (List.map
     (fun decl ->
        {clsty_ty_id = decl.cls_ty_id;
         clsty_id_loc = decl.cls_id_loc;
         clsty_ty_decl = decl.cls_ty_decl;
         clsty_obj_id = decl.cls_obj_id;
         clsty_obj_abbr = decl.cls_obj_abbr;
         clsty_typesharp_id = decl.cls_typesharp_id;
         clsty_abbr = decl.cls_abbr;
         clsty_info = decl.cls_info})
     decls,
   env)

let rec unify_parents env ty cl =
  match cl.cl_desc with
    Tcl_ident (p, _, _) ->
      begin try
        let decl = Env.find_class p env in
        let _, body = Ctype.find_cltype_for_path env decl.cty_path in
        Ctype.unify env ty (Ctype.instance body)
      with
        Not_found -> ()
      | _exn -> assert false
      end
  | Tcl_structure st -> unify_parents_struct env ty st
  | Tcl_open (_, cl)
  | Tcl_fun (_, _, _, cl, _)
  | Tcl_apply (cl, _)
  | Tcl_let (_, _, _, cl)
  | Tcl_constraint (cl, _, _, _, _) -> unify_parents env ty cl
  (* [Typed_ppxlib] 
     Unifying is equivalent to adding more constraints,
     we wish to have the most general types (since we cannot infer any 
     information from extensions) => no unifying!
  *)
  | Tcl_extension _ext ->
      ()
and unify_parents_struct env ty st =
  List.iter
    (function
      | {cf_desc = Tcf_inherit (_, cl, _, _, _)} ->
          unify_parents env ty cl
      | _ -> ())
    st.cstr_fields

let type_object env loc s =
  incr class_num;
  let (desc, sign) =
    class_structure (Int.to_string !class_num) true env env loc s in
  let sty = Ctype.expand_head env sign.csig_self in
  Ctype.hide_private_methods sty;
  let (fields, _) = Ctype.flatten_fields (Ctype.object_fields sty) in
  let meths = List.map (fun (s,_,_) -> s) fields in
  unify_parents_struct env sign.csig_self desc;
  (desc, sign, meths)

let () =
  Typecore.type_object := type_object

(*******************************)

(* Approximate the class declaration as class ['params] id = object end *)
let approx_class sdecl =
  let open Ast_helper in
  let self' = Typ.any () in
  let clty' = Cty.signature ~loc:sdecl.pci_expr.pcty_loc (Csig.mk self' []) in
  { sdecl with pci_expr = clty' }

let approx_class_declarations env sdecls =
  fst (class_type_declarations env (List.map approx_class sdecls))

(*******************************)

(* Error report *)

open Format

let report_error env ppf = function
  | Repeated_parameter ->
      fprintf ppf "A type parameter occurs several times"
  | Unconsistent_constraint trace ->
      fprintf ppf "The class constraints are not consistent.@.";
      Printtyp.report_unification_error ppf env trace
        (fun ppf -> fprintf ppf "Type")
        (fun ppf -> fprintf ppf "is not compatible with type")
  | Field_type_mismatch (k, m, trace) ->
      Printtyp.report_unification_error ppf env trace
        (function ppf ->
           fprintf ppf "The %s %s@ has type" k m)
        (function ppf ->
           fprintf ppf "but is expected to have type")
  | Structure_expected clty ->
      fprintf ppf
        "@[This class expression is not a class structure; it has type@ %a@]"
        Printtyp.class_type clty
  | Cannot_apply _ ->
      fprintf ppf
        "This class expression is not a class function, it cannot be applied"
  | Apply_wrong_label l ->
      let mark_label = function
        | Nolabel -> "out label"
        |  l -> sprintf " label %s" (Btype.prefixed_label_name l) in
      fprintf ppf "This argument cannot be applied with%s" (mark_label l)
  | Pattern_type_clash ty ->
      (* XXX Trace *)
      (* XXX Revoir message d'erreur | Improve error message *)
      fprintf ppf "@[%s@ %a@]"
        "This pattern cannot match self: it only matches values of type"
        Printtyp.type_expr ty
  | Unbound_class_2 cl ->
      fprintf ppf "@[The class@ %a@ is not yet completely defined@]"
      Printtyp.longident cl
  | Unbound_class_type_2 cl ->
      fprintf ppf "@[The class type@ %a@ is not yet completely defined@]"
      Printtyp.longident cl
  | Abbrev_type_clash (abbrev, actual, expected) ->
      (* XXX Afficher une trace ? | Print a trace? *)
      Printtyp.reset_and_mark_loops_list [abbrev; actual; expected];
      fprintf ppf "@[The abbreviation@ %a@ expands to type@ %a@ \
       but is used with type@ %a@]"
        !Oprint.out_type (Printtyp.tree_of_typexp false abbrev)
        !Oprint.out_type (Printtyp.tree_of_typexp false actual)
        !Oprint.out_type (Printtyp.tree_of_typexp false expected)
  | Constructor_type_mismatch (c, trace) ->
      Printtyp.report_unification_error ppf env trace
        (function ppf ->
           fprintf ppf "The expression \"new %s\" has type" c)
        (function ppf ->
           fprintf ppf "but is used with type")
  | Virtual_class (cl, imm, mets, vals) ->
      let print_mets ppf mets =
        List.iter (function met -> fprintf ppf "@ %s" met) mets in
      let missings =
        match mets, vals with
          [], _ -> "variables"
        | _, [] -> "methods"
        | _ -> "methods and variables"
      in
      let print_msg ppf =
        if imm then fprintf ppf "This object has virtual %s" missings
        else if cl then fprintf ppf "This class should be virtual"
        else fprintf ppf "This class type should be virtual"
      in
      fprintf ppf
        "@[%t.@ @[<2>The following %s are undefined :%a@]@]"
        print_msg missings print_mets (mets @ vals)
  | Parameter_arity_mismatch(lid, expected, provided) ->
      fprintf ppf
        "@[The class constructor %a@ expects %i type argument(s),@ \
           but is here applied to %i type argument(s)@]"
        Printtyp.longident lid expected provided
  | Parameter_mismatch trace ->
      Printtyp.report_unification_error ppf env trace
        (function ppf ->
           fprintf ppf "The type parameter")
        (function ppf ->
           fprintf ppf "does not meet its constraint: it should be")
  | Bad_parameters (id, params, cstrs) ->
      Printtyp.reset_and_mark_loops_list [params; cstrs];
      fprintf ppf
        "@[The abbreviation %a@ is used with parameters@ %a@ \
           which are incompatible with constraints@ %a@]"
        Printtyp.ident id
        !Oprint.out_type (Printtyp.tree_of_typexp false params)
        !Oprint.out_type (Printtyp.tree_of_typexp false cstrs)
  | Class_match_failure error ->
      Includeclass.report_error ppf error
  | Unbound_val lab ->
      fprintf ppf "Unbound instance variable %s" lab
  | Unbound_type_var (printer, reason) ->
      let print_common ppf kind ty0 real lab ty =
        let ty1 =
          if real then ty0 else Btype.newgenty(Tobject(ty0, ref None)) in
        List.iter Printtyp.mark_loops [ty; ty1];
        fprintf ppf
          "The %s %s@ has type@;<1 2>%a@ where@ %a@ is unbound"
          kind lab
          !Oprint.out_type (Printtyp.tree_of_typexp false ty)
          !Oprint.out_type (Printtyp.tree_of_typexp false ty0)
      in
      let print_reason ppf = function
      | Ctype.CC_Method (ty0, real, lab, ty) ->
          print_common ppf "method" ty0 real lab ty
      | Ctype.CC_Value (ty0, real, lab, ty) ->
          print_common ppf "instance variable" ty0 real lab ty
      in
      Printtyp.reset ();
      fprintf ppf
        "@[<v>@[Some type variables are unbound in this type:@;<1 2>%t@]@ \
              @[%a@]@]"
       printer print_reason reason
  | Non_generalizable_class (id, clty) ->
      fprintf ppf
        "@[The type of this class,@ %a,@ \
           contains type variables that cannot be generalized@]"
        (Printtyp.class_declaration id) clty
  | Cannot_coerce_self ty ->
      fprintf ppf
        "@[The type of self cannot be coerced to@ \
           the type of the current class:@ %a.@.\
           Some occurrences are contravariant@]"
        Printtyp.type_scheme ty
  | Non_collapsable_conjunction (id, clty, trace) ->
      fprintf ppf
        "@[The type of this class,@ %a,@ \
           contains non-collapsible conjunctive types in constraints.@ %t@]"
        (Printtyp.class_declaration id) clty
        (fun ppf -> Printtyp.report_unification_error ppf env trace
            (fun ppf -> fprintf ppf "Type")
            (fun ppf -> fprintf ppf "is not compatible with type")
        )
  | Final_self_clash trace ->
      Printtyp.report_unification_error ppf env trace
        (function ppf ->
           fprintf ppf "This object is expected to have type")
        (function ppf ->
           fprintf ppf "but actually has type")
  | Mutability_mismatch (_lab, mut) ->
      let mut1, mut2 =
        if mut = Immutable then "mutable", "immutable"
        else "immutable", "mutable" in
      fprintf ppf
        "@[The instance variable is %s;@ it cannot be redefined as %s@]"
        mut1 mut2
  | No_overriding (_, "") ->
      fprintf ppf "@[This inheritance does not override any method@ %s@]"
        "instance variable"
  | No_overriding (kind, name) ->
      fprintf ppf "@[The %s `%s'@ has no previous definition@]" kind name
  | Duplicate (kind, name) ->
      fprintf ppf "@[The %s `%s'@ has multiple definitions in this object@]"
                    kind name
  | Closing_self_type self ->
    fprintf ppf
      "@[Cannot close type of object literal:@ %a@,\
       it has been unified with the self type of a class that is not yet@ \
       completely defined.@]"
      Printtyp.type_scheme self

let report_error env ppf err =
  Printtyp.wrap_printing_env ~error:true
    env (fun () -> report_error env ppf err)

let () =
  Location.register_error_of_exn
    (function
      | Error (loc, env, err) ->
        Some (Location.error_of_printer ~loc (report_error env) err)
      | Error_forward err ->
        Some err
      | _ ->
        None
    )
