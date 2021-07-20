open Typed_ppxlib_ocaml_typing

module Extension = struct
  type expression =
    ?in_function:Location.t * Types.type_expr
    -> recarg:Typecore.recarg
    -> env:Env.t
    -> expected:Typecore.type_expected
    -> Parsetree.extension
    -> Typedtree.expression

  (* TODO: Use ppx_import. Must wait for 4.12 update. *)
  type pattern = Typecore.typed_ppxlib_pattern_extension =
    { f :
        'k 'r.
        'k Typedtree.pattern_category
        -> no_existentials:Typecore.existential_restriction option
        -> env:Env.t ref
        -> expected:Types.type_expr
        -> cnt:('k Typedtree.general_pattern -> 'r)
        -> Parsetree.extension
        -> 'r
    }

  type structure_item =
    env:Env.t -> Parsetree.extension -> Typedtree.structure_item_desc * Types.signature * Env.t

  type signature_item = env:Env.t -> Parsetree.extension -> Typedtree.signature
  type module_expr = env:Env.t -> Parsetree.extension -> Typedtree.module_expr
  type module_type = env:Env.t -> Parsetree.extension -> Typedtree.module_type
  type class_expr = val_env:Env.t -> method_env:Env.t -> Parsetree.extension -> Typedtree.class_expr

  type class_field =
    self:Types.type_expr
    -> methods:(Ident.t * Types.type_expr) Types.Meths.t ref
    -> vars:
         (Ident.t * Asttypes.mutable_flag * Asttypes.virtual_flag * Types.type_expr) Types.Vars.t
         ref
    -> acc:class_field_acc
    -> Parsetree.extension
    -> class_field_acc

  and class_field_acc = Typeclass.class_field_acc =
    { class_env : Typeclass.class_env
    ; fields : Typedtree.class_field Lazy.t list
    ; concr_methods : Types.Concr.t
    ; warn_vals : Types.Concr.t
    ; inher : (Path.t * Types.type_expr list) list
    ; local_methods : Types.Concr.t
    ; local_vals : Types.Concr.t
    }

  type class_type = env:Env.t -> Parsetree.extension -> Typedtree.class_type

  type class_type_field =
    env:Env.t
    -> self:Types.type_expr
    -> methods:(Ident.t * Types.type_expr) Types.Meths.t ref
    -> acc:class_type_field_acc
    -> Parsetree.extension
    -> class_type_field_acc

  and class_type_field_acc = Typeclass.class_type_field_acc =
    { fields : Typedtree.class_type_field list
    ; val_sig : (Asttypes.mutable_flag * Asttypes.virtual_flag * Types.type_expr) Types.Vars.t
    ; concr_methods : Types.Concr.t
    ; inher : (Path.t * Types.type_expr list) list
    }
end

module type Intf = sig
  module Extension : sig
    include module type of Extension

    class hook :
      object
        method expression : expression
        method pattern : pattern
        method structure_item : structure_item
        method signature_item : signature_item
        method class_expr : class_expr
        method class_field : class_field
        method class_type_field : class_type_field
        method class_type : class_type
      end
  end
end