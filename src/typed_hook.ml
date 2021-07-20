open Typed_ppxlib_ocaml_typing

module Extension = struct
  include Typed_hook_intf.Extension

  class hook =
    object
      method expression : expression = !Typecore.typed_ppxlib_expression_extension_ref
      method pattern : pattern = !Typecore.typed_ppxlib_pattern_extension_ref
      method structure_item : structure_item = !Typemod.typed_ppxlib_structure_item_extension_ref
      method signature_item : signature_item = !Typemod.typed_ppxlib_signature_item_extension_ref
      method class_expr : class_expr = !Typeclass.typed_ppxlib_class_expr_extension_ref
      method class_field : class_field = !Typeclass.typed_ppxlib_class_field_extension_ref

      method class_type_field : class_type_field =
        !Typeclass.typed_ppxlib_class_type_field_extension_ref

      method class_type : class_type = !Typeclass.typed_ppxlib_class_type_extension_ref
    end
end