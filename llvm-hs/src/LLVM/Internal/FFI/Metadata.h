#ifndef __LLVM_INTERNAL_FFI__METADATA__H__
#define __LLVM_INTERNAL_FFI__METADATA__H__

enum {
#define HANDLE_METADATA_LEAF(CLASS) CLASS##Kind,
#include "llvm/IR/Metadata.def"
#undef HANDLE_METADATA_LEAF
} MetadataSubclassId;

#define LLVM_HS_FOR_EACH_MDNODE_SUBCLASS(macro) \
  macro(MDString) \
  macro(ConstantAsMetadata) \
  macro(LocalAsMetadata) \
  macro(DistinctMDOperandPlaceholder) \
  macro(MDTuple) \
  macro(DILocation) \
  macro(DIExpression) \
  macro(DIGlobalVariableExpression) \
  macro(GenericDINode) \
  macro(DISubrange) \
  macro(DIEnumerator) \
  macro(DIBasicType) \
  macro(DIDerivedType) \
  macro(DICompositeType) \
  macro(DISubroutineType) \
  macro(DIFile) \
  macro(DICompileUnit) \
  macro(DISubprogram) \
  macro(DILexicalBlock) \
  macro(DILexicalBlockFile) \
  macro(DINamespace) \
  macro(DIModule) \
  macro(DITemplateTypeParameter) \
  macro(DITemplateValueParameter) \
  macro(DIGlobalVariable) \
  macro(DILocalVariable) \
  macro(DIObjCProperty) \
  macro(DIImportedEntity) \
  macro(DIMacro) \
  macro(DIMacroFile)

enum LLVM_Hs_DwOp {
#define HANDLE_DW_OP(ID, NAME, VERSION, VENDOR) LLVM_Hs_DwOp_##NAME = ID,
#include "llvm/BinaryFormat/Dwarf.def"
#undef HANDLE_DW_OP
    LLVM_Hs_DwOp_LLVM_fragment = 0x1000
};

#define LLVM_HS_FOR_EACH_DW_OP(macro) \
    macro(and)                        \
    macro(bregx)                      \
    macro(constu)                     \
    macro(deref)                      \
    macro(div)                        \
    macro(dup)                        \
    macro(lit0)                       \
    macro(LLVM_fragment)              \
    macro(minus)                      \
    macro(mod)                        \
    macro(mul)                        \
    macro(not)                        \
    macro(or)                         \
    macro(plus)                       \
    macro(plus_uconst)                \
    macro(push_object_address)        \
    macro(shl)                        \
    macro(shr)                        \
    macro(shra)                       \
    macro(stack_value)                \
    macro(swap)                       \
    macro(xderef)                     \
    macro(xor)

enum LLVM_Hs_DwAtE {
#define HANDLE_DW_ATE(ID, NAME, VERSION, VENDOR) LLVM_Hs_DwAtE_##NAME = ID,
#include "llvm/BinaryFormat/Dwarf.def"
#undef HANDLE_DW_ATE
};

#define LLVM_HS_FOR_EACH_DW_ATE(macro) \
    macro(address)                     \
    macro(boolean)                     \
    macro(complex_float)               \
    macro(float)                       \
    macro(signed)                      \
    macro(signed_char)                 \
    macro(unsigned)                    \
    macro(unsigned_char)               \
    macro(imaginary_float)             \
    macro(packed_decimal)              \
    macro(numeric_string)              \
    macro(edited)                      \
    macro(signed_fixed)                \
    macro(unsigned_fixed)              \
    macro(decimal_float)               \
    macro(UTF)                         \
    macro(UCS)                         \
    macro(ASCII)

enum LLVM_Hs_DwTag {
#define HANDLE_DW_TAG(ID, NAME, VERSION, VENDOR) LLVM_Hs_DwTag_##NAME = ID,
#include "llvm/BinaryFormat/Dwarf.def"
#undef HANDLE_DW_TAG
};

#define LLVM_HS_FOR_EACH_DW_TAG(macro) \
    macro(imported_module) \
    macro(imported_declaration) \
    macro(typedef) \
    macro(pointer_type) \
    macro(ptr_to_member_type) \
    macro(reference_type) \
    macro(rvalue_reference_type) \
    macro(const_type) \
    macro(volatile_type) \
    macro(restrict_type) \
    macro(atomic_type) \
    macro(member) \
    macro(inheritance) \
    macro(friend) \
    macro(base_type) \
    macro(unspecified_type) \
    macro(template_value_parameter) \
    macro(GNU_template_template_param) \
    macro(GNU_template_parameter_pack) \
    macro(array_type) \
    macro(enumeration_type) \
    macro(structure_type) \
    macro(class_type) \
    macro(union_type)

enum LLVM_Hs_DwVirtuality {
#define HANDLE_DW_VIRTUALITY(ID, NAME) LLVM_Hs_DwVirtuality_##NAME = ID,
#include "llvm/BinaryFormat/Dwarf.def"
#undef HANDLE_DW_VIRTUALITY
};

#define LLVM_HS_FOR_EACH_DW_VIRTUALITY(macro) \
    macro(none) \
    macro(virtual) \
    macro(pure_virtual)
#endif
