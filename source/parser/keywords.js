var HASH_MOD = 148;
var HASH_MULT = 121;

var keyword_hashtable =
[
 null
,null
,null
,null
,null
,null
,null
,{ id: "future", cat: FUTURE_CAT }
,null
,null
,{ id: "void", cat: VOID_CAT }
,{ id: "null", cat: NULL_CAT }
,null
,null
,{ id: "export", cat: EXPORT_CAT }
,{ id: "yield", cat: YIELD_CAT }
,null
,null
,null
,null
,{ id: "return", cat: RETURN_CAT }
,null
,null
,null
,{ id: "case", cat: CASE_CAT }
,{ id: "while", cat: WHILE_CAT }
,null
,null
,null
,{ id: "debugger", cat: DEBUGGER_CAT }
,{ id: "new", cat: NEW_CAT }
,null
,null
,{ id: "continue", cat: CONTINUE_CAT }
,null
,{ id: "private", cat: PRIVATE_CAT }
,null
,null
,{ id: "class", cat: CLASS_CAT }
,null
,null
,null
,null
,null
,null
,{ id: "var", cat: VAR_CAT }
,null
,{ id: "const", cat: CONST_CAT }
,null
,{ id: "let", cat: LET_CAT }
,null
,null
,null
,{ id: "else", cat: ELSE_CAT }
,null
,null
,null
,null
,null
,{ id: "try", cat: TRY_CAT }
,null
,{ id: "break", cat: BREAK_CAT }
,{ id: "function", cat: FUNCTION_CAT }
,null
,null
,null
,null
,null
,null
,null
,{ id: "switch", cat: SWITCH_CAT }
,{ id: "public", cat: PUBLIC_CAT }
,null
,null
,null
,{ id: "do", cat: DO_CAT }
,null
,null
,null
,{ id: "if", cat: IF_CAT }
,{ id: "with", cat: WITH_CAT }
,null
,null
,{ id: "finally", cat: FINALLY_CAT }
,null
,null
,null
,{ id: "in", cat: IN_CAT }
,null
,{ id: "default", cat: DEFAULT_CAT }
,null
,{ id: "catch", cat: CATCH_CAT }
,{ id: "throw", cat: THROW_CAT }
,null
,{ id: "implements", cat: IMPLEMENTS_CAT }
,{ id: "extends", cat: EXTENDS_CAT }
,{ id: "true", cat: TRUE_CAT }
,null
,{ id: "instanceof", cat: INSTANCEOF_CAT }
,null
,{ id: "this", cat: THIS_CAT }
,null
,null
,null
,null
,{ id: "interface", cat: INTERFACE_CAT }
,null
,{ id: "false", cat: FALSE_CAT }
,null
,null
,null
,null
,null
,null
,null
,null
,null
,{ id: "atomic", cat: ATOMIC_CAT }
,null
,{ id: "import", cat: IMPORT_CAT }
,null
,null
,null
,{ id: "super", cat: SUPER_CAT }
,{ id: "static", cat: STATIC_CAT }
,null
,null
,null
,null
,null
,{ id: "protected", cat: PROTECTED_CAT }
,{ id: "delete", cat: DELETE_CAT }
,{ id: "package", cat: PACKAGE_CAT }
,{ id: "enum", cat: ENUM_CAT }
,null
,null
,null
,null
,null
,{ id: "for", cat: FOR_CAT }
,null
,null
,null
,null
,null
,null
,null
,{ id: "typeof", cat: TYPEOF_CAT }
];
