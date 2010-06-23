var HASH_MOD = 147;
var HASH_MULT = 17;

var keyword_hashtable =
[
 { id: "const", cat: CONST_CAT }
,{ id: "continue", cat: CONTINUE_CAT }
,null
,null
,null
,null
,null
,null
,null
,{ id: "try", cat: TRY_CAT }
,null
,null
,null
,null
,{ id: "finally", cat: FINALLY_CAT }
,null
,null
,null
,null
,{ id: "enum", cat: ENUM_CAT }
,null
,{ id: "for", cat: FOR_CAT }
,null
,null
,{ id: "debugger", cat: DEBUGGER_CAT }
,{ id: "class", cat: CLASS_CAT }
,null
,{ id: "public", cat: PUBLIC_CAT }
,null
,null
,null
,null
,{ id: "switch", cat: SWITCH_CAT }
,null
,null
,null
,null
,null
,{ id: "break", cat: BREAK_CAT }
,{ id: "true", cat: TRUE_CAT }
,null
,null
,{ id: "typeof", cat: TYPEOF_CAT }
,null
,null
,null
,{ id: "this", cat: THIS_CAT }
,{ id: "do", cat: DO_CAT }
,null
,null
,null
,null
,null
,{ id: "throw", cat: THROW_CAT }
,null
,null
,null
,null
,null
,null
,null
,null
,null
,null
,{ id: "implements", cat: IMPLEMENTS_CAT }
,{ id: "case", cat: CASE_CAT }
,null
,null
,null
,{ id: "package", cat: PACKAGE_CAT }
,null
,null
,null
,null
,null
,{ id: "delete", cat: DELETE_CAT }
,null
,null
,{ id: "default", cat: DEFAULT_CAT }
,null
,{ id: "import", cat: IMPORT_CAT }
,{ id: "super", cat: SUPER_CAT }
,null
,{ id: "protected", cat: PROTECTED_CAT }
,{ id: "false", cat: FALSE_CAT }
,null
,null
,null
,{ id: "yield", cat: YIELD_CAT }
,null
,null
,null
,null
,null
,{ id: "null", cat: NULL_CAT }
,{ id: "return", cat: RETURN_CAT }
,null
,null
,null
,null
,null
,null
,null
,null
,{ id: "while", cat: WHILE_CAT }
,null
,null
,null
,null
,{ id: "with", cat: WITH_CAT }
,{ id: "new", cat: NEW_CAT }
,null
,null
,null
,null
,{ id: "private", cat: PRIVATE_CAT }
,null
,{ id: "let", cat: LET_CAT }
,null
,null
,{ id: "void", cat: VOID_CAT }
,{ id: "function", cat: FUNCTION_CAT }
,null
,{ id: "if", cat: IF_CAT }
,null
,{ id: "export", cat: EXPORT_CAT }
,null
,null
,null
,null
,null
,{ id: "in", cat: IN_CAT }
,null
,{ id: "interface", cat: INTERFACE_CAT }
,{ id: "else", cat: ELSE_CAT }
,{ id: "instanceof", cat: INSTANCEOF_CAT }
,null
,null
,null
,null
,null
,{ id: "catch", cat: CATCH_CAT }
,null
,null
,{ id: "var", cat: VAR_CAT }
,{ id: "extends", cat: EXTENDS_CAT }
,{ id: "static", cat: STATIC_CAT }
];
