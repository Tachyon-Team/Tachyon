var HASH_MOD = 147;
var HASH_MULT = 17;

var keyword_hashtable =
[
 { id: "const", cat: CONST_CAT }
,{ id: "continue", cat: CONTINUE_CAT }
,false
,false
,false
,false
,false
,false
,false
,{ id: "try", cat: TRY_CAT }
,false
,false
,false
,false
,{ id: "finally", cat: FINALLY_CAT }
,false
,false
,false
,false
,{ id: "enum", cat: ENUM_CAT }
,false
,{ id: "for", cat: FOR_CAT }
,false
,false
,{ id: "debugger", cat: DEBUGGER_CAT }
,{ id: "class", cat: CLASS_CAT }
,false
,{ id: "public", cat: PUBLIC_CAT }
,false
,false
,false
,false
,{ id: "switch", cat: SWITCH_CAT }
,false
,false
,false
,false
,false
,{ id: "break", cat: BREAK_CAT }
,{ id: "true", cat: TRUE_CAT }
,false
,false
,{ id: "typeof", cat: TYPEOF_CAT }
,false
,false
,false
,{ id: "this", cat: THIS_CAT }
,{ id: "do", cat: DO_CAT }
,false
,false
,false
,false
,false
,{ id: "throw", cat: THROW_CAT }
,false
,false
,false
,false
,false
,false
,false
,false
,false
,false
,{ id: "implements", cat: IMPLEMENTS_CAT }
,{ id: "case", cat: CASE_CAT }
,false
,false
,false
,{ id: "package", cat: PACKAGE_CAT }
,false
,false
,false
,false
,false
,{ id: "delete", cat: DELETE_CAT }
,false
,false
,{ id: "default", cat: DEFAULT_CAT }
,false
,{ id: "import", cat: IMPORT_CAT }
,{ id: "super", cat: SUPER_CAT }
,false
,{ id: "protected", cat: PROTECTED_CAT }
,{ id: "false", cat: FALSE_CAT }
,false
,false
,false
,{ id: "yield", cat: YIELD_CAT }
,false
,false
,false
,false
,false
,{ id: "null", cat: NULL_CAT }
,{ id: "return", cat: RETURN_CAT }
,false
,false
,false
,false
,false
,false
,false
,false
,{ id: "while", cat: WHILE_CAT }
,false
,false
,false
,false
,{ id: "with", cat: WITH_CAT }
,{ id: "new", cat: NEW_CAT }
,false
,false
,false
,false
,{ id: "private", cat: PRIVATE_CAT }
,false
,{ id: "let", cat: LET_CAT }
,false
,false
,{ id: "void", cat: VOID_CAT }
,{ id: "function", cat: FUNCTION_CAT }
,false
,{ id: "if", cat: IF_CAT }
,false
,{ id: "export", cat: EXPORT_CAT }
,false
,false
,false
,false
,false
,{ id: "in", cat: IN_CAT }
,false
,{ id: "interface", cat: INTERFACE_CAT }
,{ id: "else", cat: ELSE_CAT }
,{ id: "instanceof", cat: INSTANCEOF_CAT }
,false
,false
,false
,false
,false
,{ id: "catch", cat: CATCH_CAT }
,false
,false
,{ id: "var", cat: VAR_CAT }
,{ id: "extends", cat: EXTENDS_CAT }
,{ id: "static", cat: STATIC_CAT }
];
