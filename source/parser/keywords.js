/* _________________________________________________________________________
 *
 *             Tachyon : A Self-Hosted JavaScript Virtual Machine
 *
 *
 *  This file is part of the Tachyon JavaScript project. Tachyon is
 *  distributed at:
 *  http://github.com/Tachyon-Team/Tachyon
 *
 *
 *  Copyright (c) 2011, Universite de Montreal
 *  All rights reserved.
 *
 *  This software is licensed under the following license (Modified BSD
 *  License):
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions are
 *  met:
 *    * Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *    * Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *    * Neither the name of the Universite de Montreal nor the names of its
 *      contributors may be used to endorse or promote products derived
 *      from this software without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 *  IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 *  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 *  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL UNIVERSITE DE
 *  MONTREAL BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 *  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 *  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 *  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 *  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * _________________________________________________________________________
 */

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
