/**
@fileOverview
Implementation of the Foreign Function Interface (FFI) to interface with
the C/C++ code required by Tachyon.

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/

/*

FFI version 0.1, minimal pour bootstrap
---------------------------------------

// Enregistrement d'une fonction C (prototype)
// Type descend de IRValue, comme IRFunction
// Placeholder, contient:
// - nom de fonction
// - types des arguments
// - type de retour
var ffiFuncObj = new ffi.CFunction('printBar', ['char*', 'int'], 'int')

// Appel vers C, linké statiquement plus tard
// Le backend sait que call_ffi utilise la convention d'appel C
var retVal = iir.call_ffi(ffiFuncObj, args...);

// Marc: tous arguments boxés, fixnums ou strings
// new ffi.CFunction crée des wrappers automatiquement?

// Wrapper généré dynamiquement
box printBar(box s, box i)
{
    rptr str = malloc_and_copy_str(s);
    i32 iv = unboxInt(i);

    i32 retval = iir.call_ffi(ffi_func, str, iv);

    free_str(s);

    return boxInt(retVal);
}

FFI version 0.2, introduction de callbacks
------------------------------------------

// On passe un pointeur vers une fonction JS
// Un proxy (code stub) appellable à partir de C est créé
ffi.regCallback(jsFuncObj, ['char*', 'int'], 'int');

FFI version 0.3
---------------

- Parsing de headers C
- Création de code C qui accède aux objets JS (classe C++)

*/








