/**
@fileOverview
This file includes wrapper code for low-level, raw memory access, which is
necessary for bootstrapping.
<br><br>
When running under D8, these functions will be calling external C/C++ host
functions to do their work, and pointers will be represented as ordinary
JavaScript number values (doubles).
<br><br>
In fully bootstrapped mode, however, the VM will recognize calls to these
functions and replace them by native instructions directly. Pointers will then
be represented as unsigned integers which machine instructions can operate on
directly.

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/

/**
@namespace
*/
memory = makeModule("memory");

/**
Add a signed offset to a pointer value
@param ptr pointer to raw memory
@param val signed offset
@returns the adjusted pointer
*/
memory.ptrAdd = function(ptr, offset)
{
}

/**
Compute the difference between two pointers
@param ptr1 first pointer
@param ptr2 second pointer
@returns the integer pointer difference
*/
memory.ptrSub = function(ptr1, ptr2)
{
}

/**
Write an 8-bit unsigned integer to memory
@param ptr pointer to raw memory
@param val value to write
*/
memory.writeU8 = function(ptr, val)
{
}

/**
Read an 8-bit unsigned integer from memory
@param ptr pointer to raw memory
@returns value read
*/
memory.readU8 = function(ptr)
{
}

