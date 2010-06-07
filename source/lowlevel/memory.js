/**
@fileOverview
This file includes wrapper code for low-level, raw memory access, which is
necessary for bootstrapping. When running under D8, these functions will be
calling external C/C++ host functions to do their work. In fully bootstrapped
mode, however, the VM will recognize calls to these functions and replace them
by native instructions directly.

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/

/**
@description Adds a signed offset to a pointer value
@param ptr pointer to raw memory
@param val signed offset
@returns the adjusted pointer
*/
function memPtrAdd(ptr, offset)
{
}

/**
@description Computes the difference between two pointers
@param ptr1 first pointer
@param ptr2 second pointer
@returns the integer pointer difference
*/
function memPtrSub(ptr1, ptr2)
{
}

/**
@description Writes an 8-bit unsigned integer to memory
@param ptr pointer to raw memory
@param val value to write
*/
function memWriteU8(ptr, val)
{
}

/**
@description Read an 8-bit unsigned integer from memory
@param ptr pointer to raw memory
@returns value read
*/
function memReadU8(ptr)
{
}

