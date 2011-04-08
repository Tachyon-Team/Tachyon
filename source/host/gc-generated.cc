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

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <stdint.h>

typedef uint32_t u32;
typedef intptr_t pint;
typedef intptr_t box;
typedef int8_t* ref;
typedef int8_t* rptr;

// TODO: implement this GC function to visit/move objects
ref gcUpdateRef(ref ptr);

// TODO: implement this GC function to test that a pointer points in the heap
bool ptrInHeap(rptr ptr);

const pint TAG_OTHER = 1;
const pint TAG_OBJECT = 7;
const pint TAG_ARRAY = 5;
const pint TAG_STRING = 2;
const pint TAG_FUNCTION = 6;

ref unboxRef(box boxVal)
{
	return (ref)(boxVal & ~7);
}

box boxRef(ref refVal, pint tagVal)
{
	return (box)((pint)refVal | tagVal);
}

pint getRefTag(box boxVal)
{
	return (boxVal & 7);
}

bool boxIsRef(box boxVal)
{
	return (boxVal & 3) != 0;
}

u32 getObjHeader(ref obj)
{
	return *((u32*)obj);
}

//
// x86ctx
//

u32 get_x86ctx_header(ref obj)
{
	pint offset = 0;
	offset += 0;
	ref ptr = obj;
	return *((u32*)(ptr + offset));
}

void set_x86ctx_header(ref obj, u32 val)
{
	pint offset = 0;
	offset += 0;
	ref ptr = obj;
	*((u32*)(ptr + offset)) = val;
}

ref get_x86ctx_argtbl(ref obj)
{
	pint offset = 0;
	offset += 8;
	ref ptr = obj;
	return *((ref*)(ptr + offset));
}

void set_x86ctx_argtbl(ref obj, ref val)
{
	pint offset = 0;
	offset += 8;
	ref ptr = obj;
	*((ref*)(ptr + offset)) = val;
}

box get_x86ctx_temp(ref obj)
{
	pint offset = 0;
	offset += 12;
	ref ptr = obj;
	return *((box*)(ptr + offset));
}

void set_x86ctx_temp(ref obj, box val)
{
	pint offset = 0;
	offset += 12;
	ref ptr = obj;
	*((box*)(ptr + offset)) = val;
}

box get_x86ctx_reg0(ref obj)
{
	pint offset = 0;
	offset += 16;
	ref ptr = obj;
	return *((box*)(ptr + offset));
}

void set_x86ctx_reg0(ref obj, box val)
{
	pint offset = 0;
	offset += 16;
	ref ptr = obj;
	*((box*)(ptr + offset)) = val;
}

box get_x86ctx_reg1(ref obj)
{
	pint offset = 0;
	offset += 20;
	ref ptr = obj;
	return *((box*)(ptr + offset));
}

void set_x86ctx_reg1(ref obj, box val)
{
	pint offset = 0;
	offset += 20;
	ref ptr = obj;
	*((box*)(ptr + offset)) = val;
}

box get_x86ctx_reg2(ref obj)
{
	pint offset = 0;
	offset += 24;
	ref ptr = obj;
	return *((box*)(ptr + offset));
}

void set_x86ctx_reg2(ref obj, box val)
{
	pint offset = 0;
	offset += 24;
	ref ptr = obj;
	*((box*)(ptr + offset)) = val;
}

box get_x86ctx_reg3(ref obj)
{
	pint offset = 0;
	offset += 28;
	ref ptr = obj;
	return *((box*)(ptr + offset));
}

void set_x86ctx_reg3(ref obj, box val)
{
	pint offset = 0;
	offset += 28;
	ref ptr = obj;
	*((box*)(ptr + offset)) = val;
}

box get_x86ctx_reg4(ref obj)
{
	pint offset = 0;
	offset += 32;
	ref ptr = obj;
	return *((box*)(ptr + offset));
}

void set_x86ctx_reg4(ref obj, box val)
{
	pint offset = 0;
	offset += 32;
	ref ptr = obj;
	*((box*)(ptr + offset)) = val;
}

pint comp_size_x86ctx()
{
	pint baseSize = 32;
	pint elemSize = 4;
	pint size = 1;
	pint objSize = baseSize + elemSize * size;
	return objSize;
}

pint sizeof_x86ctx(ref obj)
{
	return comp_size_x86ctx();
}

void visit_x86ctx(ref obj)
{
	ref refVal;
	box boxVal;
	rptr ptrVal;
	pint tagVal;
	refVal = get_x86ctx_argtbl(obj);
	if (!ptrInHeap((rptr)refVal))
	{
		printf("reference for field x86ctx_argtbl points outside the heap (%p)\n", (rptr)refVal);
		exit(1);
	}
	refVal = gcUpdateRef(refVal);
	set_x86ctx_argtbl(obj, refVal);
	boxVal = get_x86ctx_temp(obj);
	tagVal = getRefTag(boxVal);
	refVal = unboxRef(boxVal);
	if (!ptrInHeap((rptr)refVal))
	{
		printf("reference for field x86ctx_temp points outside the heap (%p)\n", (rptr)refVal);
		exit(1);
	}
	refVal = gcUpdateRef(refVal);
	boxVal = boxRef(refVal, tagVal);
	set_x86ctx_temp(obj, boxVal);
	boxVal = get_x86ctx_reg0(obj);
	tagVal = getRefTag(boxVal);
	refVal = unboxRef(boxVal);
	if (!ptrInHeap((rptr)refVal))
	{
		printf("reference for field x86ctx_reg0 points outside the heap (%p)\n", (rptr)refVal);
		exit(1);
	}
	refVal = gcUpdateRef(refVal);
	boxVal = boxRef(refVal, tagVal);
	set_x86ctx_reg0(obj, boxVal);
	boxVal = get_x86ctx_reg1(obj);
	tagVal = getRefTag(boxVal);
	refVal = unboxRef(boxVal);
	if (!ptrInHeap((rptr)refVal))
	{
		printf("reference for field x86ctx_reg1 points outside the heap (%p)\n", (rptr)refVal);
		exit(1);
	}
	refVal = gcUpdateRef(refVal);
	boxVal = boxRef(refVal, tagVal);
	set_x86ctx_reg1(obj, boxVal);
	boxVal = get_x86ctx_reg2(obj);
	tagVal = getRefTag(boxVal);
	refVal = unboxRef(boxVal);
	if (!ptrInHeap((rptr)refVal))
	{
		printf("reference for field x86ctx_reg2 points outside the heap (%p)\n", (rptr)refVal);
		exit(1);
	}
	refVal = gcUpdateRef(refVal);
	boxVal = boxRef(refVal, tagVal);
	set_x86ctx_reg2(obj, boxVal);
	boxVal = get_x86ctx_reg3(obj);
	tagVal = getRefTag(boxVal);
	refVal = unboxRef(boxVal);
	if (!ptrInHeap((rptr)refVal))
	{
		printf("reference for field x86ctx_reg3 points outside the heap (%p)\n", (rptr)refVal);
		exit(1);
	}
	refVal = gcUpdateRef(refVal);
	boxVal = boxRef(refVal, tagVal);
	set_x86ctx_reg3(obj, boxVal);
	boxVal = get_x86ctx_reg4(obj);
	tagVal = getRefTag(boxVal);
	refVal = unboxRef(boxVal);
	if (!ptrInHeap((rptr)refVal))
	{
		printf("reference for field x86ctx_reg4 points outside the heap (%p)\n", (rptr)refVal);
		exit(1);
	}
	refVal = gcUpdateRef(refVal);
	boxVal = boxRef(refVal, tagVal);
	set_x86ctx_reg4(obj, boxVal);
}

//
// ctx
//

u32 get_ctx_header(ref obj)
{
	pint offset = 0;
	offset += 0;
	ref ptr = obj;
	return *((u32*)(ptr + offset));
}

void set_ctx_header(ref obj, u32 val)
{
	pint offset = 0;
	offset += 0;
	ref ptr = obj;
	*((u32*)(ptr + offset)) = val;
}

ref get_ctx_argtbl(ref obj)
{
	pint offset = 0;
	offset += 8;
	ref ptr = obj;
	return *((ref*)(ptr + offset));
}

void set_ctx_argtbl(ref obj, ref val)
{
	pint offset = 0;
	offset += 8;
	ref ptr = obj;
	*((ref*)(ptr + offset)) = val;
}

box get_ctx_temp(ref obj)
{
	pint offset = 0;
	offset += 12;
	ref ptr = obj;
	return *((box*)(ptr + offset));
}

void set_ctx_temp(ref obj, box val)
{
	pint offset = 0;
	offset += 12;
	ref ptr = obj;
	*((box*)(ptr + offset)) = val;
}

box get_ctx_reg0(ref obj)
{
	pint offset = 0;
	offset += 16;
	ref ptr = obj;
	return *((box*)(ptr + offset));
}

void set_ctx_reg0(ref obj, box val)
{
	pint offset = 0;
	offset += 16;
	ref ptr = obj;
	*((box*)(ptr + offset)) = val;
}

box get_ctx_reg1(ref obj)
{
	pint offset = 0;
	offset += 20;
	ref ptr = obj;
	return *((box*)(ptr + offset));
}

void set_ctx_reg1(ref obj, box val)
{
	pint offset = 0;
	offset += 20;
	ref ptr = obj;
	*((box*)(ptr + offset)) = val;
}

box get_ctx_reg2(ref obj)
{
	pint offset = 0;
	offset += 24;
	ref ptr = obj;
	return *((box*)(ptr + offset));
}

void set_ctx_reg2(ref obj, box val)
{
	pint offset = 0;
	offset += 24;
	ref ptr = obj;
	*((box*)(ptr + offset)) = val;
}

box get_ctx_reg3(ref obj)
{
	pint offset = 0;
	offset += 28;
	ref ptr = obj;
	return *((box*)(ptr + offset));
}

void set_ctx_reg3(ref obj, box val)
{
	pint offset = 0;
	offset += 28;
	ref ptr = obj;
	*((box*)(ptr + offset)) = val;
}

box get_ctx_reg4(ref obj)
{
	pint offset = 0;
	offset += 32;
	ref ptr = obj;
	return *((box*)(ptr + offset));
}

void set_ctx_reg4(ref obj, box val)
{
	pint offset = 0;
	offset += 32;
	ref ptr = obj;
	*((box*)(ptr + offset)) = val;
}

box get_ctx_globalobj(ref obj)
{
	pint offset = 0;
	offset += 36;
	ref ptr = obj;
	return *((box*)(ptr + offset));
}

void set_ctx_globalobj(ref obj, box val)
{
	pint offset = 0;
	offset += 36;
	ref ptr = obj;
	*((box*)(ptr + offset)) = val;
}

rptr get_ctx_heapstart(ref obj)
{
	pint offset = 0;
	offset += 40;
	ref ptr = obj;
	return *((rptr*)(ptr + offset));
}

void set_ctx_heapstart(ref obj, rptr val)
{
	pint offset = 0;
	offset += 40;
	ref ptr = obj;
	*((rptr*)(ptr + offset)) = val;
}

rptr get_ctx_heaplimit(ref obj)
{
	pint offset = 0;
	offset += 44;
	ref ptr = obj;
	return *((rptr*)(ptr + offset));
}

void set_ctx_heaplimit(ref obj, rptr val)
{
	pint offset = 0;
	offset += 44;
	ref ptr = obj;
	*((rptr*)(ptr + offset)) = val;
}

rptr get_ctx_allocptr(ref obj)
{
	pint offset = 0;
	offset += 48;
	ref ptr = obj;
	return *((rptr*)(ptr + offset));
}

void set_ctx_allocptr(ref obj, rptr val)
{
	pint offset = 0;
	offset += 48;
	ref ptr = obj;
	*((rptr*)(ptr + offset)) = val;
}

box get_ctx_strtbl(ref obj)
{
	pint offset = 0;
	offset += 52;
	ref ptr = obj;
	return *((box*)(ptr + offset));
}

void set_ctx_strtbl(ref obj, box val)
{
	pint offset = 0;
	offset += 52;
	ref ptr = obj;
	*((box*)(ptr + offset)) = val;
}

box get_ctx_objproto(ref obj)
{
	pint offset = 0;
	offset += 56;
	ref ptr = obj;
	return *((box*)(ptr + offset));
}

void set_ctx_objproto(ref obj, box val)
{
	pint offset = 0;
	offset += 56;
	ref ptr = obj;
	*((box*)(ptr + offset)) = val;
}

box get_ctx_funcproto(ref obj)
{
	pint offset = 0;
	offset += 60;
	ref ptr = obj;
	return *((box*)(ptr + offset));
}

void set_ctx_funcproto(ref obj, box val)
{
	pint offset = 0;
	offset += 60;
	ref ptr = obj;
	*((box*)(ptr + offset)) = val;
}

box get_ctx_arrproto(ref obj)
{
	pint offset = 0;
	offset += 64;
	ref ptr = obj;
	return *((box*)(ptr + offset));
}

void set_ctx_arrproto(ref obj, box val)
{
	pint offset = 0;
	offset += 64;
	ref ptr = obj;
	*((box*)(ptr + offset)) = val;
}

box get_ctx_strproto(ref obj)
{
	pint offset = 0;
	offset += 68;
	ref ptr = obj;
	return *((box*)(ptr + offset));
}

void set_ctx_strproto(ref obj, box val)
{
	pint offset = 0;
	offset += 68;
	ref ptr = obj;
	*((box*)(ptr + offset)) = val;
}

box get_ctx_numproto(ref obj)
{
	pint offset = 0;
	offset += 72;
	ref ptr = obj;
	return *((box*)(ptr + offset));
}

void set_ctx_numproto(ref obj, box val)
{
	pint offset = 0;
	offset += 72;
	ref ptr = obj;
	*((box*)(ptr + offset)) = val;
}

box get_ctx_rangeerror(ref obj)
{
	pint offset = 0;
	offset += 76;
	ref ptr = obj;
	return *((box*)(ptr + offset));
}

void set_ctx_rangeerror(ref obj, box val)
{
	pint offset = 0;
	offset += 76;
	ref ptr = obj;
	*((box*)(ptr + offset)) = val;
}

box get_ctx_referror(ref obj)
{
	pint offset = 0;
	offset += 80;
	ref ptr = obj;
	return *((box*)(ptr + offset));
}

void set_ctx_referror(ref obj, box val)
{
	pint offset = 0;
	offset += 80;
	ref ptr = obj;
	*((box*)(ptr + offset)) = val;
}

box get_ctx_syntaxerror(ref obj)
{
	pint offset = 0;
	offset += 84;
	ref ptr = obj;
	return *((box*)(ptr + offset));
}

void set_ctx_syntaxerror(ref obj, box val)
{
	pint offset = 0;
	offset += 84;
	ref ptr = obj;
	*((box*)(ptr + offset)) = val;
}

box get_ctx_typeerror(ref obj)
{
	pint offset = 0;
	offset += 88;
	ref ptr = obj;
	return *((box*)(ptr + offset));
}

void set_ctx_typeerror(ref obj, box val)
{
	pint offset = 0;
	offset += 88;
	ref ptr = obj;
	*((box*)(ptr + offset)) = val;
}

box get_ctx_urierror(ref obj)
{
	pint offset = 0;
	offset += 92;
	ref ptr = obj;
	return *((box*)(ptr + offset));
}

void set_ctx_urierror(ref obj, box val)
{
	pint offset = 0;
	offset += 92;
	ref ptr = obj;
	*((box*)(ptr + offset)) = val;
}

pint comp_size_ctx()
{
	pint baseSize = 92;
	pint elemSize = 4;
	pint size = 1;
	pint objSize = baseSize + elemSize * size;
	return objSize;
}

pint sizeof_ctx(ref obj)
{
	return comp_size_ctx();
}

void visit_ctx(ref obj)
{
	ref refVal;
	box boxVal;
	rptr ptrVal;
	pint tagVal;
	refVal = get_ctx_argtbl(obj);
	if (!ptrInHeap((rptr)refVal))
	{
		printf("reference for field ctx_argtbl points outside the heap (%p)\n", (rptr)refVal);
		exit(1);
	}
	refVal = gcUpdateRef(refVal);
	set_ctx_argtbl(obj, refVal);
	boxVal = get_ctx_temp(obj);
	tagVal = getRefTag(boxVal);
	refVal = unboxRef(boxVal);
	if (!ptrInHeap((rptr)refVal))
	{
		printf("reference for field ctx_temp points outside the heap (%p)\n", (rptr)refVal);
		exit(1);
	}
	refVal = gcUpdateRef(refVal);
	boxVal = boxRef(refVal, tagVal);
	set_ctx_temp(obj, boxVal);
	boxVal = get_ctx_reg0(obj);
	tagVal = getRefTag(boxVal);
	refVal = unboxRef(boxVal);
	if (!ptrInHeap((rptr)refVal))
	{
		printf("reference for field ctx_reg0 points outside the heap (%p)\n", (rptr)refVal);
		exit(1);
	}
	refVal = gcUpdateRef(refVal);
	boxVal = boxRef(refVal, tagVal);
	set_ctx_reg0(obj, boxVal);
	boxVal = get_ctx_reg1(obj);
	tagVal = getRefTag(boxVal);
	refVal = unboxRef(boxVal);
	if (!ptrInHeap((rptr)refVal))
	{
		printf("reference for field ctx_reg1 points outside the heap (%p)\n", (rptr)refVal);
		exit(1);
	}
	refVal = gcUpdateRef(refVal);
	boxVal = boxRef(refVal, tagVal);
	set_ctx_reg1(obj, boxVal);
	boxVal = get_ctx_reg2(obj);
	tagVal = getRefTag(boxVal);
	refVal = unboxRef(boxVal);
	if (!ptrInHeap((rptr)refVal))
	{
		printf("reference for field ctx_reg2 points outside the heap (%p)\n", (rptr)refVal);
		exit(1);
	}
	refVal = gcUpdateRef(refVal);
	boxVal = boxRef(refVal, tagVal);
	set_ctx_reg2(obj, boxVal);
	boxVal = get_ctx_reg3(obj);
	tagVal = getRefTag(boxVal);
	refVal = unboxRef(boxVal);
	if (!ptrInHeap((rptr)refVal))
	{
		printf("reference for field ctx_reg3 points outside the heap (%p)\n", (rptr)refVal);
		exit(1);
	}
	refVal = gcUpdateRef(refVal);
	boxVal = boxRef(refVal, tagVal);
	set_ctx_reg3(obj, boxVal);
	boxVal = get_ctx_reg4(obj);
	tagVal = getRefTag(boxVal);
	refVal = unboxRef(boxVal);
	if (!ptrInHeap((rptr)refVal))
	{
		printf("reference for field ctx_reg4 points outside the heap (%p)\n", (rptr)refVal);
		exit(1);
	}
	refVal = gcUpdateRef(refVal);
	boxVal = boxRef(refVal, tagVal);
	set_ctx_reg4(obj, boxVal);
	boxVal = get_ctx_globalobj(obj);
	tagVal = getRefTag(boxVal);
	refVal = unboxRef(boxVal);
	if (!ptrInHeap((rptr)refVal))
	{
		printf("reference for field ctx_globalobj points outside the heap (%p)\n", (rptr)refVal);
		exit(1);
	}
	refVal = gcUpdateRef(refVal);
	boxVal = boxRef(refVal, tagVal);
	set_ctx_globalobj(obj, boxVal);
	ptrVal = get_ctx_heapstart(obj);
	if (ptrInHeap(ptrVal))
	{
		printf("pointer for field ctx_heapstart points inside the heap (%p)\n", ptrVal);
		exit(1);
	}
	ptrVal = get_ctx_heaplimit(obj);
	if (ptrInHeap(ptrVal))
	{
		printf("pointer for field ctx_heaplimit points inside the heap (%p)\n", ptrVal);
		exit(1);
	}
	ptrVal = get_ctx_allocptr(obj);
	if (ptrInHeap(ptrVal))
	{
		printf("pointer for field ctx_allocptr points inside the heap (%p)\n", ptrVal);
		exit(1);
	}
	boxVal = get_ctx_strtbl(obj);
	tagVal = getRefTag(boxVal);
	refVal = unboxRef(boxVal);
	if (!ptrInHeap((rptr)refVal))
	{
		printf("reference for field ctx_strtbl points outside the heap (%p)\n", (rptr)refVal);
		exit(1);
	}
	refVal = gcUpdateRef(refVal);
	boxVal = boxRef(refVal, tagVal);
	set_ctx_strtbl(obj, boxVal);
	boxVal = get_ctx_objproto(obj);
	tagVal = getRefTag(boxVal);
	refVal = unboxRef(boxVal);
	if (!ptrInHeap((rptr)refVal))
	{
		printf("reference for field ctx_objproto points outside the heap (%p)\n", (rptr)refVal);
		exit(1);
	}
	refVal = gcUpdateRef(refVal);
	boxVal = boxRef(refVal, tagVal);
	set_ctx_objproto(obj, boxVal);
	boxVal = get_ctx_funcproto(obj);
	tagVal = getRefTag(boxVal);
	refVal = unboxRef(boxVal);
	if (!ptrInHeap((rptr)refVal))
	{
		printf("reference for field ctx_funcproto points outside the heap (%p)\n", (rptr)refVal);
		exit(1);
	}
	refVal = gcUpdateRef(refVal);
	boxVal = boxRef(refVal, tagVal);
	set_ctx_funcproto(obj, boxVal);
	boxVal = get_ctx_arrproto(obj);
	tagVal = getRefTag(boxVal);
	refVal = unboxRef(boxVal);
	if (!ptrInHeap((rptr)refVal))
	{
		printf("reference for field ctx_arrproto points outside the heap (%p)\n", (rptr)refVal);
		exit(1);
	}
	refVal = gcUpdateRef(refVal);
	boxVal = boxRef(refVal, tagVal);
	set_ctx_arrproto(obj, boxVal);
	boxVal = get_ctx_strproto(obj);
	tagVal = getRefTag(boxVal);
	refVal = unboxRef(boxVal);
	if (!ptrInHeap((rptr)refVal))
	{
		printf("reference for field ctx_strproto points outside the heap (%p)\n", (rptr)refVal);
		exit(1);
	}
	refVal = gcUpdateRef(refVal);
	boxVal = boxRef(refVal, tagVal);
	set_ctx_strproto(obj, boxVal);
	boxVal = get_ctx_numproto(obj);
	tagVal = getRefTag(boxVal);
	refVal = unboxRef(boxVal);
	if (!ptrInHeap((rptr)refVal))
	{
		printf("reference for field ctx_numproto points outside the heap (%p)\n", (rptr)refVal);
		exit(1);
	}
	refVal = gcUpdateRef(refVal);
	boxVal = boxRef(refVal, tagVal);
	set_ctx_numproto(obj, boxVal);
	boxVal = get_ctx_rangeerror(obj);
	tagVal = getRefTag(boxVal);
	refVal = unboxRef(boxVal);
	if (!ptrInHeap((rptr)refVal))
	{
		printf("reference for field ctx_rangeerror points outside the heap (%p)\n", (rptr)refVal);
		exit(1);
	}
	refVal = gcUpdateRef(refVal);
	boxVal = boxRef(refVal, tagVal);
	set_ctx_rangeerror(obj, boxVal);
	boxVal = get_ctx_referror(obj);
	tagVal = getRefTag(boxVal);
	refVal = unboxRef(boxVal);
	if (!ptrInHeap((rptr)refVal))
	{
		printf("reference for field ctx_referror points outside the heap (%p)\n", (rptr)refVal);
		exit(1);
	}
	refVal = gcUpdateRef(refVal);
	boxVal = boxRef(refVal, tagVal);
	set_ctx_referror(obj, boxVal);
	boxVal = get_ctx_syntaxerror(obj);
	tagVal = getRefTag(boxVal);
	refVal = unboxRef(boxVal);
	if (!ptrInHeap((rptr)refVal))
	{
		printf("reference for field ctx_syntaxerror points outside the heap (%p)\n", (rptr)refVal);
		exit(1);
	}
	refVal = gcUpdateRef(refVal);
	boxVal = boxRef(refVal, tagVal);
	set_ctx_syntaxerror(obj, boxVal);
	boxVal = get_ctx_typeerror(obj);
	tagVal = getRefTag(boxVal);
	refVal = unboxRef(boxVal);
	if (!ptrInHeap((rptr)refVal))
	{
		printf("reference for field ctx_typeerror points outside the heap (%p)\n", (rptr)refVal);
		exit(1);
	}
	refVal = gcUpdateRef(refVal);
	boxVal = boxRef(refVal, tagVal);
	set_ctx_typeerror(obj, boxVal);
	boxVal = get_ctx_urierror(obj);
	tagVal = getRefTag(boxVal);
	refVal = unboxRef(boxVal);
	if (!ptrInHeap((rptr)refVal))
	{
		printf("reference for field ctx_urierror points outside the heap (%p)\n", (rptr)refVal);
		exit(1);
	}
	refVal = gcUpdateRef(refVal);
	boxVal = boxRef(refVal, tagVal);
	set_ctx_urierror(obj, boxVal);
}

//
// hashtbl
//

u32 get_hashtbl_header(box obj)
{
	pint offset = 0;
	offset += 0;
	ref ptr = unboxRef(obj);
	return *((u32*)(ptr + offset));
}

void set_hashtbl_header(box obj, u32 val)
{
	pint offset = 0;
	offset += 0;
	ref ptr = unboxRef(obj);
	*((u32*)(ptr + offset)) = val;
}

box get_hashtbl_tbl_key(box obj, pint idx0)
{
	pint offset = 0;
	offset += 8;
	offset += 8 * idx0;
	offset += 0;
	ref ptr = unboxRef(obj);
	return *((box*)(ptr + offset));
}

void set_hashtbl_tbl_key(box obj, pint idx0, box val)
{
	pint offset = 0;
	offset += 8;
	offset += 8 * idx0;
	offset += 0;
	ref ptr = unboxRef(obj);
	*((box*)(ptr + offset)) = val;
}

box get_hashtbl_tbl_val(box obj, pint idx0)
{
	pint offset = 0;
	offset += 8;
	offset += 8 * idx0;
	offset += 4;
	ref ptr = unboxRef(obj);
	return *((box*)(ptr + offset));
}

void set_hashtbl_tbl_val(box obj, pint idx0, box val)
{
	pint offset = 0;
	offset += 8;
	offset += 8 * idx0;
	offset += 4;
	ref ptr = unboxRef(obj);
	*((box*)(ptr + offset)) = val;
}

pint comp_size_hashtbl(pint size)
{
	pint baseSize = 8;
	pint elemSize = 8;
	pint objSize = baseSize + elemSize * size;
	return objSize;
}

pint sizeof_hashtbl(box obj)
{
	pint size = get_hashtbl_size(obj);
	return comp_size_hashtbl(size);
}

void visit_hashtbl(box obj)
{
	ref refVal;
	box boxVal;
	rptr ptrVal;
	pint tagVal;
	pint size = get_hashtbl_size(obj);
	for (pint i0 = 0; i0 < size; ++i0)
	{
		boxVal = get_hashtbl_tbl_key(obj, i0);
		tagVal = getRefTag(boxVal);
		refVal = unboxRef(boxVal);
		if (!ptrInHeap((rptr)refVal))
		{
			printf("reference for field hashtbl_tbl_key points outside the heap (%p)\n", (rptr)refVal);
			exit(1);
		}
		refVal = gcUpdateRef(refVal);
		boxVal = boxRef(refVal, tagVal);
		set_hashtbl_tbl_key(obj, i0, boxVal);
		boxVal = get_hashtbl_tbl_val(obj, i0);
		tagVal = getRefTag(boxVal);
		refVal = unboxRef(boxVal);
		if (!ptrInHeap((rptr)refVal))
		{
			printf("reference for field hashtbl_tbl_val points outside the heap (%p)\n", (rptr)refVal);
			exit(1);
		}
		refVal = gcUpdateRef(refVal);
		boxVal = boxRef(refVal, tagVal);
		set_hashtbl_tbl_val(obj, i0, boxVal);
	}
}

//
// obj
//

u32 get_obj_header(box obj)
{
	pint offset = 0;
	offset += 0;
	ref ptr = unboxRef(obj);
	return *((u32*)(ptr + offset));
}

void set_obj_header(box obj, u32 val)
{
	pint offset = 0;
	offset += 0;
	ref ptr = unboxRef(obj);
	*((u32*)(ptr + offset)) = val;
}

box get_obj_proto(box obj)
{
	pint offset = 0;
	offset += 4;
	ref ptr = unboxRef(obj);
	return *((box*)(ptr + offset));
}

void set_obj_proto(box obj, box val)
{
	pint offset = 0;
	offset += 4;
	ref ptr = unboxRef(obj);
	*((box*)(ptr + offset)) = val;
}

box get_obj_tbl(box obj)
{
	pint offset = 0;
	offset += 8;
	ref ptr = unboxRef(obj);
	return *((box*)(ptr + offset));
}

void set_obj_tbl(box obj, box val)
{
	pint offset = 0;
	offset += 8;
	ref ptr = unboxRef(obj);
	*((box*)(ptr + offset)) = val;
}

pint comp_size_obj()
{
	pint baseSize = 12;
	pint elemSize = 4;
	pint size = 1;
	pint objSize = baseSize + elemSize * size;
	return objSize;
}

pint sizeof_obj(box obj)
{
	return comp_size_obj();
}

void visit_obj(box obj)
{
	ref refVal;
	box boxVal;
	rptr ptrVal;
	pint tagVal;
	boxVal = get_obj_proto(obj);
	tagVal = getRefTag(boxVal);
	refVal = unboxRef(boxVal);
	if (!ptrInHeap((rptr)refVal))
	{
		printf("reference for field obj_proto points outside the heap (%p)\n", (rptr)refVal);
		exit(1);
	}
	refVal = gcUpdateRef(refVal);
	boxVal = boxRef(refVal, tagVal);
	set_obj_proto(obj, boxVal);
	boxVal = get_obj_tbl(obj);
	tagVal = getRefTag(boxVal);
	refVal = unboxRef(boxVal);
	if (!ptrInHeap((rptr)refVal))
	{
		printf("reference for field obj_tbl points outside the heap (%p)\n", (rptr)refVal);
		exit(1);
	}
	refVal = gcUpdateRef(refVal);
	boxVal = boxRef(refVal, tagVal);
	set_obj_tbl(obj, boxVal);
}

//
// arrtbl
//

u32 get_arrtbl_header(box obj)
{
	pint offset = 0;
	offset += 0;
	ref ptr = unboxRef(obj);
	return *((u32*)(ptr + offset));
}

void set_arrtbl_header(box obj, u32 val)
{
	pint offset = 0;
	offset += 0;
	ref ptr = unboxRef(obj);
	*((u32*)(ptr + offset)) = val;
}

box get_arrtbl_tbl(box obj, pint idx0)
{
	pint offset = 0;
	offset += 8;
	offset += 4 * idx0;
	ref ptr = unboxRef(obj);
	return *((box*)(ptr + offset));
}

void set_arrtbl_tbl(box obj, pint idx0, box val)
{
	pint offset = 0;
	offset += 8;
	offset += 4 * idx0;
	ref ptr = unboxRef(obj);
	*((box*)(ptr + offset)) = val;
}

pint comp_size_arrtbl(pint size)
{
	pint baseSize = 8;
	pint elemSize = 4;
	pint objSize = baseSize + elemSize * size;
	return objSize;
}

pint sizeof_arrtbl(box obj)
{
	pint size = get_arrtbl_size(obj);
	return comp_size_arrtbl(size);
}

void visit_arrtbl(box obj)
{
	ref refVal;
	box boxVal;
	rptr ptrVal;
	pint tagVal;
	pint size = get_arrtbl_size(obj);
	for (pint i0 = 0; i0 < size; ++i0)
	{
		boxVal = get_arrtbl_tbl(obj, i0);
		tagVal = getRefTag(boxVal);
		refVal = unboxRef(boxVal);
		if (!ptrInHeap((rptr)refVal))
		{
			printf("reference for field arrtbl_tbl points outside the heap (%p)\n", (rptr)refVal);
			exit(1);
		}
		refVal = gcUpdateRef(refVal);
		boxVal = boxRef(refVal, tagVal);
		set_arrtbl_tbl(obj, i0, boxVal);
	}
}

//
// arr
//

u32 get_arr_header(box obj)
{
	pint offset = 0;
	offset += 0;
	ref ptr = unboxRef(obj);
	return *((u32*)(ptr + offset));
}

void set_arr_header(box obj, u32 val)
{
	pint offset = 0;
	offset += 0;
	ref ptr = unboxRef(obj);
	*((u32*)(ptr + offset)) = val;
}

box get_arr_proto(box obj)
{
	pint offset = 0;
	offset += 4;
	ref ptr = unboxRef(obj);
	return *((box*)(ptr + offset));
}

void set_arr_proto(box obj, box val)
{
	pint offset = 0;
	offset += 4;
	ref ptr = unboxRef(obj);
	*((box*)(ptr + offset)) = val;
}

box get_arr_tbl(box obj)
{
	pint offset = 0;
	offset += 8;
	ref ptr = unboxRef(obj);
	return *((box*)(ptr + offset));
}

void set_arr_tbl(box obj, box val)
{
	pint offset = 0;
	offset += 8;
	ref ptr = unboxRef(obj);
	*((box*)(ptr + offset)) = val;
}

box get_arr_arr(box obj)
{
	pint offset = 0;
	offset += 16;
	ref ptr = unboxRef(obj);
	return *((box*)(ptr + offset));
}

void set_arr_arr(box obj, box val)
{
	pint offset = 0;
	offset += 16;
	ref ptr = unboxRef(obj);
	*((box*)(ptr + offset)) = val;
}

pint comp_size_arr()
{
	pint baseSize = 20;
	pint elemSize = 4;
	pint size = 1;
	pint objSize = baseSize + elemSize * size;
	return objSize;
}

pint sizeof_arr(box obj)
{
	return comp_size_arr();
}

void visit_arr(box obj)
{
	ref refVal;
	box boxVal;
	rptr ptrVal;
	pint tagVal;
	boxVal = get_arr_proto(obj);
	tagVal = getRefTag(boxVal);
	refVal = unboxRef(boxVal);
	if (!ptrInHeap((rptr)refVal))
	{
		printf("reference for field arr_proto points outside the heap (%p)\n", (rptr)refVal);
		exit(1);
	}
	refVal = gcUpdateRef(refVal);
	boxVal = boxRef(refVal, tagVal);
	set_arr_proto(obj, boxVal);
	boxVal = get_arr_tbl(obj);
	tagVal = getRefTag(boxVal);
	refVal = unboxRef(boxVal);
	if (!ptrInHeap((rptr)refVal))
	{
		printf("reference for field arr_tbl points outside the heap (%p)\n", (rptr)refVal);
		exit(1);
	}
	refVal = gcUpdateRef(refVal);
	boxVal = boxRef(refVal, tagVal);
	set_arr_tbl(obj, boxVal);
	boxVal = get_arr_arr(obj);
	tagVal = getRefTag(boxVal);
	refVal = unboxRef(boxVal);
	if (!ptrInHeap((rptr)refVal))
	{
		printf("reference for field arr_arr points outside the heap (%p)\n", (rptr)refVal);
		exit(1);
	}
	refVal = gcUpdateRef(refVal);
	boxVal = boxRef(refVal, tagVal);
	set_arr_arr(obj, boxVal);
}

//
// str
//

u32 get_str_header(box obj)
{
	pint offset = 0;
	offset += 0;
	ref ptr = unboxRef(obj);
	return *((u32*)(ptr + offset));
}

void set_str_header(box obj, u32 val)
{
	pint offset = 0;
	offset += 0;
	ref ptr = unboxRef(obj);
	*((u32*)(ptr + offset)) = val;
}

pint comp_size_str(pint size)
{
	pint baseSize = 12;
	pint elemSize = 2;
	pint objSize = baseSize + elemSize * size;
	return objSize;
}

pint sizeof_str(box obj)
{
	pint size = get_str_size(obj);
	return comp_size_str(size);
}

void visit_str(box obj)
{
	ref refVal;
	box boxVal;
	rptr ptrVal;
	pint tagVal;
	pint size = get_str_size(obj);
}

//
// strtbl
//

u32 get_strtbl_header(box obj)
{
	pint offset = 0;
	offset += 0;
	ref ptr = unboxRef(obj);
	return *((u32*)(ptr + offset));
}

void set_strtbl_header(box obj, u32 val)
{
	pint offset = 0;
	offset += 0;
	ref ptr = unboxRef(obj);
	*((u32*)(ptr + offset)) = val;
}

box get_strtbl_tbl(box obj, pint idx0)
{
	pint offset = 0;
	offset += 12;
	offset += 4 * idx0;
	ref ptr = unboxRef(obj);
	return *((box*)(ptr + offset));
}

void set_strtbl_tbl(box obj, pint idx0, box val)
{
	pint offset = 0;
	offset += 12;
	offset += 4 * idx0;
	ref ptr = unboxRef(obj);
	*((box*)(ptr + offset)) = val;
}

pint comp_size_strtbl(pint size)
{
	pint baseSize = 12;
	pint elemSize = 4;
	pint objSize = baseSize + elemSize * size;
	return objSize;
}

pint sizeof_strtbl(box obj)
{
	pint size = get_strtbl_size(obj);
	return comp_size_strtbl(size);
}

void visit_strtbl(box obj)
{
	ref refVal;
	box boxVal;
	rptr ptrVal;
	pint tagVal;
	pint size = get_strtbl_size(obj);
	for (pint i0 = 0; i0 < size; ++i0)
	{
		boxVal = get_strtbl_tbl(obj, i0);
		tagVal = getRefTag(boxVal);
		refVal = unboxRef(boxVal);
		if (!ptrInHeap((rptr)refVal))
		{
			printf("reference for field strtbl_tbl points outside the heap (%p)\n", (rptr)refVal);
			exit(1);
		}
		refVal = gcUpdateRef(refVal);
		boxVal = boxRef(refVal, tagVal);
		set_strtbl_tbl(obj, i0, boxVal);
	}
}

//
// clos
//

u32 get_clos_header(box obj)
{
	pint offset = 0;
	offset += 0;
	ref ptr = unboxRef(obj);
	return *((u32*)(ptr + offset));
}

void set_clos_header(box obj, u32 val)
{
	pint offset = 0;
	offset += 0;
	ref ptr = unboxRef(obj);
	*((u32*)(ptr + offset)) = val;
}

box get_clos_proto(box obj)
{
	pint offset = 0;
	offset += 4;
	ref ptr = unboxRef(obj);
	return *((box*)(ptr + offset));
}

void set_clos_proto(box obj, box val)
{
	pint offset = 0;
	offset += 4;
	ref ptr = unboxRef(obj);
	*((box*)(ptr + offset)) = val;
}

box get_clos_tbl(box obj)
{
	pint offset = 0;
	offset += 8;
	ref ptr = unboxRef(obj);
	return *((box*)(ptr + offset));
}

void set_clos_tbl(box obj, box val)
{
	pint offset = 0;
	offset += 8;
	ref ptr = unboxRef(obj);
	*((box*)(ptr + offset)) = val;
}

rptr get_clos_funcptr(box obj)
{
	pint offset = 0;
	offset += 16;
	ref ptr = unboxRef(obj);
	return *((rptr*)(ptr + offset));
}

void set_clos_funcptr(box obj, rptr val)
{
	pint offset = 0;
	offset += 16;
	ref ptr = unboxRef(obj);
	*((rptr*)(ptr + offset)) = val;
}

box get_clos_cells(box obj, pint idx0)
{
	pint offset = 0;
	offset += 24;
	offset += 4 * idx0;
	ref ptr = unboxRef(obj);
	return *((box*)(ptr + offset));
}

void set_clos_cells(box obj, pint idx0, box val)
{
	pint offset = 0;
	offset += 24;
	offset += 4 * idx0;
	ref ptr = unboxRef(obj);
	*((box*)(ptr + offset)) = val;
}

pint comp_size_clos(pint size)
{
	pint baseSize = 24;
	pint elemSize = 4;
	pint objSize = baseSize + elemSize * size;
	return objSize;
}

pint sizeof_clos(box obj)
{
	pint size = get_clos_size(obj);
	return comp_size_clos(size);
}

void visit_clos(box obj)
{
	ref refVal;
	box boxVal;
	rptr ptrVal;
	pint tagVal;
	pint size = get_clos_size(obj);
	boxVal = get_clos_proto(obj);
	tagVal = getRefTag(boxVal);
	refVal = unboxRef(boxVal);
	if (!ptrInHeap((rptr)refVal))
	{
		printf("reference for field clos_proto points outside the heap (%p)\n", (rptr)refVal);
		exit(1);
	}
	refVal = gcUpdateRef(refVal);
	boxVal = boxRef(refVal, tagVal);
	set_clos_proto(obj, boxVal);
	boxVal = get_clos_tbl(obj);
	tagVal = getRefTag(boxVal);
	refVal = unboxRef(boxVal);
	if (!ptrInHeap((rptr)refVal))
	{
		printf("reference for field clos_tbl points outside the heap (%p)\n", (rptr)refVal);
		exit(1);
	}
	refVal = gcUpdateRef(refVal);
	boxVal = boxRef(refVal, tagVal);
	set_clos_tbl(obj, boxVal);
	ptrVal = get_clos_funcptr(obj);
	if (ptrInHeap(ptrVal))
	{
		printf("pointer for field clos_funcptr points inside the heap (%p)\n", ptrVal);
		exit(1);
	}
	for (pint i0 = 0; i0 < size; ++i0)
	{
		boxVal = get_clos_cells(obj, i0);
		tagVal = getRefTag(boxVal);
		refVal = unboxRef(boxVal);
		if (!ptrInHeap((rptr)refVal))
		{
			printf("reference for field clos_cells points outside the heap (%p)\n", (rptr)refVal);
			exit(1);
		}
		refVal = gcUpdateRef(refVal);
		boxVal = boxRef(refVal, tagVal);
		set_clos_cells(obj, i0, boxVal);
	}
}

//
// cell
//

u32 get_cell_header(box obj)
{
	pint offset = 0;
	offset += 0;
	ref ptr = unboxRef(obj);
	return *((u32*)(ptr + offset));
}

void set_cell_header(box obj, u32 val)
{
	pint offset = 0;
	offset += 0;
	ref ptr = unboxRef(obj);
	*((u32*)(ptr + offset)) = val;
}

box get_cell_val(box obj)
{
	pint offset = 0;
	offset += 4;
	ref ptr = unboxRef(obj);
	return *((box*)(ptr + offset));
}

void set_cell_val(box obj, box val)
{
	pint offset = 0;
	offset += 4;
	ref ptr = unboxRef(obj);
	*((box*)(ptr + offset)) = val;
}

pint comp_size_cell()
{
	pint baseSize = 4;
	pint elemSize = 4;
	pint size = 1;
	pint objSize = baseSize + elemSize * size;
	return objSize;
}

pint sizeof_cell(box obj)
{
	return comp_size_cell();
}

void visit_cell(box obj)
{
	ref refVal;
	box boxVal;
	rptr ptrVal;
	pint tagVal;
	boxVal = get_cell_val(obj);
	tagVal = getRefTag(boxVal);
	refVal = unboxRef(boxVal);
	if (!ptrInHeap((rptr)refVal))
	{
		printf("reference for field cell_val points outside the heap (%p)\n", (rptr)refVal);
		exit(1);
	}
	refVal = gcUpdateRef(refVal);
	boxVal = boxRef(refVal, tagVal);
	set_cell_val(obj, boxVal);
}

//
// memblock
//

u32 get_memblock_header(box obj)
{
	pint offset = 0;
	offset += 0;
	ref ptr = unboxRef(obj);
	return *((u32*)(ptr + offset));
}

void set_memblock_header(box obj, u32 val)
{
	pint offset = 0;
	offset += 0;
	ref ptr = unboxRef(obj);
	*((u32*)(ptr + offset)) = val;
}

rptr get_memblock_ptr(box obj)
{
	pint offset = 0;
	offset += 8;
	ref ptr = unboxRef(obj);
	return *((rptr*)(ptr + offset));
}

void set_memblock_ptr(box obj, rptr val)
{
	pint offset = 0;
	offset += 8;
	ref ptr = unboxRef(obj);
	*((rptr*)(ptr + offset)) = val;
}

pint comp_size_memblock()
{
	pint baseSize = 8;
	pint elemSize = 4;
	pint size = 1;
	pint objSize = baseSize + elemSize * size;
	return objSize;
}

pint sizeof_memblock(box obj)
{
	return comp_size_memblock();
}

void visit_memblock(box obj)
{
	ref refVal;
	box boxVal;
	rptr ptrVal;
	pint tagVal;
	ptrVal = get_memblock_ptr(obj);
	if (ptrInHeap(ptrVal))
	{
		printf("pointer for field memblock_ptr points inside the heap (%p)\n", ptrVal);
		exit(1);
	}
}

//
// High-level visit function.
//
void visitRef(ref obj)
{
	u32 typeId = getObjHeader(obj);
	switch (typeId)
	{
		case 1:
		visit_x86ctx(obj);
		break;
		case 2:
		visit_ctx(obj);
		break;
		case 4:
		visit_hashtbl(boxRef(obj, TAG_OTHER));
		break;
		case 5:
		visit_obj(boxRef(obj, TAG_OBJECT));
		break;
		case 6:
		visit_arrtbl(boxRef(obj, TAG_OTHER));
		break;
		case 7:
		visit_arr(boxRef(obj, TAG_ARRAY));
		break;
		case 8:
		visit_str(boxRef(obj, TAG_STRING));
		break;
		case 9:
		visit_strtbl(boxRef(obj, TAG_OTHER));
		break;
		case 10:
		visit_clos(boxRef(obj, TAG_FUNCTION));
		break;
		case 11:
		visit_cell(boxRef(obj, TAG_OTHER));
		break;
		case 12:
		visit_memblock(boxRef(obj, TAG_OTHER));
		break;
		default:
		printf("invalid type id value (%i)\n", typeId);
		exit(1);
	}
}

