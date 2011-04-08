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

/*
 * A simple test setup for the Tachyon statistical profiler.
 *
 *  Created on: Oct 15, 2010
 *      Author: Bruno Dufour (dufour@iro.umontreal.ca)
 */

#include <stdio.h>
#include "tachyon_prof.h"
#include "tachyon_fnmap.h"

/* These functions are used as markers for the beginning and end
 * (respectively) of the code in memory. Since GCC doesn't reorder functions,
 * this kludge works in practice. */
void BEGIN();
void END();

void BEGIN() {
    /*
     * Kludge to get the starting address for the generated code.
     * 
     * MUST ALWAYS APPEAR AS THE FIRST FUNCTION
     */
}

/* Computes the nth number in the fibonacci sequence. */
int fib(int n) {
    if (n <= 1) return n;
    return fib(n-1) + fib(n-2);
}


/* Tests if a number is a prime in the most naive and
 * inefficient way possible. */
int prime(int n) {
    int i;
    for (i = 2; i < n; i++) {
        if ((n % i) == 0) {
            return 0;
        }
    }

    return 1;
}

int main(int argc, char **argv) {
    int val;
    int n = 40;

    /* Setup the profiler */
    initialize_profiler(&BEGIN, &END);
    enable_profiler(NULL);
    register_function("fib", &fib, &prime);
    register_function("prime", &prime, &main);
    register_function("main", &main, &END);

    /* Fibonacci test */
    fprintf(stdout, "Computing fib\n");
    fflush(stdout);
    val = fib(n);
    fprintf(stdout, "Fib(%d) = %d\n", n, val);
    fflush(stdout);

    /* Primality test */
    fprintf(stdout, "Computing primality\n");
    fflush(stdout);
    n = 982451653;
    if (prime(n)) {
        fprintf(stdout, "%d is prime\n", n);
    } else {
        fprintf(stdout, "%d is NOT prime\n", n);
    }

    disable_profiler();
    print_profile_data();

    return 0;
}

void END() {
    /*
     * Kludge to get the ending address for the generated code.
     * 
     * MUST ALWAYS APPEAR AS THE LAST FUNCTION
     */
}
