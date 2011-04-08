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

#include "tachyon_fnmap.h"
#include "tachyon_prof.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define NEW(t) (t *) malloc(sizeof(t))

typedef struct _t_function_data {
    char *name;
    void *start;
    void *end;
    struct _t_function_data *next;
} t_function_data;

static t_function_data *function_map;

void register_function(const char *name, void *start, void *end) {
    t_function_data *data = NEW(t_function_data);
    data->name = strdup(name);
    data->start = start;
    data->end = end;
    data->next = function_map;
    function_map = data;
}

void destroy_fnmap() {
    t_function_data *p, *n;
    if (function_map != NULL) {
        for (p = function_map; p != NULL; ) {
            n = p->next;
            free(p->name);
            p->name = NULL;
            free(p);
            p = n;
        }
        function_map = NULL;
    }
}

unsigned int get_counter_index(t_profiler *profiler, void *pc) {
    return (unsigned int)(pc - profiler->start_addr);
}

void print_profile_data() {
    t_function_data *p;
    unsigned int sum = 0;
    unsigned int i, exec_count;
    t_profiler *profiler;

    get_active_profiler(&profiler);

    for (i = 0; i < (unsigned int) (profiler->end_addr - profiler->start_addr); i++) {
        sum += profiler->counters[i];
    }
    fprintf(stdout, "\nTotal samples: %u\n", sum);

    for (p = function_map; p != NULL; p = p->next) {
        exec_count = 0;
        for (i = get_counter_index(profiler, p->start); i < get_counter_index(profiler, p->end); i++) {
            exec_count += profiler->counters[i];
        }

        fprintf(stdout, "  %s: %u (%0.2f%%)\n", p->name, exec_count, (100.0 * exec_count) / sum);
    }

    fflush(stdout);
}
