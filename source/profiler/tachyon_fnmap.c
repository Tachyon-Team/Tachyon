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
