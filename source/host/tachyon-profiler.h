#ifndef TACHYON_PROFILER_H_
#define TACHYON_PROFILER_H_

#include <stdint.h>
#include <signal.h>
#include <sys/time.h>
#include <sys/types.h>

#define PROF_OK  0
#define PROF_ERR 1


#ifndef __cplusplus
typedef enum {false, true} bool;
#endif

typedef uint32_t prof_counter; // Execution counter type
typedef uint8_t* code_address; // Code block address type

typedef enum {
    PROF_TYPE_REGULAR,
    PROF_TYPE_STOCHASTIC
} prof_type;

typedef union {
    unsigned int usec; // Constant interval in microseconds
    unsigned int (*nextInterval)(void *);
} prof_interval;

int prof_init(prof_type type, int kind, prof_interval *interval);
int prof_register_block(code_address block, size_t size);
int prof_enable();
int prof_disable();
int prof_get_counters(code_address block, prof_counter **counters, uint32_t *len);
int prof_terminate();


#endif
