#ifdef __cplusplus
extern "C" {
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <assert.h>
#include "tachyon-profiler.h"


#define PROF_DEBUG

#ifdef PROF_DEBUG
#define BEGIN(op) fprintf(stderr, "Tachyon profiler> %s: ", op)
#define END(result) fprintf(stderr, "[%s]\n", ((result) ? "OK" : "ERR"))
#else
#define BEGIN(op)
#define END(result)
#endif

/**
 * Gets the program counter (PC) from a ucontext_t structure. This is highly
 * machine-dependent, so conditional definitions will need to be included
 * to support other systems.
 */

#if defined(__APPLE__)
#include <sys/ucontext.h>
#ifdef __cplusplus
#define GET_PC(c) (code_address)(c->uc_mcontext->ss.eip)
#else
#define GET_PC(c) (code_address)(c->uc_mcontext->__ss.__rip)
#endif
#else
#error Platform not supported
#endif

#define NEW(t) (t*) malloc(sizeof(t))
#define NEW_ARRAY(t,size) (t*) calloc(size, sizeof(t))

#define GET_BLOCK_LEN(block)        (block->end_addr - block->start_addr)
#define GET_COUNTER_INDEX(block,pc) (unsigned int)(pc - block->start_addr)
#define CONTAINS(block,pc)          (pc >= block->start_addr && pc < block->end_addr)

#define PROF_DEFAULT_INTERVAL_USEC 1000

typedef enum {
    PROF_STATE_CREATED,
    PROF_STATE_ENABLED,
    PROF_STATE_DISABLED,
    PROF_STATE_TERMINATED
} prof_state;

// Type definitions
typedef struct profiler profiler;

typedef struct block_info {
    code_address start_addr; // Starting address of code block
    code_address end_addr;   // Ending address of code block (exclusive)
    prof_counter *counters;  // Array of profiler counters, one per byte in code block
    struct block_info *next;
} block_info;

struct profiler {
    prof_state state;
    prof_type type;
    int timer_kind;          // One of ITIMER_REAL, ITIMER_VIRTUAL, ITIMER_PROF
    block_info *blocks;
    prof_interval interval;
    struct sigaction prev_handler;
};

// Global profiler
static profiler p;
static block_info *last_block = NULL;

#ifdef PROF_DEBUG
static unsigned int samples;
#endif

block_info *lookupBlock(code_address pc)
{
    block_info *b;
    if (last_block != NULL)
    {
        // Try the last used block first
        if (CONTAINS(last_block, pc))
        {
            return last_block;
        }
    }

    // Fall back to longer search
    // TODO: optimize this
    for (b = p.blocks; b != NULL; b = b->next)
    {
        if (CONTAINS(b, pc))
        {
            last_block = b;
            return b;
        }
    }

    // Oops, no block found. Shouldn't happen.
    return NULL;
}

/**
 * SIGPROF handler. At the moment, only records an execution of the current
 * PC address.
 */
void prof_onTimer(int sig, siginfo_t *info, ucontext_t *uap)
{
    block_info *block;
    code_address pc = GET_PC(uap);

    block = lookupBlock(pc);
    if (block != NULL)
    {
#ifdef PROF_DEBUG
        samples++;
#endif
        block->counters[GET_COUNTER_INDEX(block, pc)] += 1;
    }

    if (p.type == PROF_TYPE_STOCHASTIC)
    {
        assert(false); // TODO: Reset timer (not implemented)
    }

    if (p.prev_handler.sa_sigaction != NULL)
    {
        if (p.prev_handler.sa_flags & SA_SIGINFO)
        {
            p.prev_handler.sa_sigaction(sig, info, uap);
        }
        else
        {
            p.prev_handler.sa_handler(sig);
        }
    }
}

int prof_get_signum(profiler *p)
{
    switch (p->timer_kind)
    {
        case ITIMER_REAL:
            return SIGALRM;
        case ITIMER_VIRTUAL:
            return SIGVTALRM;
        default:
            return SIGPROF;
    }
}

int prof_enable()
{
    unsigned int interval;
    struct itimerval timer;
    struct sigaction sa;

#ifdef PROF_DEBUG
    samples = 0;
#endif
    
    if (p.state == PROF_STATE_ENABLED) return PROF_OK;
    assert (p.state == PROF_STATE_DISABLED);

    BEGIN("Enabling profiler");

    /* Install signal handler */
    memset(&sa, 0, sizeof(sa));
    sigemptyset(&sa.sa_mask);
    sa.sa_sigaction = (void (*)(int, siginfo_t *, void *)) prof_onTimer;
    sa.sa_flags = SA_SIGINFO | SA_RESTART;

    if (sigaction(prof_get_signum(&p), &sa, &(p.prev_handler)) != 0)
    {
        perror("Tachyon profiler");
                END(false);
        return PROF_ERR;
    }

    /* Initialize timer */
    memset(&timer, 0, sizeof(timer));
    switch (p.type)
    {
        case PROF_TYPE_STOCHASTIC:
            interval = p.interval.nextInterval(&p);
            timer.it_value.tv_sec = interval / 1000;
            timer.it_value.tv_usec = (interval % 1000) * 1000;
            break;
        case PROF_TYPE_REGULAR:
            if (p.interval.usec == 0) {
                p.interval.usec = PROF_DEFAULT_INTERVAL_USEC;
            }
            timer.it_value.tv_sec = p.interval.usec / 1000000;
            timer.it_value.tv_usec = (p.interval.usec % 1000000);
            timer.it_interval = timer.it_value;
            break;
        default:
            fprintf(stderr, "Unknown profiler type\n");
            END(false);
            return PROF_ERR;
    }

    if (setitimer(p.timer_kind, &timer, NULL) != 0)
    {
        perror("Tachyon profiler");
        END(false);
        return PROF_ERR;
    }

    END(true);

    p.state = PROF_STATE_ENABLED;

    return PROF_OK;
}

int prof_disable()
{
    struct itimerval timer;

    if (p.state == PROF_STATE_DISABLED) return PROF_OK;
    assert (p.state == PROF_STATE_ENABLED);

    BEGIN("Disabling profiler");

    memset(&timer, 0, sizeof(timer));
    if (setitimer(p.timer_kind, &timer, NULL) != 0)
    {
        perror("Tachyon profiler");
        END(false);
        return PROF_ERR;
    }

    // Restore previous handler
    sigaction(prof_get_signum(&p), &(p.prev_handler), NULL);

    END(true);
#ifdef PROF_DEBUG
    fprintf(stderr, "Tachyon profiler> %d sample(s) taken\n", samples);
#endif

    p.state = PROF_STATE_DISABLED;

    return PROF_OK;
}

int prof_init(prof_type type, int kind, prof_interval *interval)
{
    BEGIN("Initializing profiler");
    p.blocks = NULL;
    p.type = PROF_TYPE_REGULAR;
    p.timer_kind = ITIMER_PROF;
    p.interval = *interval;

    END(true);

    p.state = PROF_STATE_DISABLED;
    return PROF_OK;
}

int prof_register_block(code_address block, size_t size)
{
    block_info *binfo;

    assert (p.state != PROF_STATE_CREATED && p.state != PROF_STATE_TERMINATED);
    
    binfo = NEW(block_info);
    if (binfo == NULL) return PROF_ERR;

    binfo->start_addr = (code_address) block;
    binfo->end_addr = (code_address) (block + size);
    binfo->counters = NEW_ARRAY(prof_counter, size);
    if (binfo->counters == NULL)
    {
        free(binfo);
        return PROF_ERR;
    }
    binfo->next = p.blocks;
    p.blocks = binfo;

    return PROF_OK;
}

int prof_terminate()
{
    block_info *block;

    if (p.state == PROF_STATE_ENABLED) {
        prof_disable();
    }

    assert (p.state == PROF_STATE_DISABLED);

    BEGIN("Freeing profiler data");

    for (block = p.blocks; block != NULL; )
    {
        block_info *b = block;
        block = block->next;

        if (b->counters != NULL)
        {
            free(b->counters);
        }
        free(b);
    }
    p.blocks = NULL;

    END(true);

    p.state = PROF_STATE_TERMINATED;

    return PROF_OK;
}

int prof_get_counters(code_address block, prof_counter **counters, uint32_t *len)
{
    block_info *b;

    for (b = p.blocks; b != NULL; b = b->next)
    {
        if (block == b->start_addr)
        {
            /* Found block */
            if (counters != NULL)
            {
                *counters = b->counters;    
            }
            if (len != NULL)
            {
                *len = (uint32_t) GET_BLOCK_LEN(b);
            }
            return PROF_OK;
        }
    }

    fprintf(stderr, "Block not found: %p\n", block);

    /* Not found */
    return PROF_ERR;
}

#ifdef __cplusplus
}
#endif
