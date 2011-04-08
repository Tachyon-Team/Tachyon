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
 * A statistical profiler for the Tachyon JavaScript compiler project.
 *
 *  Created on: Oct 15, 2010
 *      Author: Bruno Dufour (dufour@iro.umontreal.ca)
 */

#include "tachyon_prof.h"
#include <stdlib.h>
#include <string.h>
#include <signal.h>

#define TACHYON_ERR_MSG "Tachyon"

/**
 * Gets the program counter (PC) from a ucontext_t structure. This is highly
 * machine-dependent, so conditional definitions will need to be included
 * to support other systems.
 */
#define GET_PC(c) (void *)(c->uc_mcontext->__ss.__rip)
#define GET_COUNTER_INDEX(pc) (unsigned int)(pc - profiler.start_addr)

static t_profiler profiler;

void onTimer(int sig, siginfo_t *info, ucontext_t *uap) {
    void *pc = GET_PC(uap);
    if (pc >= profiler.start_addr && pc < profiler.end_addr) {
        profiler.counters[GET_COUNTER_INDEX(pc)] += 1;
    }
}

int get_active_profiler(t_profiler **p) {
    *p = &profiler;
    return TACHYON_OK;
}

int enable_profiler(struct timeval *interval) {
    struct itimerval timer;

    /* Initialize timer */
    memset(&timer, 0, sizeof(timer));
    if (interval != NULL) {
        timer.it_value.tv_sec = interval->tv_sec;
        timer.it_value.tv_usec = interval->tv_usec;
        timer.it_interval.tv_sec = interval->tv_sec;
        timer.it_interval.tv_usec = interval->tv_usec;
    } else {
        timer.it_value.tv_usec = TACHYON_INTERVAL_MS * 1000;
        timer.it_interval.tv_usec = TACHYON_INTERVAL_MS * 1000;
    }

    if (setitimer(ITIMER_PROF, &timer, NULL) != 0) {
        perror(TACHYON_ERR_MSG);
        return TACHYON_ERR;
    }

    return TACHYON_OK;
}

int disable_profiler() {
    struct itimerval timer;
    memset(&timer, 0, sizeof(timer));
    if (setitimer(ITIMER_VIRTUAL, &timer, NULL) != 0) {
        perror(TACHYON_ERR_MSG);
        return TACHYON_ERR;
    }

    return TACHYON_OK;
}

int destroy_profiler() {
    if (profiler.counters != NULL) {
        free(profiler.counters);
        profiler.counters = NULL;

        profiler.start_addr = NULL;
        profiler.end_addr = NULL;
    }
}

int initialize_profiler(void *start, void *end) {
    struct sigaction action;

    unsigned int num_counters;

    profiler.start_addr = start;
    profiler.end_addr = end;
    num_counters = end - start;
    profiler.counters = (t_counter *) calloc(num_counters, sizeof(t_counter));

    /* Install SIGALRM handler */
    memset(&action, 0, sizeof(action));
    sigemptyset(&action.sa_mask);
    action.sa_sigaction = (void (*)(int, siginfo_t *, void *)) onTimer;
    action.sa_flags = SA_SIGINFO | SA_RESTART;

    if (sigaction(SIGPROF, &action, NULL) != 0) {
        perror(TACHYON_ERR_MSG);
        return TACHYON_ERR;
    }

    return TACHYON_OK;
}
