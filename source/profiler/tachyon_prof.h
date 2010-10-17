/*
 * A statistical profiler for the Tachyon JavaScript compiler project.
 *
 *  Created on: Oct 15, 2010
 *      Author: Bruno Dufour (dufour@iro.umontreal.ca)
 */

#ifndef TACHYON_PROF_H_
#define TACHYON_PROF_H_

#include <sys/time.h>

#define TACHYON_INTERVAL_MS 10  /* Interval between samples (in ms) */

#define TACHYON_OK         0
#define TACHYON_ERR       -1

typedef unsigned int t_counter;
typedef struct {
    void *start_addr;
    void *end_addr;
    t_counter *counters;
} t_profiler;


int enable_profiler(struct timeval *interval);
int disable_profiler();
int initialize_profiler(void *start, void *end);
int get_active_profiler(t_profiler **profiler);
int destroy_profiler();

#endif /* TACHYON_PROF_H_ */
