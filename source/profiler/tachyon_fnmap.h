#ifndef TACHON_FNMAP_H_
#define TACHON_FNMAP_H_   

void register_function(const char *name, void *start, void *end);
void print_profile_data();
void destroy_fnmap();

#endif
