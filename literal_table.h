
#ifndef _LITERAL_TABLE_H
#define _LITERAL_TABLE_H

#include <stdbool.h>
#include "machine_types.h"

// Initialize the literal table
void literal_table_initialize(void);

// Lookup a literal by its string representation and value
unsigned int literal_table_lookup(const char *val_string, int value);

// Start iterating over the literal table
void literal_table_start_iteration(void);

// End the iteration over the literal table
void literal_table_end_iteration(void);

// Check if there are more literals in the table
bool literal_table_iteration_has_next(void);

// Get the next literal in the table
int literal_table_iteration_next(void);

// Return the size (in words/entries) in the literal table
unsigned int literal_table_size(void);

#endif
