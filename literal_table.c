#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include "literal_table.h"
#include "utilities.h"

static literal_table_entry_t *first;
static literal_table_entry_t *last;
static unsigned int next_word_offset;
static bool iterating;
static literal_table_entry_t *iteration_next;

// Initialize the literal table
void literal_table_initialize(void) {
    first = NULL;
    last = NULL;
    next_word_offset = 0;
    iterating = false;
    iteration_next = NULL;
}

// Lookup a literal by its string representation and value
unsigned int literal_table_lookup(const char *val_string, int value) {
    int ret = literal_table_find_offset(val_string, value);
    if (ret >= 0) {
        return ret;
    }
    literal_table_entry_t *new_entry = (literal_table_entry_t *) malloc(sizeof(literal_table_entry_t));
    if (new_entry == NULL) {
        bail_with_error("No space to allocate new literal table entry!");
    }
    new_entry->text = val_string;
    new_entry->value = value;
    new_entry->next = NULL;
    ret = next_word_offset;
    new_entry->offset = next_word_offset++;
    if (first == NULL) {
        first = new_entry;
        last = new_entry;
    } else {
        last->next = new_entry;
        last = new_entry;
    }
    return ret;
}

// Find the offset of a literal in the table
static int literal_table_find_offset(const char *sought, int value) {
    literal_table_entry_t *entry = first;
    while (entry != NULL) {
        if (strcmp(entry->text, sought) == 0) {
            return entry->offset;
        }
        entry = entry->next;
    }
    return -1;
}

// Start iterating over the literal table
void literal_table_start_iteration(void) {
    iterating = true;
    iteration_next = first;
}

// End the iteration over the literal table
void literal_table_end_iteration(void) {
    iterating = false;
}

// Check if there are more literals in the table
bool literal_table_iteration_has_next(void) {
    return iteration_next != NULL;
}

// Get the next literal in the table
int literal_table_iteration_next(void) {
    assert(iteration_next != NULL);
    int ret = iteration_next->value;
    iteration_next = iteration_next->next;
    return ret;
}

// Return the size (in words/entries) in the literal table
unsigned int literal_table_size(void) {
    return next_word_offset;
}
