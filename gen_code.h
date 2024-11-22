#ifndef GEN_CODE_H
#define GEN_CODE_H

#include "ast.h"
#include "code_seq.h"
#include "bof.h"

// Initialize the code generator
void gen_code_initialize();

// Generate the BOF program header
BOFHeader gen_code_program_header(code_seq main_cs);

// Output the program to a BOFFILE
void gen_code_output_program(BOFFILE bf, code_seq main_cs);

// Output code sequences
void gen_code_output_seq(BOFFILE bf, code_seq cs);

// Output literals
void gen_code_output_literals(BOFFILE bf);

// Generate the entire program
void gen_code_program(BOFFILE bf, block_t *prog);

// Generate code for a block
code_seq gen_code_block(block_t *block);

// Generate code for constant declarations
code_seq gen_code_const_decls(const_decls_t const_decls);

// Generate code for a single constant declaration
code_seq gen_code_const_decl(const_decl_t *const_decl);

// Generate code for variable declarations
code_seq gen_code_var_decls(var_decls_t var_decls);

// Generate code for a single variable declaration
code_seq gen_code_var_decl(var_decl_t *vd);

// Generate code for the identifiers in idents
code_seq gen_code_idents(ident_list_t *idents);

// Generate code for a list of statements
code_seq gen_code_stmts(stmts_t *stmts);

// Generate code for a single statement
code_seq gen_code_stmt(stmt_t *stmt);

// Generate code for an assignment statement
code_seq gen_code_assign_stmt(assign_stmt_t *stmt);

// Generate code for a call statement
code_seq gen_code_call_stmt(call_stmt_t *stmt);

// Generate code for an if statement
code_seq gen_code_if_stmt(if_stmt_t *stmt);

// Generate code for a while statement
code_seq gen_code_while_stmt(while_stmt_t *stmt);

// Generate code for a read statement
code_seq gen_code_read_stmt(read_stmt_t *stmt);

// Generate code for a print statement
code_seq gen_code_print_stmt(print_stmt_t *stmt);

// Generate code for a block statement
code_seq gen_code_block_stmt(block_stmt_t *stmt);

// Generate code for an expression
code_seq gen_code_expr(expr_t *expr);

// Generate code for a binary operation expression
code_seq gen_code_binary_op_expr(binary_op_expr_t *expr);

// Generate code for a negated expression
code_seq gen_code_negated_expr(negated_expr_t *expr);

// Generate code for an operator
code_seq gen_code_op(token_t op);

// Generate code for a number literal
code_seq gen_code_number(int value);

// Generate code for an identifier
code_seq gen_code_ident(ident_t *id);

#endif // GEN_CODE_H
