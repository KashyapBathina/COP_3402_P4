#ifndef GEN_CODE_H
#define GEN_CODE_H

#include "ast.h"
#include "code_seq.h"
#include "bof.h"

// Function declarations
void gen_code_initialize();
void gen_code_program(BOFFILE bf, block_t *prog);
code_seq gen_code_block(block_t *block);
code_seq gen_code_const_decls(const_decls_t const_decls);
code_seq gen_code_const_decl(const_decl_t *const_decl);
code_seq gen_code_const_def_list(const_def_list_t *const_def_list);
code_seq gen_code_const_def(const_def_t *const_def);
code_seq gen_code_var_decls(var_decls_t var_decls);
code_seq gen_code_var_decl(var_decl_t *vd);
code_seq gen_code_idents(ident_list_t *idents);
code_seq gen_code_stmts(stmts_t *stmts);
code_seq gen_code_stmt_list(stmt_list_t *stmt_list);
code_seq gen_code_stmt(stmt_t *stmt);
code_seq gen_code_assign_stmt(assign_stmt_t *stmt);
code_seq gen_code_call_stmt(call_stmt_t *stmt);
code_seq gen_code_if_stmt(if_stmt_t *stmt);
code_seq gen_code_while_stmt(while_stmt_t *stmt);
code_seq gen_code_read_stmt(read_stmt_t *stmt);
code_seq gen_code_print_stmt(print_stmt_t *stmt);
code_seq gen_code_block_stmt(block_stmt_t *stmt);
code_seq gen_code_condition(condition_t *cond);
code_seq gen_code_db_condition(db_condition_t *cond);
code_seq gen_code_rel_op_condition(rel_op_condition_t *cond);
code_seq gen_code_expr(expr_t *expr);
code_seq gen_code_binary_op_expr(binary_op_expr_t *expr);
code_seq gen_code_negated_expr(negated_expr_t *expr);
code_seq gen_code_ident(ident_t *id);
code_seq gen_code_number(int value);
code_seq gen_code_op(token_t op);
code_seq gen_code_arith_op(token_t op);
code_seq gen_code_rel_op(token_t op);

#endif // GEN_CODE_H
