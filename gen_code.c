#include "gen_code.h"
#include "ast.h" // For block_t, stmt_t, expr_t, etc.
#include "code_seq.h" // For code_seq and related operations
#include "bof.h" // For BOFHeader and related functions
#include "literal_table.h" // For managing literals
#include "instruction.h" // For instruction-related functions
#include "regname.h" // For register names
#include "utilities.h" // For utility functions
#include <string.h> // For memcpy

#define STACK_SPACE 4096

// Initialize the code generator
void gen_code_initialize() {
    literal_table_initialize();
}

// Generate the BOF program header
BOFHeader gen_code_program_header(code_seq main_cs) {
    BOFHeader ret;
    memcpy(ret.magic, "BO32", 4); // Magic number for BOF files
    ret.text_start_address = 0;
    ret.text_length = code_seq_size(main_cs) * sizeof(bin_instr_t);
    ret.data_start_address = (ret.text_length + 1023) & ~1023; // Align to 1024
    ret.data_length = literal_table_size() * sizeof(word_type);
    ret.stack_bottom_addr = ret.data_start_address + ret.data_length + STACK_SPACE;
    return ret;
}

// Output the program to a BOFFILE
void gen_code_output_program(BOFFILE bf, code_seq main_cs) {
    BOFHeader header = gen_code_program_header(main_cs);
    bof_write_header(bf, header);
    gen_code_output_seq(bf, main_cs);
    gen_code_output_literals(bf);
}

// Output code sequences
void gen_code_output_seq(BOFFILE bf, code_seq cs) {
    while (!code_seq_is_empty(cs)) {
        bin_instr_t instr = code_seq_first(cs)->instr;
        instruction_write_bin_instr(bf, instr);
        cs = code_seq_rest(cs);
    }
}

// Output literals
void gen_code_output_literals(BOFFILE bf) {
    literal_table_start_iteration();
    while (literal_table_iteration_has_next()) {
        word_type literal = literal_table_iteration_next();
        bof_write_word(bf, literal);
    }
    literal_table_end_iteration();
}

// Generate the entire program
void gen_code_program(BOFFILE bf, block_t *prog) {
    code_seq main_cs = gen_code_block(prog);
    gen_code_output_program(bf, main_cs);
}

// Generate code for a block
code_seq gen_code_block(block_t *block) {
    code_seq ret = code_seq_empty();
    code_seq_concat(&ret, gen_code_const_decls(block->const_decls));
    code_seq_concat(&ret, gen_code_var_decls(block->var_decls));
    code_seq_concat(&ret, gen_code_stmts(&(block->stmts)));
    return ret;
}

// Generate code for constant declarations
code_seq gen_code_const_decls(const_decls_t const_decls) {
    code_seq ret = code_seq_empty();
    const_decl_t *cd = const_decls.start;
    while (cd != NULL) {
        code_seq_concat(&ret, gen_code_const_decl(cd));
        cd = cd->next;
    }
    return ret;
}

// Generate code for a single constant declaration
code_seq gen_code_const_decl(const_decl_t *const_decl) {
    code_seq ret = code_seq_empty();
    const_def_t *def = const_decl->const_def_list.start;
    while (def != NULL) {
        unsigned int offset = literal_table_lookup(def->number.text, def->number.value);
        code_seq_add_to_end(&ret, code_lit(GP, offset, def->number.value));
        def = def->next;
    }
    return ret;
}

// Generate code for variable declarations
code_seq gen_code_var_decls(var_decls_t var_decls) {
    code_seq ret = code_seq_empty();
    var_decl_t *vd = var_decls.var_decls;
    while (vd != NULL) {
        code_seq_concat(&ret, gen_code_var_decl(vd));
        vd = vd->next;
    }
    return ret;
}

// Generate code for a single variable declaration
code_seq gen_code_var_decl(var_decl_t *vd) {
    return gen_code_idents(&(vd->ident_list));
}

// Generate code for the identifiers in idents
code_seq gen_code_idents(ident_list_t *idents) {
    code_seq ret = code_seq_empty();
    ident_t *idp = idents->start;
    while (idp != NULL) {
        code_seq alloc_and_init = code_seq_singleton(code_sri(SP, 1));
        code_seq_add_to_end(&alloc_and_init, code_lit(SP, 0, 0));
        code_seq_concat(&ret, alloc_and_init);
        idp = idp->next;
    }
    return ret;
}

// Generate code for a list of statements
code_seq gen_code_stmts(stmts_t *stmts) {
    code_seq ret = code_seq_empty();
    stmt_t *sp = stmts->stmt_list.start;
    while (sp != NULL) {
        code_seq_concat(&ret, gen_code_stmt(sp));
        sp = sp->next;
    }
    return ret;
}

// Generate code for a single statement
code_seq gen_code_stmt(stmt_t *stmt) {
    switch (stmt->stmt_kind) {
        case assign_stmt:
            return gen_code_assign_stmt(&(stmt->data.assign_stmt));
        case call_stmt:
            return gen_code_call_stmt(&(stmt->data.call_stmt));
        case if_stmt:
            return gen_code_if_stmt(&(stmt->data.if_stmt));
        case while_stmt:
            return gen_code_while_stmt(&(stmt->data.while_stmt));
        case read_stmt:
            return gen_code_read_stmt(&(stmt->data.read_stmt));
        case print_stmt:
            return gen_code_print_stmt(&(stmt->data.print_stmt));
        case block_stmt:
            return gen_code_block_stmt(&(stmt->data.block_stmt));
        default:
            bail_with_error("Unknown stmt_kind (%d) in gen_code_stmt!", stmt->stmt_kind);
    }
    return code_seq_empty(); // should never reach here
}


// Generate code for an assignment statement
code_seq gen_code_assign_stmt(assign_stmt_t *stmt) {
    code_seq ret = gen_code_expr(stmt->expr);
    code_seq_concat(&ret, code_seq_singleton(code_lwr(3, SP, 0))); // Pop stack into $3
    code_seq fp_seq = code_utils_compute_fp(4, stmt->idu->levelsOutward); // Use $4 for T9
    code_seq_concat(&ret, fp_seq);
    unsigned int offset_count = id_use_get_attrs(stmt->idu)->offset_count;
    code_seq_add_to_end(&ret, code_swr(4, offset_count, 3)); // Use $4 for T9 and $3 for V0
    return ret;
}

// Generate code for a call statement
code_seq gen_code_call_stmt(call_stmt_t *stmt) {
    bail_with_error("Call statements are not supported in this version.");
    return code_seq_empty();
}

// Generate code for an if statement
code_seq gen_code_if_stmt(if_stmt_t *stmt) {
    code_seq ret = gen_code_condition(&(stmt->condition));
    code_seq then_cs = gen_code_stmts(&(stmt->then_stmts));
    code_seq else_cs = stmt->else_stmts ? gen_code_stmts(stmt->else_stmts) : code_seq_empty();
    unsigned int then_len = code_seq_size(then_cs);
    unsigned int else_len = code_seq_size(else_cs);
    code_seq_concat(&ret, code_seq_singleton(code_lwr(3, SP, 0)));
    code_seq_add_to_end(&ret, code_beq(3, 0, then_len + 1));
    code_seq_concat(&ret, then_cs);
    code_seq_add_to_end(&ret, code_jrel(else_len));
    code_seq_concat(&ret, else_cs);
    return ret;
}

// Generate code for a while statement
code_seq gen_code_while_stmt(while_stmt_t *stmt) {
    code_seq ret = gen_code_condition(&(stmt->condition));
    code_seq body_cs = gen_code_stmts(&(stmt->body));
    unsigned int body_len = code_seq_size(body_cs);
    code_seq_concat(&ret, code_seq_singleton(code_lwr(3, SP, 0)));
    code_seq_add_to_end(&ret, code_beq(3, 0, body_len + 1));
    code_seq_concat(&ret, body_cs);
    code_seq_add_to_end(&ret, code_jrel(-(body_len + code_seq_size(ret) + 1)));
    return ret;
}

// Generate code for a read statement
code_seq gen_code_read_stmt(read_stmt_t *stmt) {
    code_seq ret = code_seq_singleton(code_rch(3, 0)); // Use $3 for V0
    code_seq fp_seq = code_utils_compute_fp(4, stmt->idu->levelsOutward); // Use $4 for T9
    code_seq_concat(&ret, fp_seq);
    unsigned int offset_count = id_use_get_attrs(stmt->idu)->offset_count;
    code_seq_add_to_end(&ret, code_swr(4, offset_count, 3)); // Use $4 for T9 and $3 for V0
    return ret;
}


// Generate code for a print statement
code_seq gen_code_print_stmt(print_stmt_t *stmt) {
    code_seq ret = gen_code_expr(&(stmt->expr));
    code_seq_concat(&ret, code_seq_singleton(code_lwr(5, SP, 0))); // Pop stack into $5
    code_seq_add_to_end(&ret, code_pint(5, 0)); // Use $5 for A0
    return ret;
}

// Generate code for a block statement
code_seq gen_code_block_stmt(block_stmt_t *stmt) {
    return gen_code_block(stmt->block);
}


// Generate code for expressions
code_seq gen_code_expr(expr_t *expr) {
    switch (expr->expr_kind) {
        case expr_bin:
            return gen_code_binary_op_expr(&(expr->data.binary));
        case expr_ident:
            return gen_code_ident(&(expr->data.ident));
        case expr_number:
            return gen_code_number(expr->data.number.value);
        case expr_negated:
            return gen_code_negated_expr(&(expr->data.negated));
        default:
            bail_with_error("Unexpected expr_kind_e (%d)", expr->expr_kind);
    }
    return code_seq_empty(); // unreachable
}

// Generate binary operation expressions
code_seq gen_code_binary_op_expr(binary_op_expr_t *expr) {
    code_seq left_cs = gen_code_expr(expr->expr1);
    code_seq right_cs = gen_code_expr(expr->expr2);
    code_seq_concat(&left_cs, right_cs);
    code_seq op_cs = gen_code_op(expr->arith_op);
    code_seq_concat(&left_cs, op_cs);
    return left_cs;
}

// Generate code for an operator
code_seq gen_code_op(token_t op) {
    switch (op.code) {
        case plussym:
            return code_seq_singleton(code_add(3, 0, 3, 0, 6, 0)); // Use $3 for V0 and $6 for AT
        case minussym:
            return code_seq_singleton(code_sub(3, 0, 3, 0, 6, 0)); // Use $3 for V0 and $6 for AT
        case multsym:
            return code_seq_singleton(code_mul(3, 0, 6, 0)); // Use $3 for V0 and $6 for AT
        case divsym:
            return code_seq_singleton(code_div(3, 0, 6, 0)); // Use $3 for V0 and $6 for AT
        default:
            bail_with_error("Unknown token code (%d) in gen_code_op", op.code);
    }
    return code_seq_empty(); // should never reach here
}

// Generate negated expressions
code_seq gen_code_negated_expr(negated_expr_t *expr) {
    code_seq ret = gen_code_expr(expr->expr);
    code_seq_concat(&ret, code_seq_singleton(code_neg(6, 0, 6, 0))); // Negate $6
    return ret;
}

// Generate code for identifiers
code_seq gen_code_ident(ident_t *id) {
    code_seq ret = code_utils_compute_fp(4, id->idu->levelsOutward);
    unsigned int offset_count = id_use_get_attrs(id->idu)->offset_count;
    code_seq_add_to_end(&ret, code_lwr(3, 4, offset_count));
    code_seq_concat(&ret, code_seq_singleton(code_swr(SP, 0, 3)));
    return ret;
}

// Generate code for number literals
code_seq gen_code_number(int value) {
    unsigned int offset = literal_table_lookup("", value);
    code_seq ret = code_seq_singleton(code_lit(3, 0, offset));
    code_seq_concat(&ret, code_seq_singleton(code_swr(SP, 0, 3)));
    return ret;
}
