#include "typedef.h"
#include "mpc.h"


lisp_env* lisp_env_new(void);
lisp_val* lisp_env_get(lisp_env* e, lisp_val* v);
lisp_env* lisp_env_copy(lisp_env* env);
lisp_env* lisp_env_def(lisp_env* e, lisp_val* sym, lisp_val* val);
void lisp_env_delete(lisp_env* e);
void lisp_env_put(lisp_env* e, lisp_val* symbol, lisp_val* value);
lisp_val* lisp_val_num_long(long x);
lisp_val* lisp_val_num_double(double x) ;
lisp_val* lisp_val_err(char* error, ...);
lisp_val* lisp_val_sym(char* s);
lisp_val* lisp_val_sexpr(void);
lisp_val* lisp_val_qexpr(void);
lisp_val* lisp_val_func(lbuiltin func);
void lisp_val_delete(lisp_val* v);
lisp_val* lisp_val_call(lisp_env* e, lisp_val* func, lisp_val* params);
lisp_val* lisp_val_read_num(mpc_ast_t* t);
lisp_val* lisp_val_read(mpc_ast_t* t);
lisp_val* lisp_val_add(lisp_val* v, lisp_val* to_add);
lisp_val* lisp_val_copy(lisp_val* v);
lisp_val* lisp_val_eval_sexpr(lisp_env* e, lisp_val* v);
lisp_val* lisp_val_eval(lisp_env* e, lisp_val* v);
lisp_val* lisp_val_pop(lisp_val* v, int i);
lisp_val* lisp_val_take(lisp_val* v, int i);
lisp_val* lisp_val_join(lisp_val* to, lisp_val* from);
//lisp_val* builtin(lisp_val* v, char* func);
lisp_val* builtin_op(lisp_env* e, lisp_val* v, char* op);  
lisp_val* builtin_add(lisp_env* e, lisp_val* v);
lisp_val* builtin_sub(lisp_env* e, lisp_val* v);
lisp_val* builtin_mult(lisp_env* e, lisp_val* v);
lisp_val* builtin_div(lisp_env* e, lisp_val* v);
lisp_val* builtin_pow(lisp_env* e, lisp_val* v);
lisp_val* builtin_mod(lisp_env* e, lisp_val* v);
lisp_val* builtin_head(lisp_env* e, lisp_val* q);
lisp_val* builtin_tail(lisp_env* e, lisp_val* q);
lisp_val* builtin_list(lisp_env* e, lisp_val* q);
lisp_val* builtin_eval(lisp_env* e, lisp_val* q);
lisp_val* builtin_join(lisp_env* e, lisp_val* q);
lisp_val* builtin_cons(lisp_env* e, lisp_val* q);
lisp_val* builtin_len(lisp_env* e, lisp_val* q);
lisp_val* builtin_init(lisp_env* e, lisp_val* q);
lisp_val* builtin_var(lisp_env* e, lisp_val* v, char* func);
lisp_val* builtin_put(lisp_env* e, lisp_val* v);
lisp_val* builtin_def(lisp_env* e, lisp_val* v);
lisp_val* builtin_lambda(lisp_env* e, lisp_val* v);		
lisp_val* builtin_exit(lisp_env* e, lisp_val*v);
void lisp_env_add_builtin(lisp_env* e, char* name, lbuiltin func);
void lisp_env_add_builtins(lisp_env* e);
void lisp_val_expr_print(lisp_val* v, char start, char end);
void lisp_val_print(lisp_val* v);
void lisp_val_println(lisp_val* v);
char* lisp_type_name(int t); 

