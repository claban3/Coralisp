#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include <readline/readline.h>
#include <readline/history.h>

#include "prompt.h"

#define LASSERT(args, cond, fmt, ...) \
  if (!(cond)) { lisp_val* err = lisp_val_err(fmt, ##__VA_ARGS__); lisp_val_delete(args); return err; }

#define LASSERT_TYPE(func, args, index, expect) \
  LASSERT(args, args->cell[index]->type == expect, \
    "Function '%s' passed incorrect type for argument %i. Got %s, Expected %s.", \
    func, index, lisp_type_name(args->cell[index]->type), lisp_type_name(expect))

#define LASSERT_NUM(func, args, num) \
  LASSERT(args, args->count == num, \
    "Function '%s' passed incorrect number of arguments. Got %i, Expected %i.", \
    func, args->count, num)

#define LASSERT_NOT_EMPTY(func, args, index) \
  LASSERT(args, args->cell[index]->count != 0, \
    "Function '%s' passed {} for argument %i.", func, index);


lisp_env* lisp_env_new(void) {
  lisp_env* e = malloc(sizeof(lisp_env));
  e->parents = NULL;
  e->count = 0;
  e->syms = NULL;
  e->vals = NULL;
  return e;
}

lisp_env* lisp_env_copy(lisp_env* env) {
  lisp_env* copy = malloc(sizeof(lisp_env));
  copy->count = env->count;
  copy->parents = env->parents;
  copy->syms = malloc(sizeof(char*) * copy->count);
  copy->vals = malloc(sizeof(lisp_val*) * copy->count);
  for(int i = 0; i < copy->count; ++i) {
    copy->syms[i] = malloc(strlen(env->syms[i]) + 1);
    strcpy(copy->syms[i], env->syms[i]);
    copy->vals[i] = lisp_val_copy(env->vals[i]);
  }
  return copy;
}

lisp_env* lisp_env_def(lisp_env* e, lisp_val* sym, lisp_val* val) {
  while(e->parents) {
    e = e->parents;
  }
  lisp_env_put(e, sym, val);
  return e;
}

void lisp_env_delete(lisp_env* e) {
  for(int i = 0; i < e->count; ++i) {
    free(e->syms[i]);
    lisp_val_delete(e->vals[i]);
  }
  free(e->syms);
  free(e->vals);
  free(e);
}

// Change this to use a hashmap
lisp_val* lisp_env_get(lisp_env* e, lisp_val* v) {
  for(int i = 0; i < e->count; ++i) {
    if(strcmp(e->syms[i], v->sym) == 0) {
      return lisp_val_copy(e->vals[i]);
    }
  }
  if(e->parents) {
    return lisp_env_get(e->parents, v);
  }
  else {
    return lisp_val_err("unbound symbol");
  }
}


// Change this to use a hashmap
void lisp_env_put(lisp_env* e, lisp_val* symbol, lisp_val* value) {
  for(int i = 0; i < e->count ; ++i) {
    if(strcmp(e->syms[i], symbol->sym) == 0) {
      lisp_val_delete(e->vals[i]);
      e->vals[i] = lisp_val_copy(value);
      return;
    }
  }

  e->count++;
  e->syms = realloc(e->syms, sizeof(char*) * e->count);
  e->vals = realloc(e->vals, sizeof(lisp_val*) * e->count);

  e->syms[e->count - 1] = malloc(strlen(symbol->sym) + 1);
  e->vals[e->count - 1] = lisp_val_copy(value);
  strcpy(e->syms[e->count - 1], symbol->sym);
}

lisp_val* lisp_val_num_long(long x) {
  lisp_val* v = malloc(sizeof(lisp_val));
  v->type = LISP_VAL_NUM_LONG;
  v->num_long = x;
  return v;
}

lisp_val* lisp_val_num_double(double x) {
  lisp_val* v = malloc(sizeof(lisp_val));
  v->type = LISP_VAL_NUM_DOUBLE;
  v->num_double = x;
  return v;
}

lisp_val* lisp_val_err(char* error, ...) {
  lisp_val* v = malloc(sizeof(lisp_val));
  v->type = LISP_VAL_ERR;

  va_list va;
  va_start(va, error);

  v->err = malloc(512);

  vsnprintf(v->err, 511, error, va);
  
  v->err = realloc(v->err, strlen(v->err) + 1);
  va_end(va);
  return v;
}

lisp_val* lisp_val_sym(char* s) {
  lisp_val* v = malloc(sizeof(lisp_val));
  v->type = LISP_VAL_SYM;
  v->sym = malloc(strlen(s) + 1);
  strcpy(v->sym, s);
  return v;
}

lisp_val* lisp_val_sexpr(void) {
  lisp_val* v = malloc(sizeof(lisp_val));
  v->type = LISP_VAL_SEXPR;
  v->count = 0;
  v->cell = NULL;
  return v;
}

lisp_val* lisp_val_qexpr(void) {
  lisp_val* v = malloc(sizeof(lisp_val));
  v->type = LISP_VAL_QEXPR;
  v->count = 0;
  v->cell = NULL;
  return v;
}

lisp_val* lisp_val_func(lbuiltin func) {
  lisp_val* v = malloc(sizeof(lisp_val));
  v->type = LISP_VAL_FUNC;
  v->builtin_func = func;
  return v;
}

lisp_val* lisp_val_user_defined(lisp_val* formals, lisp_val* body) {
  lisp_val* v = malloc(sizeof(lisp_val));
  v->type = LISP_VAL_FUNC;
  v->builtin_func = NULL;

  v->env = lisp_env_new();

  v->formals = formals;
  v->body = body;
  return v;
}

void lisp_val_delete(lisp_val* v) {
  switch(v->type) {
    /* No dynamic memory for nums */
    case LISP_VAL_NUM_LONG: break;
    case LISP_VAL_NUM_DOUBLE: break;
    case LISP_VAL_FUNC: 
      if(!v->builtin_func) {
        lisp_val_delete(v->formals);
        lisp_val_delete(v->body);
        lisp_env_delete(v->env);
      }
    break;

    case LISP_VAL_SYM: free(v->sym); break;
    case LISP_VAL_ERR: free(v->err); break;

    case LISP_VAL_QEXPR:
    case LISP_VAL_SEXPR: 
      for(int i = 0; i < v->count; ++i) {
        lisp_val_delete(v->cell[i]);
      }
      free(v->cell);
    break;
  }

  free(v);
}

lisp_val* lisp_val_read_num(mpc_ast_t* t) {
  errno = 0;
    
  if(strchr(t->contents, '.') != NULL){
    double decimal = strtof(t->contents, NULL);
    //lisp_val_print(lisp_val_num_double(decimal));
    return errno != ERANGE ? lisp_val_num_double(decimal) : lisp_val_err("invalid number");
  } else {
    long integer = strtol(t->contents, NULL, 10);
    return errno != ERANGE ? lisp_val_num_long(integer) : lisp_val_err("invalid number");
  }
  
  return NULL;
}

lisp_val* lisp_val_read(mpc_ast_t* t) {
  if(strstr(t->tag, "number")) { return lisp_val_read_num(t); /* define this*/ }
  if(strstr(t->tag, "symbol")) { return lisp_val_sym(t->contents); }

  lisp_val* store = NULL;
  if(strstr(t->tag, ">")) { store = lisp_val_sexpr(); }
  if(strstr(t->tag, "sexpr")) { store = lisp_val_sexpr(); }
  if(strstr(t->tag, "qexpr")) { store = lisp_val_qexpr(); }

  for(int i = 0; i < t->children_num; ++i) {
    if(strcmp(t->children[i]->contents, "(") == 0) { continue; }
    if(strcmp(t->children[i]->contents, ")") == 0) { continue; }
    if(strcmp(t->children[i]->contents, "{") == 0) { continue; }
    if(strcmp(t->children[i]->contents, "}") == 0) { continue; }
    if(strcmp(t->children[i]->tag, "regex") == 0)  { continue; }
    store = lisp_val_add(store, lisp_val_read(t->children[i]));
  }

  return store;
}

lisp_val* lisp_val_call(lisp_env* e, lisp_val* func, lisp_val* params) {
  if(func->builtin_func) {return func->builtin_func(e, params);}

  int given = params->count;
  int total = func->formals->count;

  while(params->count) {
    if(func->formals->count == 0) {
      lisp_val_delete(params); return lisp_val_err(
        "Function passed too many arguments. "
        "Got %i, Expected %i", given, total);
    }
    lisp_val* sym = lisp_val_pop(func->formals, 0);

    if(strcmp(sym->sym, "&") == 0) {
      if(func->formals->count != 0) {
        lisp_val_delete(params);
        return lisp_val_err("Function format invalid. "
          "Symbol & not passed single symbol.");
      }

      lisp_val* mult_syms = lisp_val_pop(func->formals, 0);
      lisp_env_put(func->env, mult_syms, builtin_list(e, params));
      lisp_val_delete(sym); lisp_val_delete(mult_syms);
      break;
    }

    lisp_val* val = lisp_val_pop(params, 0);

    lisp_env_put(func->env, sym, val);
    lisp_val_delete(sym);
    lisp_val_delete(val);
  }

  lisp_val_delete(params);

  if(func->formals->count > 0 && 
    strcmp(func->formals->cell[0]->sym, "&") == 0) {
    if(func->formals->count != 2) {
      return lisp_val_err("Function format invalid. "
        "Symbol '&' not followed by single symbol.");
    }
    lisp_val_delete(lisp_val_pop(func->formals, 0));
    
    lisp_val* sym = lisp_val_pop(func->formals, 0);
    lisp_val* val = lisp_val_qexpr();

    lisp_env_put(func->env, sym, val);
    lisp_val_delete(sym); lisp_val_delete(val);

  }

  if(func->formals->count == 0) {
    func->env->parents = e;
    return builtin_eval(func->env, lisp_val_add(lisp_val_sexpr(),
       lisp_val_copy(func->body)));
  }
  else {
    return lisp_val_copy(func);
  }
}

lisp_val* lisp_val_add(lisp_val* v, lisp_val* to_add) {
  v->count++;
  v->cell = realloc(v->cell, sizeof(lisp_val*) * v->count);
  v->cell[v->count - 1] = to_add;
  return v;
}

lisp_val* lisp_val_copy(lisp_val* v) {
  lisp_val* copy = malloc(sizeof(lisp_val));
  copy->type = v->type;

  switch(v->type) {
    case LISP_VAL_FUNC: 
      if(v->builtin_func) {
        copy->builtin_func = v->builtin_func; 
      }
      else {
        copy->builtin_func = NULL;
        copy->formals = lisp_val_copy(v->formals);
        copy->body = lisp_val_copy(v->body);
        copy->env = lisp_env_copy(v->env);
      }
    break;
    case LISP_VAL_NUM_LONG: copy->num_long = v->num_long; break;
    case LISP_VAL_NUM_DOUBLE: copy->num_double = v->num_double; break;

    case LISP_VAL_ERR: 
      copy->err = malloc(strlen(v->err) + 1);
      strcpy(copy->err, v->err); break;
    case LISP_VAL_SYM:
      copy->sym = malloc(strlen(v->sym) + 1);
      strcpy(copy->sym, v->sym); break;

    case LISP_VAL_QEXPR:
    case LISP_VAL_SEXPR:
      copy->count = v->count;
      copy->cell = malloc(sizeof(lisp_val*) * copy->count);
      for(int i = 0; i < copy->count; ++i) {
        copy->cell[i] = lisp_val_copy(v->cell[i]);
      }
    break;
  }
  return copy;
}

lisp_val* lisp_val_eval_sexpr(lisp_env* e, lisp_val* v) {
  for(int i = 0; i < v->count; ++i) {
    v->cell[i] = lisp_val_eval(e, v->cell[i]);
  }

  for(int i = 0; i < v->count; ++i) {
    if(v->cell[i]->type == LISP_VAL_ERR) {
      return lisp_val_take(v, i);
    }
  }

  if(v->count == 0) {
    return v;
  }

  if(v->count == 1) {
    return lisp_val_take(v, 0);
  }

  lisp_val* front = lisp_val_pop(v, 0);
  if(front->type != LISP_VAL_FUNC) {
    lisp_val* err = lisp_val_err(
      "S-Expression starts with incorrect type, "
      "Got %s, Expected %s",
      lisp_type_name(front->type), lisp_type_name(LISP_VAL_FUNC));
    lisp_val_delete(front); lisp_val_delete(v);
    return err;
  }

  lisp_val* result = lisp_val_call(e, front, v);

  //lisp_val* result = builtin(v, front->sym);
  lisp_val_delete(front);
  return result;
}

lisp_val* lisp_val_eval(lisp_env* e, lisp_val* v) {
  
  if(v->type == LISP_VAL_SYM) {
    lisp_val* store = lisp_env_get(e, v);
    lisp_val_delete(v);
    return store;
  }

  if(v->type == LISP_VAL_SEXPR) { return lisp_val_eval_sexpr(e, v);}
  return v;
}

lisp_val* lisp_val_pop(lisp_val* v, int i) {
  lisp_val* store = v->cell[i];
  memmove(&v->cell[i], &v->cell[i + 1], sizeof(lisp_val*) * (v->count - i - 1));
  v->count--;
  v->cell = realloc(v->cell, sizeof(lisp_val*) * v->count);
  return store;
}

lisp_val* lisp_val_take(lisp_val* v, int i) {
  lisp_val* store = lisp_val_pop(v, i);
  lisp_val_delete(v);
  return store;
}

lisp_val* lisp_val_join(lisp_val* to, lisp_val* from) {

  while(from->count > 0) {
    to = lisp_val_add(to, lisp_val_pop(from, 0));
  }
  lisp_val_delete(from);
  return to;
}

lisp_val* builtin_op(lisp_env* e, lisp_val* v, char* op) {
  for(int i = 0; i < v->count; ++i) {
    if(v->cell[i]->type != LISP_VAL_NUM_LONG && v->cell[i]->type != LISP_VAL_NUM_DOUBLE) {
      return lisp_val_err("Operation %s cannot operate on non numerical values, "
          "Got %s, Expected %s or %s",
          op, lisp_type_name(v->cell[i]->type), lisp_type_name(LISP_VAL_NUM_LONG), 
          lisp_type_name(LISP_VAL_NUM_DOUBLE));
    }
  }

  lisp_val* front = lisp_val_pop(v, 0);

  if(v->count == 1 && strcmp(op, "-")) {
    if(v->type == LISP_VAL_NUM_DOUBLE) {
      v->num_double = -v->num_double;
    } 
    else{
      v->num_long = -v->num_long;
    }
  }

  while(v->count > 0) {
    lisp_val* next = lisp_val_pop(v, 0);

    if(front->type == LISP_VAL_NUM_LONG && next->type == LISP_VAL_NUM_LONG) {

    if(strcmp(op, "+") == 0) {front->num_long += next->num_long;}
    if(strcmp(op, "-") == 0) {front->num_long -= next->num_long;}
    if(strcmp(op, "*") == 0) {front->num_long *= next->num_long;}
    if(strcmp(op, "/") == 0) {
      if(next->num_long == 0){
        lisp_val_delete(front);
        lisp_val_delete(next);
        front = lisp_val_err("divide by zero error");
        break;
      } else {
        front->num_long /= next->num_long;
      }
    }
    if(strcmp(op, "%") == 0) {front->num_long = (front->num_long % next->num_long);}
    if(strcmp(op, "^") == 0) {front->num_long = powl(front->num_long, next->num_long);}

    if(strcmp(op, "min") == 0) {if(front->num_long > next->num_long){ front->num_long = next->num_long; }}
    if(strcmp(op, "max") == 0) {if(front->num_long < next->num_long){ front->num_long = next->num_long; }}

    } else {
      if (front->type == LISP_VAL_NUM_LONG) { double x_num_double = front->num_long; front->num_double = x_num_double; }
      if (next->type == LISP_VAL_NUM_LONG) { double y_num_double = next->num_long; next->num_double = y_num_double; }

      if(strcmp(op, "+") == 0) {front->num_double += next->num_double;}
      if(strcmp(op, "-") == 0) {front->num_double -= next->num_double;}
      if(strcmp(op, "*") == 0) {front->num_double *= next->num_double;}
      if(strcmp(op, "/") == 0) {
        if(next->num_double == 0){
          lisp_val_delete(front);
          lisp_val_delete(next);
          front = lisp_val_err("divide by zero error");
          break;
        } else {
        front->num_double /= next->num_double;
        }
      }
      if(strcmp(op, "%") == 0) {
        lisp_val_delete(front);
        lisp_val_delete(next);
        front = lisp_val_err("Cannot perform modulo on doubles");
        break;
      }
      if(strcmp(op, "^") == 0) {front->num_double = powl(front->num_double, next->num_double);}

      if(strcmp(op, "min") == 0) {if(front->num_double > next->num_double){ front->num_double = next->num_double; }}
      if(strcmp(op, "max") == 0) {if(front->num_double < next->num_double){ front->num_double = next->num_double; }}
    }
    lisp_val_delete(next);
  }

  lisp_val_delete(v);

  return front;
}

lisp_val* builtin_add(lisp_env* e, lisp_val* v) {
  return builtin_op(e, v, "+");
}

lisp_val* builtin_sub(lisp_env* e, lisp_val* v) {
  return builtin_op(e, v, "-");
}

lisp_val* builtin_mult(lisp_env* e, lisp_val* v) {
  return builtin_op(e, v, "*");
}

lisp_val* builtin_div(lisp_env* e, lisp_val* v) {
  return builtin_op(e, v, "/");
}

lisp_val* builtin_pow(lisp_env* e, lisp_val* v) {
  return builtin_op(e, v, "^");
}

lisp_val* builtin_mod(lisp_env* e, lisp_val* v) {
  return builtin_op(e, v, "%");
}

lisp_val* builtin_head(lisp_env* e, lisp_val* q) {
  LASSERT_NUM("head", q, 1);
  LASSERT_TYPE("head", q, 0, LISP_VAL_QEXPR);
  LASSERT_NOT_EMPTY("head", q, 0);

  lisp_val* head = lisp_val_take(q, 0);
  while(head->count > 1) {
    lisp_val_delete(lisp_val_pop(head, 1));
  }
  return head; 
}

lisp_val* builtin_tail(lisp_env* e, lisp_val* q) {
  LASSERT_NUM("tail", q, 1);
  LASSERT_TYPE("tail", q, 0, LISP_VAL_QEXPR);
  LASSERT_NOT_EMPTY("tail", q, 0);

  lisp_val* head = lisp_val_take(q, 0);
  lisp_val_delete(lisp_val_pop(head, 0));
  return head;
}

lisp_val* builtin_list(lisp_env* e, lisp_val* q) {
  q->type = LISP_VAL_QEXPR;
  return q;
}

lisp_val* builtin_eval(lisp_env* e, lisp_val* q) {
  LASSERT_TYPE("eval", q, 0, LISP_VAL_QEXPR);
  LASSERT_NUM("eval", q, 1);

  lisp_val* list = lisp_val_take(q, 0);
  list->type = LISP_VAL_SEXPR;
  return lisp_val_eval(e, list);
}

lisp_val* builtin_join(lisp_env* e, lisp_val* q) {
  for(int i = 0; i < q->count; ++i) {
    LASSERT_TYPE("join", q, i, LISP_VAL_QEXPR);
  }

  lisp_val* join = lisp_val_pop(q, 0);

  while(q->count > 0) {
    join = lisp_val_join(join, lisp_val_pop(q, 0));
  }

  lisp_val_delete(q);
  return join;
}

lisp_val* builtin_cons(lisp_env* e, lisp_val* q) {
  LASSERT_NUM("cons", q, 2);

  lisp_val* elt = lisp_val_pop(q, 0);
  lisp_val* qexpr = lisp_val_take(q, 0);
  lisp_val* cons = lisp_val_qexpr();

  lisp_val_add(cons, elt);

  while(qexpr->count) {
    lisp_val_add(cons, lisp_val_pop(qexpr, 0));
  }

  lisp_val_delete(qexpr);

  return cons;
}

lisp_val* builtin_len(lisp_env* e, lisp_val* q) {
  LASSERT_TYPE("len", q, 0, LISP_VAL_QEXPR);
  LASSERT_NUM("len", q, 1);
  
  int count = 0;
  lisp_val* list = lisp_val_take(q, 0);
  while(list->count) {
    lisp_val_delete(lisp_val_pop(list, 0));
    ++count;
  }

  return lisp_val_num_long(count);
}

lisp_val* builtin_init(lisp_env* e, lisp_val* q) {
  LASSERT_TYPE("init", q, 0, LISP_VAL_QEXPR);
  LASSERT_NUM("init", q, 1);
  
  lisp_val* list = lisp_val_take(q, 0);
  if(list->count < 1) {
    return list;
  }
  lisp_val_delete(lisp_val_pop(list, list->count - 1));

  return list;
}

lisp_val* builtin_var(lisp_env* e, lisp_val* v, char* func) {
  LASSERT_TYPE(func, v, 0, LISP_VAL_QEXPR);

  lisp_val* syms = v->cell[0];

  for(int i = 0; i < syms->count; ++i) {
    LASSERT(v, syms->cell[i]->type == LISP_VAL_SYM,
      "Function 'def' cannot define non-symbol, "
      "Got %s, Expected %s",
      lisp_type_name(syms->cell[i]->type), lisp_type_name(LISP_VAL_SYM));
  }

  LASSERT(v, syms->count == v->count - 1, 
    "Function 'def' passed incorrect number of values to symbols, "
    "Got %s, Expected %s",
    syms->count, v->count - 1);

  for(int i = 0; i < syms->count; ++i) {
    if(strcmp(func, "def") == 0) {
      lisp_env_def(e, syms->cell[i], v->cell[i+1]);  
    }
    if(strcmp(func, "=") == 0) {
      lisp_env_put(e, syms->cell[i], v->cell[i+1]);
    }
  }

  lisp_val_delete(v);
  return lisp_val_sexpr();
}

lisp_val* builtin_put(lisp_env* e, lisp_val* v) {
  return builtin_var(e, v, "=");
}

lisp_val* builtin_def(lisp_env* e, lisp_val* v) {
  return builtin_var(e, v, "def");
}

lisp_val* builtin_lambda(lisp_env* e, lisp_val* v) {
  LASSERT_NUM("\\", v, 2);
  LASSERT_TYPE("\\", v, 0, LISP_VAL_QEXPR);
  LASSERT_TYPE("\\", v, 1, LISP_VAL_QEXPR);

  for(int i = 0; i < v->count; ++i) {
    LASSERT(v, v->cell[0]->cell[i]->type == LISP_VAL_SYM, 
      "Cannot define a non-symbol. "
      "Got %s, Expected %s.", lisp_type_name(v->cell[0]->cell[i]->type), 
      lisp_type_name(LISP_VAL_SYM));
  }

  lisp_val* formals = lisp_val_pop(v, 0);
  lisp_val* body = lisp_val_pop(v, 0);
  lisp_val_delete(v);

  return lisp_val_user_defined(formals, body);
}

//lisp_val* builtin_fun(lisp_env* e, lisp_val* v) {
//}

lisp_val* builtin_exit(lisp_env* e, lisp_val* v) {
  lisp_val_delete(v); lisp_env_delete(e);
  exit(0);
}

void lisp_env_add_builtin(lisp_env* e, char* name, lbuiltin func) {
  lisp_val* func_name = lisp_val_sym(name);
  lisp_val* func_value = lisp_val_func(func);
  lisp_env_put(e, func_name, func_value);
  lisp_val_delete(func_name); lisp_val_delete(func_value);
}

void lisp_env_add_builtins(lisp_env* e) {
  lisp_env_add_builtin(e, "list", builtin_list);
  lisp_env_add_builtin(e, "tail", builtin_tail);
  lisp_env_add_builtin(e, "join", builtin_join);
  lisp_env_add_builtin(e, "init", builtin_init);
  lisp_env_add_builtin(e, "cons", builtin_cons);
  lisp_env_add_builtin(e, "eval", builtin_eval);
  lisp_env_add_builtin(e, "len", builtin_len);
  lisp_env_add_builtin(e, "head", builtin_head);
  lisp_env_add_builtin(e, "def", builtin_def);
  lisp_env_add_builtin(e, "put", builtin_put);
  lisp_env_add_builtin(e, "\\", builtin_lambda);
  lisp_env_add_builtin(e, "exit", builtin_exit);
  lisp_env_add_builtin(e, "+", builtin_add);
  lisp_env_add_builtin(e, "-", builtin_sub);
  lisp_env_add_builtin(e, "/", builtin_div);
  lisp_env_add_builtin(e, "*", builtin_mult);
  lisp_env_add_builtin(e, "%", builtin_mod);
  lisp_env_add_builtin(e, "^", builtin_pow);
}

void lisp_val_expr_print(lisp_val* v, char start, char end) {
  putchar(start);
  for(int i = 0; i < v->count; ++i) {
    lisp_val_print(v->cell[i]);
    if(i != (v->count - 1)) {
      putchar(' ');
    }
  }
  putchar(end);
}

void lisp_val_print(lisp_val* v) {
  switch(v->type) {
    case LISP_VAL_NUM_LONG:   printf("%li", v->num_long); break;
    case LISP_VAL_NUM_DOUBLE: printf("%f", v->num_double); break;
    case LISP_VAL_ERR:        printf("Error: %s", v->err); break;
    case LISP_VAL_SYM:        printf("%s", v->sym); break;
    case LISP_VAL_SEXPR:      lisp_val_expr_print(v, '(', ')'); break;
    case LISP_VAL_QEXPR:      lisp_val_expr_print(v, '{', '}'); break;
    case LISP_VAL_FUNC:      
      if(v->builtin_func) {
        printf("<builtin function>");
      }
      else {
        printf("(\\ "); lisp_val_print(v->formals);
        putchar(' '); lisp_val_print(v->body); putchar(')');
      }
    break;
  }
}

void lisp_val_println(lisp_val* v) {
  lisp_val_print(v);
  putchar('\n');
}

char* lisp_type_name(int t) {
  switch(t) {
    case LISP_VAL_FUNC: return "Function";
    case LISP_VAL_NUM_LONG: return "Integer";
    case LISP_VAL_NUM_DOUBLE: return "Double";
    case LISP_VAL_ERR: return "Error";
    case LISP_VAL_SYM: return "Symbol";
    case LISP_VAL_SEXPR: return "S-Expression";
    case LISP_VAL_QEXPR: return "Q-Expression";
    default: return "Unknown";
  }
}


int main(int argc, char** argv){
  mpc_parser_t* Number   = mpc_new("number");
  mpc_parser_t* Symbol   = mpc_new("symbol");
  mpc_parser_t* Sexpr    = mpc_new("sexpr");
  mpc_parser_t* Qexpr    = mpc_new("qexpr");
  mpc_parser_t* Expr     = mpc_new("expr");
  mpc_parser_t* Lispy    = mpc_new("lispy");

  mpca_lang(MPCA_LANG_DEFAULT,
    "                                                                 \
    number   : /-?[0-9]+(\\.[0-9]*)?/ ;                               \
    symbol   : /[a-zA-Z0-9_+\\-*\\/\\\\=<>!&]+/ ;                     \
    sexpr    : '(' <expr>* ')' ;                                      \
    qexpr    : '{' <expr>* '}' ;                                      \
    expr     : <number> | <symbol> | <sexpr> | <qexpr> ;              \
    lispy    : /^/ <expr>* /$/ ;                                      \
    ", 
    Number, Symbol, Sexpr, Qexpr, Expr, Lispy);

  puts("Coralisp Editor v0.0.0.0.1");
  puts("Press Ctrl+c to Exit\n");

  lisp_env* env = lisp_env_new();
  lisp_env_add_builtins(env);

  while(1) { 
    char *input = readline("coralisp>>> ");
    add_history(input);

    mpc_result_t res;
    if(mpc_parse("<stdin>", input, Lispy, &res)) {
      //mpc_ast_print(res.output);
      lisp_val* x = lisp_val_eval(env, lisp_val_read(res.output));
      lisp_val_println(x);
      lisp_val_delete(x);

      mpc_ast_delete(res.output);
    } else {
      mpc_err_print(res.error);
      mpc_err_print(res.error);
    }

    free(input);
  }
  lisp_env_delete(env);

  mpc_cleanup(6, Number, Symbol, Sexpr, Qexpr, Expr, Lispy);
  return 0;
}