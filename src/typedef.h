

struct lisp_val;
struct lisp_env;
typedef struct lisp_val lisp_val;
typedef struct lisp_env lisp_env;

typedef lisp_val* (*lbuiltin) (lisp_env*, lisp_val*);

struct lisp_val {
	int type;

	union {
		long num_long;
		double num_double;
		char* err;
		char* sym;
		lbuiltin builtin_func;
	};
	
	lisp_env* env;
	lisp_val* formals;
	lisp_val* body;

	int count;
	struct lisp_val** cell;
};

struct lisp_env {
	int count;
	char** syms;
	lisp_val** vals;
	lisp_env* parents;
};

enum { 
	LISP_VAL_NUM_LONG, 
	LISP_VAL_NUM_DOUBLE,
	LISP_VAL_SEXPR, 
	LISP_VAL_QEXPR,
	LISP_VAL_SYM, 
	LISP_VAL_ERR,
	LISP_VAL_FUNC
};

//enum { LERR_DIV_ZERO, LERR_BAD_OP, LERR_BAD_NUM};