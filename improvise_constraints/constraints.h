#ifndef CONSTRAINTS_H
#define CONSTRAINTS_H

typedef struct variable_rep variable;

// initialize a variable, passing in an initial value.
void variable_init(variable* to_init, int initial_value);

// change a variable
void variable_set(variable* to_mutate, int new_value);

// access a variable's value
int variable_get(variable* to_access);

typedef struct change_set_rep change_set;

// init a change set to the empty set
void change_set_init(change_set* to_init);

// add a variable to a change set.
void change_set_add(change_set* to_mutate, variable* to_add);

// check if a variable is in a change set.
int change_set_contains(change_set* to_access, variable* to_check);

typedef void (*exp_run_fun_type)(void*, change_set*);

typedef struct exp_rep exp;

// Used in the implementation of constraint_run.
void exp_run(exp*, change_set*);

typedef struct plus_exp_rep plus_exp;

// initialize a plus_exp, passing in left and right sides.
// TODO(johnicholas.hines@gmail.com): Replace vars with exps.
void plus_exp_init(plus_exp*, variable*, variable*);

// Implements exp_run from exp.
void plus_exp_run(void*, change_set*);

typedef struct constraint_rep constraint;

// initialize a constraint given the exp that it should keep zero
void constraint_init(constraint* to_init, exp* exp_to_keep_zero);

// Used in the implementation of constraint_set_run
void constraint_run(constraint*, change_set*);

typedef struct constraint_set_rep constraint_set;

// init a constraint set to the empty set.
void constraint_set_init(constraint_set* to_init);

// add a constraint to a constraint set.
void constraint_set_add(constraint_set* to_mutate, constraint* to_add);

typedef struct repair_plan_rep repair_plan;

// init a repair plan
void repair_plan_init(repair_plan* to_init,
		      constraint_set* to_enforce,
		      change_set* the_vars_that_will_change);

// propagate the changed vars to restore the constraints
void repair_plan_run(repair_plan*);



// PRIVATE

struct variable_rep {
  int current_value;
};

struct change_set_rep {
  variable* my_first_variable;
};

struct exp_rep {
  exp_run_fun_type exp_run_fun;
};

struct plus_exp_rep {
  exp super;
  variable* left_hand_side;
  variable* right_hand_side;
};

struct constraint_rep {
  exp* my_exp;
};

struct constraint_set_rep {
  constraint* my_first_constraint;
};

struct repair_plan_rep {
  constraint_set* to_enforce;
  change_set* changing_vars;
};

#endif // CONSTRAINTS_H
