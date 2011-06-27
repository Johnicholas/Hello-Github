#include "constraints.h"
#include <assert.h>

void variable_init(variable* to_init, int initial_value) {
  to_init->current_value= initial_value;
}

void variable_set(variable* to_mutate, int new_value) {
  to_mutate->current_value= new_value;
}

int variable_get(variable* to_access) {
  return to_access->current_value;
}

void plus_exp_run(void* to_run_uncast, change_set* changed) {
  plus_exp* to_run= (plus_exp*)to_run_uncast;
  if (change_set_contains(changed, to_run->left_hand_side) &&
      change_set_contains(changed, to_run->right_hand_side)) {
    // TODO(johnicholas.hines@gmail.com): Compare stay strengths maybe?
    assert(variable_get(to_run->left_hand_side)
	   ==
	   variable_get(to_run->right_hand_side));
  } else if (change_set_contains(changed, to_run->left_hand_side)) {
    variable_set(to_run->right_hand_side, variable_get(to_run->left_hand_side));
  } else if (change_set_contains(changed, to_run->right_hand_side)) {
    variable_set(to_run->left_hand_side, variable_get(to_run->right_hand_side));
  } else {
    // nothing to do, neither side has changed
  }
}

void plus_exp_init(plus_exp* to_init, variable* left, variable* right) {
  to_init->super.exp_run_fun= &plus_exp_run;
  // TODO(johnicholas.hines@gmail.com): maybe init super?
  to_init->left_hand_side= left;
  to_init->right_hand_side= right;
}

void exp_run(exp* to_run, change_set* changed) {
  (to_run->exp_run_fun)((void*)to_run, changed);
}

void constraint_init(constraint* to_init, exp* to_keep_zero) {
  to_init->my_exp= to_keep_zero;
}

void constraint_run(constraint* to_run, change_set* changed) {
  // TODO(johnicholas.hines@gmail.com): Refactor.
  // maybe something like "solve_for_complement_of(to_run->my_exp, changed)"?
  exp_run(to_run->my_exp, changed);
}

void constraint_set_init(constraint_set* to_init) {
  to_init->my_first_constraint= 0;
}

void constraint_set_add(constraint_set* to_mutate, constraint* to_add) {
  if (to_mutate->my_first_constraint == 0) {
    to_mutate->my_first_constraint= to_add;
  } else {
    // TODO(johnicholas.hines@gmail.com): What if there are more than one?
    assert(0);
  }
}

void constraint_set_run(constraint_set* to_enforce, change_set* changed) {
  if (to_enforce->my_first_constraint == 0) {
    // no constraints, nothing to enforce.
  } else {
    // TODO(johnicholas.hines@gmail.com): What if there are more than one?
    constraint_run(to_enforce->my_first_constraint, changed);
  }
}

void change_set_init(change_set* to_init) {
  to_init->my_first_variable= 0;
}

void change_set_add(change_set* to_mutate, variable* to_add) {
  to_mutate->my_first_variable= to_add;
}

int change_set_contains(change_set* to_access, variable* to_check) {
  if (to_access->my_first_variable == 0) {
    return 0;
  } else {
    return to_access->my_first_variable == to_check;
  }
}

void repair_plan_init(repair_plan* to_init,
		      constraint_set* to_enforce,
		      change_set* the_vars_that_will_change) {
  to_init->to_enforce= to_enforce;
  to_init->changing_vars= the_vars_that_will_change;
  // TODO(johnicholas.hines@gmail.com): actually do some planning here?
}

void repair_plan_run(repair_plan* to_run) {
  constraint_set_run(to_run->to_enforce, to_run->changing_vars);
}

