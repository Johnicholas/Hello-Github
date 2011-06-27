#include "constraints.h"
#include <assert.h>

int main() {
  // x is a variable
  variable x;
  // x is initially 12
  variable_init(&x, 12);
  // y is a variable
  variable y;
  // y is also initially 12
  variable_init(&y, 12);
  // x_plus_y is a plus expression
  plus_exp x_plus_y;
  // x_plus_y combines the variables (which are a kind of expression?) x and y
  plus_exp_init(&x_plus_y, &x, &y);
  // x_equals_y is a constraint
  constraint x_equals_y;
  // x_equals_y constrains the expression x_plus_y to be zero
  constraint_init(&x_equals_y, (exp*)&x_plus_y);
  // all_constraints is a constraint set
  constraint_set all_constraints;
  // the only constraint is x_equals_y
  constraint_set_init(&all_constraints);
  constraint_set_add(&all_constraints, &x_equals_y);
  // just_x is a change set
  change_set just_x;
  // just_x is a set consisting of x.
  change_set_init(&just_x);
  change_set_add(&just_x, &x);
  // change_x_propagates_to_y is a repair plan for when x changes
  repair_plan change_x_propagates_to_y;
  repair_plan_init(&change_x_propagates_to_y, &all_constraints, &just_x);
  // change x to 34
  variable_set(&x, 34);
  // use the repair plan to propagate the change
  repair_plan_run(&change_x_propagates_to_y);
  assert(variable_get(&y) == 34);

  change_set just_y;
  change_set_init(&just_y);
  change_set_add(&just_y, &y);
  repair_plan change_y_propagates_to_x;
  repair_plan_init(&change_y_propagates_to_x, &all_constraints, &just_y);
  // change y to 56;
  variable_set(&y, 56);
  // use the repair plan to propagate the change
  repair_plan_run(&change_y_propagates_to_x);
  assert(variable_get(&x) == 56);

  return 0;
}

