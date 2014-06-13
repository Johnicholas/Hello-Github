#include "assert.h"
#include <iostream>
#include <map>
#include <sstream>

enum result {
  SUCCESS,
  FAILURE,
  RUNNING
};

enum device {
  FILM = 0x001,
  CASE = 0x010,
  BATTERY = 0x100
};

// this is a "truth surface" keeping track of what has
// been declared to be true (or false).
std::map<device, std::map<std::string, bool> > surface;

// An decision tree that can be reevaluated quickly if the
// new scenario is almost the same as the old one.
class IncrementalDT {
public:
  virtual result start() = 0;
  // observer pattern
  // normally just "notify()" we add the device as a hint
  // that conditions testing other devices do not need to be
  // re-evaluated.
  virtual result notify(device) = 0;
  // which resources does this DT depend on?
  virtual int resources() = 0;
  // Destructor
  ~IncrementalDT() {};
protected:
  // Constructor
  IncrementalDT() {};
};

class Sequence : public IncrementalDT {
public:
  // Constructor
  Sequence(IncrementalDT* lhs, IncrementalDT* rhs);
  // from IncrementalDT
  result start();
  // from IncrementalDT
  result notify(device);
  // from IncrementalDT
  int resources();
private:
  IncrementalDT* lhs;
  IncrementalDT* rhs;
  int r;
  result current;
  bool lhs_running;
};

// Constructor
Sequence::Sequence(IncrementalDT* lhs, IncrementalDT* rhs) :
  lhs(lhs),
  rhs(rhs),
  r(lhs->resources() | rhs->resources())
{
}

result Sequence::start() {
  current = lhs->start();
  if (current == SUCCESS) {
    lhs_running = false;
    current = rhs->start();
  } else {
    lhs_running = true;
  }
  return current;
}

result Sequence::notify(device d) {
  if (not (d & r)) {
    return current;
  }
  if (lhs_running) {
    current = lhs->notify(d);
    if (current == SUCCESS) {
      lhs_running = false;
      current = rhs->start();
    }
    return current;
  } else {
    current = rhs->notify(d);
    return current;
  }
}

int Sequence::resources() {
  return r;
}

class Selector : public IncrementalDT {
public:
  // Constructor
  Selector(IncrementalDT* lhs, IncrementalDT* rhs);
  // from IncrementalDT
  result start();
  // from IncrementalDT
  result notify(device);
  // from IncrementalDT
  int resources();
private:
  IncrementalDT* lhs;
  IncrementalDT* rhs;
  int r;
  result current;
  bool lhs_running;
};

// Constructor
Selector::Selector(IncrementalDT* lhs, IncrementalDT* rhs) :
  lhs(lhs),
  rhs(rhs),
  r(lhs->resources() | rhs->resources())
{
}

result Selector::start() {
  current = lhs->start();
  if (current == FAILURE) {
    lhs_running = false;
    current = rhs->start();
  } else {
    lhs_running = true;
  }
  return current;
}

result Selector::notify(device d) {
  if (d & r) {
    if (lhs_running) {
      current = lhs->notify(d);
      if (current == FAILURE) {
        lhs_running = false;
        current = rhs->start();
      }
    } else {
      current = rhs->notify(d);
    }
  }
  return current;
}

int Selector::resources() {
  return r;
}

/*
class Selector : public IncrementalDT {
public:
  // Constructor
  Selector(IncrementalDT* lhs, IncrementalDT* rhs);
  // from IncrementalDT
  result start();
  // from IncrementalDT
  result notify(device);
  // from IncrementalDT
  int resources();
private:
  IncrementalDT* lhs;
  IncrementalDT* rhs;
  int r;
  result current;
  bool lhs_running;
};

// Constructor
Selector::Selector(IncrementalDT* lhs, IncrementalDT* rhs) :
  lhs(lhs),
  rhs(rhs),
  r(lhs->resources() | rhs->resources())
{
}

result Selector::start() {
  current = lhs->start();
  if (current == FAILURE) {
    current = rhs->start();
  }
  return current;
}

result Selector::notify(device d) {
  if (d & r) {
    current = lhs->notify(d);
    if (current == FAILURE) {
      current = rhs->start();
    }
  }
  return current;
}

int Selector::resources() {
  return r;
}
*/

class Condition : public IncrementalDT {
public:
  // Constructor
  Condition(std::string prompt, device resources);
  // from IncrementalDT
  result start();
  // from IncrementalDT
  result notify(device);
  // from IncrementalDT
  int resources();
private:
  device r;
  std::string key;
};

// Constructor
Condition::Condition(std::string key, device r) :
  r(r),
  key(key)
{
}

result Condition::start() {
  if (surface[r][key]) {
    return SUCCESS;
  } else {
    return FAILURE;
  }
}

result Condition::notify(device d) {
  if (surface[r][key]) {
    return SUCCESS;
  } else {
    return FAILURE;
  }
}

int Condition::resources() {
  return r;
}

class Command : public IncrementalDT {
public:
  // Constructor
  Command(std::string prompt, int resources);
  // from IncrementalDT
  result start();
  // from IncrementalDT
  result notify(device);
  // from IncrementalDT
  int resources();
private:
  std::string prompt;
  int r;
  result current;
};

// Constructor
Command::Command(std::string prompt, int resources) :
  prompt(prompt),
  r(resources)
{
}

result Command::start() {
  std::cout << prompt << "\n";
  current = RUNNING;
  return current;
}

result Command::notify(device d) {
  assert(d & r);
  std::cout << "is " << prompt << " done?\n";
  while (true) {
    std::string line;
    std::getline(std::cin, line);
    if (line == "y" or line == "yes") {
      current = SUCCESS;
      break;
    } else if (line == "n" or line == "no") {
      current = RUNNING;
      break;
    } else {
      std::cout << "Please answer yes (or y) or no (or n)\n";
      std::cout << "is " << prompt << " done?\n";
    }
  }
  return current;
}

int Command::resources() {
  return r;
}

device get_device() {
  std::cout << "wait for a device to change\n";
  std::cout << "which device changes?\n";
  while (true) {
    std::string line;
    std::getline(std::cin, line);
    if (line == "f" or line == "film") {
      return FILM;
    } else if (line == "c" or line == "case") {
      return CASE;
    } else if (line == "b" or line == "battery") {
      return BATTERY;
    } else {
      std::cout << "Please answer (f)ilm, (c)ase, or (b)attery\n";
      std::cout << "which device is done?\n";
    }
  }
}

int main() {
    /*
    Condition film_at_start("is the film at the start of the roll?", FILM);
    Condition film_in("is there film in the camera?", FILM);
    Condition film_slot_closed("is the film slot closed?", FILM);
    Condition battery_slot_closed("is the battery slot closed?", BATTERY);
    Condition battery_in("is there a battery in the camera?", BATTERY);
    Condition battery_ok("does the battery have charge?", BATTERY);
    Command take_pictures("take pictures", FILM | CASE | BATTERY);
    Sequence a1(&battery_ok, &take_pictures);
    Sequence a2(&battery_in, &a1);
    Sequence a3(&battery_slot_closed, &a2);
    Sequence a4(&film_slot_closed, &a3);
    Sequence a5(&film_in, &a4);
    Sequence a6(&film_at_start, &a5);

    Condition battery_not_ok("is the battery out of charge?", BATTERY);
    Command close_case("close the case if open", CASE);
    Command open_case("open the case if closed", CASE);
    Command close_battery_slot("close the battery slot if open", BATTERY);
    Command open_battery_slot("open the battery slot if closed", BATTERY);
    Command remove_battery("remove the battery if it is in the slot", BATTERY);
    Command insert_new_battery("insert a new battery into the slot", BATTERY);
    Sequence a7(&remove_battery, &insert_new_battery);
    Sequence a8(&open_battery_slot, &a7);
    Sequence a9(&close_battery_slot, &a8);
    Sequence a10(&open_case, &a9);
    Sequence a11(&close_case, &a10);
    Sequence a12(&battery_not_ok, &a11);

    Condition battery_slot_opened("is the battery slot open?", BATTERY);
    // close_battery_slot
    Sequence a13(&battery_slot_opened, &close_battery_slot);

    Condition film_used("Is the film roll all used up?", FILM);
    // open_case
    Command open_film_slot("Open the film slot", FILM);
    Command rewind("Rewind the film to the start", FILM);
    Command remove_film("Remove the film from the camera", FILM);
    Command insert_new_film("Insert a new roll of film", FILM);
    Sequence a14(&remove_film, &insert_new_film);
    Sequence a15(&rewind, &a14);
    Sequence a16(&open_film_slot, &a15);
    Sequence a18(&open_case, &a16);
    Sequence a20(&film_used, &a18);

    Condition film_slot_opened("Is the film slot open?", FILM);
    Command close_film_slot("Close the film slot", FILM);
    Sequence a21(&film_slot_opened, &close_film_slot);

    Selector b1(&a20, &a21);
    Selector b2(&a13, &b1);
    Selector b3(&a12, &b2);
    Selector top(&a6, &b3);
    */

    /*
    Condition film_used("Is the film used up?", FILM);
    Command insert_new_film("Insert new film", FILM);
    Sequence replace_film(&film_used, &insert_new_film);
    Command take_pictures("take pictures", FILM);
    Selector top(&replace_film, &take_pictures);
    */

    Command open_gripper("open the gripper", CASE);
    Command move_gripper("move the gripper to paycheck", CASE);
    Command close_gripper("close the gripper", CASE);
    Sequence pickup_paycheck_helper(&move_gripper, &close_gripper);
    Sequence pickup_paycheck(&open_gripper, &pickup_paycheck_helper);

    // Nilsson's Triangle Table example:
    Command go_paycheck("goto place of paycheck", FILM);
    Condition robot_at_paycheck("robot_at_paycheck", FILM);
    surface[FILM]["robot_at_paycheck"] = false;
    // Command pickup_paycheck("pickup paycheck", FILM);
    Command go_office("goto office of John", FILM);
    Condition here_john("john_here", FILM);
    surface[FILM]["john_here"] = true;
    Condition robot_at_office("goto John's office", FILM);
    Command wait("wait", FILM);
    Condition robot_has_paycheck("robot_has_paycheck", FILM);
    surface[FILM]["robot_has_paycheck"] = false;
    Condition robot_at_john("robot_at_john", FILM);
    Command hand_john_paycheck("hand paycheck to john", FILM);
    Condition john_has_paycheck("john_has_paycheck", FILM);
    surface[FILM]["john_has_paycheck"] = false;
    Sequence maybe_go_paycheck(&here_john, &go_paycheck);
    Sequence maybe_pickup_helper(&robot_at_paycheck, &pickup_paycheck);
    Sequence maybe_pickup(&here_john, &maybe_pickup_helper);
    Selector s5(&maybe_pickup, &maybe_go_paycheck);
    Sequence maybe_go_office_helper(&robot_has_paycheck, &go_office);
    Sequence maybe_go_office(&here_john, &maybe_go_office_helper);
    Selector s4(&maybe_go_office, &s5);
    Sequence maybe_wait_helper_helper(&robot_at_office, &wait);
    Sequence maybe_wait_helper(&robot_has_paycheck, &maybe_wait_helper_helper);
    Sequence maybe_wait(&here_john, &maybe_wait_helper);
    Selector s3(&maybe_wait, &s4);
    Sequence maybe_hand_helper(&robot_at_john, &hand_john_paycheck);
    Sequence maybe_hand(&robot_has_paycheck, &maybe_hand_helper);
    Selector s2(&maybe_hand, &s3);
    Selector top(&john_has_paycheck, &s2);

    result answer = top.start();
    while (answer == RUNNING) {
      device d = get_device();
      std::string line;
      std::getline(std::cin, line);
      std::stringstream line_stream(line);
      std::string key;
      std::string value;
      line_stream >> key >> value;
      if (value == "true") {
        surface[d][key] = true;
      } else if (value == "false") {
        surface[d][key] = false;
      }
      answer = top.notify(d);
      if (answer == SUCCESS) {
        answer = top.start();
      }
    }
    if (answer == SUCCESS) {
      std::cout << "success!\n";
    } else if (answer == FAILURE) {
      std::cout << "failure.\n";
    }
    return 0;
}
