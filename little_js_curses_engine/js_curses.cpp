#include <cassert>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <curses.h>
#include <v8.h>

using namespace v8;

Handle<Value> Addch(const Arguments& args) {
  assert(args.Length() == 1);
  Handle<String> to_add = args[0]->ToString();
  assert(to_add->Length() == 1);
  String::AsciiValue ascii(to_add);
  addch((*ascii)[0]);
  return Undefined();
}

Handle<Value> Getch(const Arguments& args) {
  int answer = getch();
  assert(answer > 0);
  assert(answer <= 255);
  const char answer_string[]= { answer, '\0' };
  return String::New(answer_string);
}

Handle<Value> Move(const Arguments& args) {
  assert(args.Length() == 2);
  Handle<Number> row = args[0]->ToNumber();
  Handle<Number> col = args[1]->ToNumber();
  move(round(row->Value()), round(col->Value()));
  return Undefined();
}

// From v8 sample shell.cc
Handle<String> ReadFile(const char* name) {
  FILE* file = fopen(name, "rb");
  if (file == NULL) {
    return Handle<String>();
  }
  fseek(file, 0, SEEK_END);
  int size = ftell(file);
  rewind(file);
  char* chars = new char[size + 1];
  chars[size] = '\0';
  for (int i= 0; i < size;) {
    int read = fread(&chars[i], 1, size - i, file);
    i += read;
  }
  fclose(file);
  Handle<String> result = String::New(chars, size);
  delete[] chars;
  return result;
}

// from v8 sample shell.cc
Handle<Value> Quit(const Arguments& args) {
  int exit_code = args[0]->Int32Value();
  endwin();
  exit(exit_code);
  return Undefined();
}

Handle<Value> Start(const Arguments& args) {
  assert(args.Length() == 1);
  Handle<Object> game_constructor = args[0]->ToObject();
  assert(game_constructor->IsFunction());
  Handle<Object> game = Handle<Function>::Cast(game_constructor)->NewInstance();
  // Setup
  Handle<Value> setup_value = game->Get(String::New("setup"));
  if (setup_value->IsFunction()) {
    Handle<Function>::Cast(setup_value)->Call(game, 0, NULL);
  }
  while (1) {
    // Draw
    Handle<Value> draw_value = game->Get(String::New("draw"));
    if (draw_value->IsFunction()) {
      Handle<Function>::Cast(draw_value)->Call(game, 0, NULL);
    }
    refresh();
    // Update
    Handle<Value> update_value = game->Get(String::New("update"));
    int answer = getch();
    if (update_value->IsFunction()) {
      if (answer == ERR) {
	Handle<Function>::Cast(update_value)->Call(game, 0, NULL);
      } else {
	assert(answer > 0);
	assert(answer <= 255);
	const char answer_string[]= { answer, '\0' };
	Handle<Value> update_args[]= { String::New(answer_string) };
	Handle<Function>::Cast(update_value)->Call(game, 1, update_args);
      }
    }
  }
  return Undefined();
}

int main(int argc, char* argv[]) {
  if (argc != 2) {
    fprintf(stderr, "Usage: js_curses something.js\n");
    exit(1);
  }
  HandleScope handle_scope;
  // bindings
  Handle<ObjectTemplate> global = ObjectTemplate::New();
  global->Set(String::New("addch"), FunctionTemplate::New(Addch));
  global->Set(String::New("getch"), FunctionTemplate::New(Getch));
  global->Set(String::New("move"), FunctionTemplate::New(Move));
  global->Set(String::New("start"), FunctionTemplate::New(Start));
  global->Set(String::New("quit"), FunctionTemplate::New(Quit));

  Persistent<Context> context = Context::New(NULL, global);
  Context::Scope context_scope(context);

  Handle<String> source_code = ReadFile(argv[1]);
  Handle<Script> byte_code = Script::Compile(source_code);

  initscr();
  cbreak(); // don't buffer user input into lines, but Ctrl-Z and Ctrl-C should work as usual
  noecho(); // don't echo user input.
  halfdelay(1); // getch returns ERR if they take more than a tenth of a second
  keypad(stdscr, TRUE); // don't ignore function keys F1, F2, etc.
  Handle<Value> result = byte_code->Run();
  context.Dispose();
  return 0;
}
