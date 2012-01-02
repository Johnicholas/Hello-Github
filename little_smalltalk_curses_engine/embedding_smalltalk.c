#include <gstpub.h>
#include <curses.h>
#include <stdlib.h>

int main(int argc, char* argv[]) {
  gst_set_var(GST_VERBOSITY, 1);
  gst_smalltalk_args(0, NULL);
  gst_set_executable_path(argv[0]);
  // we might use "/usr/lib/gnu-smalltalk/gst.im"?
  int result = gst_initialize("./kerneldir", NULL, GST_NO_TTY);
  // int result = gst_initialize(".", "/usr/lib/gnu-smalltalk/gst.im", GST_NO_TTY);
  if (result != 0) {
    exit(result < 0 ? 1 : result);
  }
  if (!gst_process_file("game_loop.st", GST_DIR_KERNEL_SYSTEM)) {
    perror("gst:  couldn't load game_loop.st");
  }
  gst_invoke_hook(GST_ABOUT_TO_QUIT);
  exit(0);
}

