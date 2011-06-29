#include <dlfcn.h>
#include <ffi.h>
#include <stdio.h>

void greet() __attribute__((used));
void greet() {
  printf("Hello world!\n");
}

int main(int argc, char* argv[]) {
  void* this_application= dlopen(NULL, // this would normally be a filename
				 RTLD_LAZY);
  if (! this_application ) {
    printf("Some dlopen error: %s\n", dlerror());
  }
  dlerror();
  void* my_greet_voidptr= dlsym(this_application, "greet");
  char* error= dlerror();
  if (error != NULL) {
    printf("Some dlsym error: %s\n", error);
  }
  void (*my_greet_funptr)();
  *(void**)(&my_greet_funptr)= my_greet_voidptr;
  
  ffi_cif the_cif;
  ffi_status status= ffi_prep_cif(&the_cif,
				  FFI_DEFAULT_ABI,
				  0, // zero arguments
				  &ffi_type_void, // returning void
				  NULL // an array of zero argument types
				  );
  if (status != FFI_OK) {
    printf("Some error.\n");
  }
  ffi_call(&the_cif,
	   my_greet_voidptr, // my_greet_funptr,
	   NULL, // discard any return value
	   NULL // an array of zero arguments
	   );
  return 0;
}
