/*

// this list was taken from Maybe: https://github.com/p-e-w/maybe/blob/master/maybe/syscall_filters.py

SYSCALL_FILTERS = [
# Delete
SyscallFilter(
    name="unlink",
    signature=("int", (("const char *", "pathname"),)),
    format=lambda args: format_delete(args[0]),
),
SyscallFilter(
    name="unlinkat",
    signature=("int", (("int", "dirfd"), ("const char *", "pathname"), ("int", "flags"),)),
    format=lambda args: format_delete(args[1]),
),
SyscallFilter(
    name="rmdir",
    signature=("int", (("const char *", "pathname"),)),
    format=lambda args: format_delete(args[0]),
),
# Move
SyscallFilter(
    name="rename",
    signature=("int", (("const char *", "oldpath"), ("const char *", "newpath"),)),
    format=lambda args: format_move(args[0], args[1]),
),
SyscallFilter(
    name="renameat",
    signature=("int", (("int", "olddirfd"), ("const char *", "oldpath"),
                       ("int", "newdirfd"), ("const char *", "newpath"),)),
    format=lambda args: format_move(args[1], args[3]),
),
SyscallFilter(
    name="renameat2",
    signature=("int", (("int", "olddirfd"), ("const char *", "oldpath"),
                       ("int", "newdirfd"), ("const char *", "newpath"), ("unsigned int", "flags"),)),
    format=lambda args: format_move(args[1], args[3]),
),
# Change permissions
SyscallFilter(
    name="chmod",
    signature=("int", (("const char *", "pathname"), ("mode_t", "mode"),)),
    format=lambda args: format_change_permissions(args[0], args[1]),
),
SyscallFilter(
    name="fchmod",
    signature=("int", (("int", "fd"), ("mode_t", "mode"),)),
    format=lambda args: format_change_permissions(get_file_descriptor_path(args[0]), args[1]),
),
SyscallFilter(
    name="fchmodat",
    signature=("int", (("int", "dirfd"), ("const char *", "pathname"), ("mode_t", "mode"), ("int", "flags"),)),
    format=lambda args: format_change_permissions(args[1], args[2]),
),
# Change owner
SyscallFilter(
    name="chown",
    signature=("int", (("const char *", "pathname"), ("uid_t", "owner"), ("gid_t", "group"),)),
    format=lambda args: format_change_owner(args[0], args[1], args[2]),
),
SyscallFilter(
    name="fchown",
    signature=("int", (("int", "fd"), ("uid_t", "owner"), ("gid_t", "group"),)),
    format=lambda args: format_change_owner(get_file_descriptor_path(args[0]), args[1], args[2]),
),
SyscallFilter(
    name="lchown",
    signature=("int", (("const char *", "pathname"), ("uid_t", "owner"), ("gid_t", "group"),)),
    format=lambda args: format_change_owner(args[0], args[1], args[2]),
),
SyscallFilter(
    name="fchownat",
    signature=("int", (("int", "dirfd"), ("const char *", "pathname"),
                       ("uid_t", "owner"), ("gid_t", "group"), ("int", "flags"),)),
    format=lambda args: format_change_owner(args[1], args[2], args[3]),
),
# Create directory
SyscallFilter(
    name="mkdir",
    signature=("int", (("const char *", "pathname"), ("mode_t", "mode"),)),
    format=lambda args: format_create_directory(args[0]),
),
SyscallFilter(
    name="mkdirat",
    signature=("int", (("int", "dirfd"), ("const char *", "pathname"), ("mode_t", "mode"),)),
    format=lambda args: format_create_directory(args[1]),
),
# Create link
SyscallFilter(
    name="link",
    signature=("int", (("const char *", "oldpath"), ("const char *", "newpath"),)),
    format=lambda args: format_create_link(args[1], args[0], False),
),
SyscallFilter(
    name="linkat",
    signature=("int", (("int", "olddirfd"), ("const char *", "oldpath"),
                       ("int", "newdirfd"), ("const char *", "newpath"), ("int", "flags"),)),
    format=lambda args: format_create_link(args[3], args[1], False),
),
SyscallFilter(
    name="symlink",
    signature=("int", (("const char *", "target"), ("const char *", "linkpath"),)),
    format=lambda args: format_create_link(args[1], args[0], True),
),
SyscallFilter(
    name="symlinkat",
    signature=("int", (("const char *", "target"), ("int", "newdirfd"), ("const char *", "linkpath"),)),
    format=lambda args: format_create_link(args[2], args[0], True),
),
# Open/create file
SyscallFilter(
    name="open",
    # TODO: "open" is overloaded (a version with 3 arguments also exists). Are both handled properly?
    signature=("int", (("const char *", "pathname"), ("int", "flags"),)),
    format=lambda args: format_open(args[0], args[1]),
    substitute=lambda args: substitute_open(args[0], args[1]),
),
SyscallFilter(
    name="creat",
    signature=("int", (("const char *", "pathname"), ("mode_t", "mode"),)),
    format=lambda args: format_open(args[0], O_CREAT | O_WRONLY | O_TRUNC),
    substitute=lambda args: substitute_open(args[0], O_CREAT | O_WRONLY | O_TRUNC),
),
SyscallFilter(
    name="openat",
    # TODO: "openat" is overloaded (see above)
    signature=("int", (("int", "dirfd"), ("const char *", "pathname"), ("int", "flags"),)),
    format=lambda args: format_open(args[1], args[2]),
    substitute=lambda args: substitute_open(args[1], args[2]),
),
SyscallFilter(
    name="mknod",
    signature=("int", (("const char *", "pathname"), ("mode_t", "mode"),)),
    format=lambda args: format_mknod(args[0], args[1]),
    substitute=lambda args: substitute_mknod(args[0], args[1]),
),
SyscallFilter(
    name="mknodat",
    signature=("int", (("int", "dirfd"), ("const char *", "pathname"), ("mode_t", "mode"),)),
    format=lambda args: format_mknod(args[1], args[2]),
    substitute=lambda args: substitute_mknod(args[1], args[2]),
),
SyscallFilter(
    name="mkfifo",
    signature=("int", (("const char *", "pathname"), ("mode_t", "mode"),)),
    format=lambda args: format_mknod(args[0], S_IFIFO),
    substitute=lambda args: substitute_mknod(args[0], S_IFIFO),
),
SyscallFilter(
    name="mkfifoat",
    signature=("int", (("int", "dirfd"), ("const char *", "pathname"), ("mode_t", "mode"),)),
    format=lambda args: format_mknod(args[1], S_IFIFO),
    substitute=lambda args: substitute_mknod(args[1], S_IFIFO),
),
# Write to file
SyscallFilter(
    name="write",
    signature=("ssize_t", (("int", "fd"), ("const void *", "buf"), ("size_t", "count"),)),
    format=lambda args: format_write(args[0], args[2]),
    substitute=lambda args: substitute_write(args[0], args[2]),
),
SyscallFilter(
    name="pwrite",
    signature=("ssize_t", (("int", "fd"), ("const void *", "buf"), ("size_t", "count"), ("off_t", "offset"),)),
    format=lambda args: format_write(args[0], args[2]),
    substitute=lambda args: substitute_write(args[0], args[2]),
),
SyscallFilter(
    name="writev",
    signature=("ssize_t", (("int", "fd"), ("const struct iovec *", "iov"), ("int", "iovcnt"),)),
    # TODO: Actual byte count is iovcnt * iov.iov_len
    format=lambda args: format_write(args[0], args[2]),
    substitute=lambda args: substitute_write(args[0], args[2]),
),
SyscallFilter(
    name="pwritev",
    signature=("ssize_t", (("int", "fd"), ("const struct iovec *", "iov"), ("int", "iovcnt"), ("off_t", "offset"),)),
    # TODO: Actual byte count is iovcnt * iov.iov_len
    format=lambda args: format_write(args[0], args[2]),
    substitute=lambda args: substitute_write(args[0], args[2]),
),
# Duplicate file descriptor
SyscallFilter(
    name="dup",
    signature=("int", (("int", "oldfd"),)),
    format=lambda args: None,
    substitute=lambda args: substitute_dup(args[0]),
),
SyscallFilter(
    name="dup2",
    signature=("int", (("int", "oldfd"), ("int", "newfd"),)),
    format=lambda args: None,
    substitute=lambda args: substitute_dup(args[0], args[1]),
),
SyscallFilter(
    name="dup3",
    signature=("int", (("int", "oldfd"), ("int", "newfd"), ("int", "flags"),)),
    format=lambda args: None,
    substitute=lambda args: substitute_dup(args[0], args[1]),
),

*/

// This skeleton was taken from http://mips42.altervista.org/ptrace.php


#include <sys/ptrace.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include <linux/user.h>
#include <sys/syscall.h>
#include <sys/reg.h>

#define TARGET "/absolute/path/to/the/target"
#define NEW_UID 0

int main() {
  int status = 0;
  int syscall_n = 0;
  int entering = 1;
  struct user_regs_struct regs;
  int pid = fork();

  if ( !pid ) {
    ptrace( PTRACE_TRACEME, 0, 0, 0 );
    execlp( TARGET, TARGET, 0 );
  }
  else {
    wait( &status );

    while ( 1 ) {
      ptrace( PTRACE_SYSCALL, pid, 0, 0 );

      wait( &status );

      if ( WIFEXITED( status ) ) break;

      ptrace( PTRACE_GETREGS, pid, 0, &regs );
      syscall_n = regs.orig_eax;
      if ( syscall_n == SYS_getuid32 ) {
        if ( entering ) {
          entering = 0;
        }
        else {
          ptrace( PTRACE_GETREGS, pid, 0, &regs );
          regs.eax = NEW_UID;
          ptrace( PTRACE_SETREGS, pid, 0, &regs );
          entering = 1;
        }
      }
    }
  }

  return 0;
}
