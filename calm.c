/*
 * Compile on Windows:
 *    GUI:
 *        cl /Fe:calmGUI calm.c /link /SUBSYSTEM:WINDOWS /ENTRY:mainCRTStartup
 *    Console:
 *        cl /Fe:calm calm.c /link /SUBSYSTEM:CONSOLE
 */

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef _WIN32
#include <windows.h>
#define F_OK 0
#else
#include <libgen.h>
#include <unistd.h>
#endif

void applyenv(char const *variable, char const *value) {
#ifdef _WIN32
  _putenv_s(variable, value);
#else
  setenv(variable, value, 1);
#endif
}

char *getbinarypath() {
  static char path[FILENAME_MAX];
  uint32_t size = sizeof(path);
#ifdef __APPLE__
  _NSGetExecutablePath(path, &size);
#elif defined _WIN32
  GetModuleFileName(NULL, path, size);
#elif defined __linux__
  readlink("/proc/self/exe", path, size);
#endif
  return path;
}

char *getbinarydir() {
#ifdef _WIN32
  char drive[FILENAME_MAX];
  char dir[FILENAME_MAX];
  _splitpath_s(getbinarypath(), drive, sizeof(drive), dir, sizeof(dir), NULL, 0,
               NULL, 0);
  return strcat(drive, dir);
#else
  return dirname(getbinarypath());
#endif
}

int main(int argc, char *argv[]) {
  char calm_cmd[1024];

  if (argc >= 2) {
    strcpy(calm_cmd, argv[1]);
  } else {
    strcpy(calm_cmd, "show");
  }

  applyenv("CALM_CMD", calm_cmd);
  printf("CALM_CMD=%s\n", getenv("CALM_CMD"));

#ifdef _WIN32
  char backslash[] = "\\";
#else
  char slash[] = "/";
#endif

  // applying APP_DIR
  char cwd[FILENAME_MAX];
  getcwd(cwd, sizeof(cwd));
#ifdef _WIN32
  strcat(cwd, backslash);
#else
  strcat(cwd, slash);
#endif
  applyenv("APP_DIR", cwd);
  printf("APP_DIR=%s \n", getenv("APP_DIR"));

  // applying CALM_DIR
  char binarydir[FILENAME_MAX];
  strcpy(binarydir, getbinarydir());
#ifndef _WIN32
  strcat(binarydir, slash);
#endif
  applyenv("CALM_DIR", binarydir);
  printf("CALM_DIR=%s \n", getenv("CALM_DIR"));

  // applying SBCL_HOME
#ifdef _WIN32
  char *sbcl_home_rel = "sbcl\\lib\\sbcl";
#else
  char *sbcl_home_rel = "/sbcl/lib/sbcl";
#endif
  char sbcl_home[FILENAME_MAX];
  strcpy(sbcl_home, getbinarydir());
  strcat(sbcl_home, sbcl_home_rel);
  applyenv("SBCL_HOME", sbcl_home);
  printf("SBCL_HOME=%s \n", getenv("SBCL_HOME"));

  // applying Library PATH
#ifdef _WIN32
  char *lib_rel = "lib";
#else
  char *lib_rel = "/lib";
#endif

  char lib_path[FILENAME_MAX];
  strcpy(lib_path, getbinarydir());
  strcat(lib_path, lib_rel);
  printf("LIB_PATH=%s\n", lib_path);

  // change DIR to CALM_DIR
  chdir(getenv("CALM_DIR"));

  char entry_cmd[FILENAME_MAX];
  strcpy(entry_cmd, getbinarydir());

#ifdef __APPLE__
  int calm_is_building = getenv("CALM_BUILDING") ? 1 : 0;
  if (calm_is_building == 1) {  // Building, don't set DYLD env
    strcat(entry_cmd, "/sbcl/bin/sbcl");
  } else {
    /*
     * Apple won't allow us to modify DYLD_FALLBACK_LIBRARY_PATH:
     * https://developer.apple.com/forums/thread/13161
     * https://developer.apple.com/library/archive/documentation/Security/Conceptual/System_Integrity_Protection_Guide/RuntimeProtections/RuntimeProtections.html
     * So the following won't work:
     *     applyenv("DYLD_FALLBACK_LIBRARY_PATH", lib_path);
     * We have to prepend this inside the command, like:
     *     DYLD_FALLBACK_LIBRARY_PATH=/some/where/my/lib ./my-app
     */
    strcpy(entry_cmd, "DYLD_FALLBACK_LIBRARY_PATH=");
    strcat(entry_cmd, "\"");
    strcat(entry_cmd, lib_path);
    strcat(entry_cmd, "\"");
    strcat(entry_cmd, " ");
    strcat(entry_cmd, getbinarydir());
    strcat(entry_cmd, "/sbcl/bin/sbcl");
  }
#elif defined _WIN32
  char *ori_path = getenv("PATH");
  char *new_path = malloc(strlen(ori_path) * sizeof(char) +
                          strlen(lib_path) * sizeof(char) + 1);
  strcpy(new_path, ori_path);
  strcat(new_path, ";");
  strcat(new_path, lib_path);
  applyenv("PATH", new_path);
  printf("PATH=%s \n", getenv("PATH"));
  strcat(entry_cmd, "sbcl\\bin\\sbcl.exe");
#elif defined __linux__
  applyenv("LD_LIBRARY_PATH", lib_path);
  printf("LD_LIBRARY_PATH=%s \n", getenv("LD_LIBRARY_PATH"));
  strcat(entry_cmd, "/sbcl/bin/sbcl");
#endif

  if (access("calm.asd", F_OK) == 0) {
    if (access("calm.core", F_OK) == 0) {
      strcat(entry_cmd, " --core calm.core");
    }

    // Disable .sbclrc
    strcat(entry_cmd, " --no-sysinit --no-userinit");

    if (strcmp(calm_cmd, "sbcl") == 0) {
      for (int i = 2; i < argc; i++) {
        strcat(entry_cmd, " ");
        strcat(entry_cmd, argv[i]);
      }
    } else if (strcmp(calm_cmd, "sh") == 0) {
      strcpy(entry_cmd, "sh ./sh");
      for (int i = 2; i < argc; i++) {
        strcat(entry_cmd, "/");
        strcat(entry_cmd, argv[i]);
      }
      strcat(entry_cmd, ".sh");
    } else {
      strcat(entry_cmd, " --load entry.lisp");
    }

    // Execute Entry Command
    printf("EXECUTING: %s\n", entry_cmd);

    system(entry_cmd);
    // system("env");
  } else {
    printf("EXECUTING: bin/calm-app\n");

#ifdef _WIN32
    WinExec("bin\\calm-app.exe", SW_NORMAL);
#elif defined __APPLE__
    strcpy(entry_cmd, "DYLD_FALLBACK_LIBRARY_PATH=");
    strcat(entry_cmd, "\"");
    strcat(entry_cmd, lib_path);
    strcat(entry_cmd, "\"");
    strcat(entry_cmd, " bin/calm-app");
    system(entry_cmd);
#else
    system("bin/calm-app");
#endif
  }

  return 0;
}
