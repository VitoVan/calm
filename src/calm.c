/*
 * I am formatting this file with:
 *    clang-format --style=Google -i calm.c
 * So, if you are editing this file, please format it afterwards
 *
 * Compile on Windows:
 *  GUI:
 *    cl /Fe:calmNoConsole calm.c /link /SUBSYSTEM:WINDOWS /ENTRY:mainCRTStartup
 *  Console:
 *    cl /Fe:calm calm.c /link /SUBSYSTEM:CONSOLE
 *
 * Compile on *nix:
 *    gcc calm.c -o calm
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

const char *getbinarypath() {
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

const char *getbinarydir() {
#ifdef _WIN32
  static char drive[FILENAME_MAX];
  char dir[FILENAME_MAX];
  _splitpath_s(getbinarypath(), drive, sizeof(drive), dir, sizeof(dir), NULL, 0,
               NULL, 0);
  return strcat(drive, dir);
#else
  return dirname((char *)getbinarypath());
#endif
}

const char *getlibpath() {
  static char lib_path[FILENAME_MAX];
#ifdef __APPLE__
  /*
   * add quotation mark, to avoid white-space problems
   * this shit seems only work on macOS
   */

  strcpy(lib_path, "\"");
#else
  strcpy(lib_path, "");
#endif

  strcat(lib_path, getbinarydir());

#ifdef _WIN32
  char *lib_rel = "lib";
#else
  char *lib_rel = "/lib";
#endif
  strcat(lib_path, lib_rel);

#ifdef __APPLE__
  strcat(lib_path, "\"");
#endif

  printf("LIB_PATH=%s\n", lib_path);
  return lib_path;
}

const char *getsbclpath() {
  static char sbcl_path[FILENAME_MAX];
  strcpy(sbcl_path, " \"");  // to escape white-space
  strcat(sbcl_path, getbinarydir());
#ifdef _WIN32
  strcat(sbcl_path, "sbcl\\bin\\sbcl");
#else
  strcat(sbcl_path, "/sbcl/bin/sbcl");
#endif
  strcat(sbcl_path, "\"");  // to escape white-space
  return sbcl_path;
}

/*
 * return the new value for
 *     Linux: LD_LIBRARY_PATH
 *     macOS: DYLD_FALLBACK_LIBRARY_PATH
 *     Windows: PATH
 */
const char *getlibenv() {
  const char *lib_path = getlibpath();
  char *path_separator = ":";
#ifdef __APPLE__
  char *ori_lib_env = getenv("DYLD_FALLBACK_LIBRARY_PATH");
#elif defined __linux__
  char *ori_lib_env = getenv("LD_LIBRARY_PATH");
#elif defined _WIN32
  path_separator = ";";
  char *ori_lib_env = getenv("PATH");
#endif
  if (ori_lib_env == NULL) {
    ori_lib_env = "";
  }
  char *lib_env = malloc(strlen(ori_lib_env) * sizeof(char) +
                         strlen(lib_path) * sizeof(char) + 1);
  strcpy(lib_env, ori_lib_env);
  if (strlen(ori_lib_env) > 0) {
    strcat(lib_env, path_separator);
  }
  strcat(lib_env, lib_path);
  printf("LIB_ENV=%s\n", lib_env);
  return lib_env;
}

/*
 * show an alert, for the first time long loading process
 * only needed when running `calm` command
 */
void firstrun() {
  if (getenv("CI") == NULL) {
    if (access(".calm-initialised", F_OK) != 0) {
#ifdef __APPLE__
      system(
          "osascript -e 'display alert \"Initialising CALM ...\" message "
          "\"This may take a few minutes.\" buttons {\"OK, I will wait\"} "
          "giving up after 10' && touch .calm-initialised &");
#endif
    }
  }
}

int main(int argc, char *argv[]) {
  char calm_cmd[1024];
  const char *lib_env = getlibenv();

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

  /*
   * applying APP_DIR
   */
  char cwd[FILENAME_MAX];
  getcwd(cwd, sizeof(cwd));
#ifdef _WIN32
  strcat(cwd, backslash);
#else
  strcat(cwd, slash);
#endif
  applyenv("APP_DIR", cwd);
  printf("APP_DIR=%s \n", getenv("APP_DIR"));

  /*
   * applying CALM_DIR
   */
  char binarydir[FILENAME_MAX];
  strcpy(binarydir, getbinarydir());
#ifndef _WIN32
  strcat(binarydir, slash);
#endif
  applyenv("CALM_DIR", binarydir);
  printf("CALM_DIR=%s \n", getenv("CALM_DIR"));

  /*
   * applying SBCL_HOME
   */
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

  /*
   * cd to CALM_DIR
   */
  chdir(getenv("CALM_DIR"));

  /*
   * ==============
   * detecting SBCL path - START
   * ==============
   */

  char entry_cmd[FILENAME_MAX];

  char *calm_host_lisp = getenv("CALM_HOST_LISP");

  if (calm_host_lisp) {
    strcpy(entry_cmd, calm_host_lisp);
  } else {
#ifdef __APPLE__
    int calm_is_building = getenv("CALM_BUILDING") ? 1 : 0;
    if (calm_is_building == 1) {  // Building, don't set lib env
      strcpy(entry_cmd, getsbclpath());
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
      strcat(entry_cmd, lib_env);
      strcat(entry_cmd, " ");
      strcat(entry_cmd, getsbclpath());
    }
#elif defined _WIN32
    applyenv("PATH", lib_env);
    printf("PATH=%s \n", getenv("PATH"));
    strcpy(entry_cmd, getsbclpath());
#elif defined __linux__
    applyenv("LD_LIBRARY_PATH", lib_env);
    printf("LD_LIBRARY_PATH=%s \n", getenv("LD_LIBRARY_PATH"));
    strcpy(entry_cmd, getsbclpath());
#endif
  }

  if (access("calm.asd", F_OK) == 0) {
    /*
     * ==============
     * Running CALM
     * ==============
     */

    firstrun();

    if (access("calm.core", F_OK) == 0) {
      strcat(entry_cmd, " --core calm.core");
    }

    if (calm_host_lisp == NULL) {
      // Disable .sbclrc
      strcat(entry_cmd, " --no-sysinit --no-userinit");
    }

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

    if (system(entry_cmd) != 0) return 42;
  } else {
    /*
     * ==============
     * Running CALM Application
     * ==============
     */

    printf("EXECUTING: bin/calm-app\n");

#ifdef _WIN32
    if (WinExec("bin\\calm-app.exe", SW_NORMAL) > 31) {
      return 0;
    } else {
      return 42;
    }
#elif defined __APPLE__
    strcpy(entry_cmd, "DYLD_FALLBACK_LIBRARY_PATH=");
    strcat(entry_cmd, lib_env);
    strcat(entry_cmd, " bin/calm-app");
    if (system(entry_cmd) != 0) return 42;
#else
    if (system("bin/calm-app") != 0) return 42;
#endif
  }

  return 0;
}
