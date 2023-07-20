/*
 * CAUTION:
 *
 *    !!!! SHITTY CODE AHEAD !!!!
 *
 * I tried to replace Bash with C to get more compatibility
 * on different OSes, but iSuck@C.
 * Now the bright side is that it worked,
 * the dark side is that the code is worse than the Bash scripts.
 *
 * Please forgive me, and improve the code if you don't mind.
 *
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

void apply_env(char const *variable, char const *value) {
#ifdef _WIN32
  _putenv_s(variable, value);
#else
  setenv(variable, value, 1);
#endif
}

const char *get_binary_path() {
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

const char *get_binary_dir() {
#ifdef _WIN32
  static char drive[FILENAME_MAX];
  char dir[FILENAME_MAX];
  _splitpath_s(get_binary_path(), drive, sizeof(drive), dir, sizeof(dir), NULL,
               0, NULL, 0);
  return strcat(drive, dir);
#else
  return dirname((char *)get_binary_path());
#endif
}

const char *get_lib_path() {
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

  strcat(lib_path, get_binary_dir());

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

const char *get_sbcl_path() {
  char *calm_host_lisp = getenv("CALM_HOST_LISP");

  if (calm_host_lisp) return calm_host_lisp;

  static char sbcl_path[FILENAME_MAX];
  strcpy(sbcl_path, " \"");  // to escape white-space
  strcat(sbcl_path, get_binary_dir());
#ifdef _WIN32
  if (getenv("CALM_WITH_ICON") == NULL) {
    strcat(sbcl_path, "sbcl\\bin\\sbcl");
  } else {
    strcat(sbcl_path, "sbcl\\bin\\sbcl-with-icon");
  }
#else
  strcat(sbcl_path, "/sbcl/bin/sbcl");
#endif
  strcat(sbcl_path, "\"");  // to escape white-space
  if (access("calm.core", F_OK) == 0 && getenv("CALM_NO_CORE") == NULL) {
    strcat(sbcl_path, " --core calm.core");
  }
  strcat(sbcl_path, " --noinform --no-sysinit --no-userinit");
  return sbcl_path;
}

/*
 * return the new value for
 *     Linux: LD_LIBRARY_PATH
 *     macOS: DYLD_LIBRARY_PATH
 *     Windows: PATH
 */
const char *get_lib_env() {
  const char *lib_path = get_lib_path();
  char *path_separator = ":";
#ifdef __APPLE__
  char *ori_lib_env = getenv("DYLD_LIBRARY_PATH");
#elif defined __linux__
  char *ori_lib_env = getenv("LD_LIBRARY_PATH");
#elif defined _WIN32
  path_separator = ";";
  char *ori_lib_env = getenv("PATH");
#endif
  if (ori_lib_env == NULL) {
    ori_lib_env = "";
  }
  char *lib_env = malloc(
      (strlen(ori_lib_env) + strlen(lib_path) + strlen(path_separator) + 1) *
      sizeof(char));
  strcpy(lib_env, ori_lib_env);
  if (strlen(ori_lib_env) > 0) {
    strcat(lib_env, path_separator);
  }
  strcat(lib_env, lib_path);
  printf("LIB_ENV=%s\n", lib_env);
  return lib_env;
}

int exec_calm_script(int argc, char *argv[], int wait) {
  char entry_cmd[FILENAME_MAX];
  char script_path[FILENAME_MAX];
  char *cmd_ext;
  strcpy(script_path, "s");
  for (int i = 2; i < argc; i++) {
    strcat(script_path, "/");
    strcat(script_path, argv[i]);
    if (i == (argc - 1)) {
      cmd_ext = strrchr(argv[i], '.');
    }
  }

  if (cmd_ext == NULL) {
    printf("NO Extension, can't determine the executer %s\n", script_path);
    return 42;
  } else if (strcmp(cmd_ext, ".sh") == 0) {
    strcpy(entry_cmd, "sh ");
  } else if (strcmp(cmd_ext, ".bat") == 0) {
    strcpy(entry_cmd, "cmd /c ");
  } else if (strcmp(cmd_ext, ".ps1") == 0) {
    strcpy(entry_cmd, "powershell -ExecutionPolicy Bypass -F ");
  } else if (strcmp(cmd_ext, ".scpt") == 0) {
    strcpy(entry_cmd, "osascript ");
  } else if (strcmp(cmd_ext, ".lisp") == 0) {
    strcpy(entry_cmd, get_sbcl_path());
    strcat(entry_cmd, " --load calm.asd --eval '(ql:quickload :calm)' --load ");
  } else {
    printf("UNKNOWN SCRIPT: %s\n", script_path);
    return 42;
  }
  strcat(entry_cmd, script_path);

  if (wait == 0) {
    strcat(entry_cmd, " &");
  }

  printf("EXECUTING CALM SCRIPT: %s\n", entry_cmd);
  return system(entry_cmd) != 0 ? 42 : 0;
}

/*
 * show an alert, for the first time long loading process
 * only needed when running `calm` command
 */
void firstrun() {
  if (getenv("CI") == NULL && getenv("CALM_BUILDING") == NULL) {
    if (access(".calm-initialised", F_OK) != 0) {
      char *argv[] = {NULL, NULL, "usr", "os", "file"};
#ifdef __APPLE__
      argv[3] = "macos";
      argv[4] = "init-msg.scpt";
#endif
#ifdef __linux__
      argv[3] = "linux";
      argv[4] = "init-msg.sh";
#endif
#ifdef _WIN32
      argv[3] = "windows";
      argv[4] = "init-msg.ps1";
#endif
      exec_calm_script(5, argv, 0);
    }
  }
}

int main(int argc, char *argv[]) {
  char calm_cmd[1024];
  const char *lib_env = get_lib_env();

  if (argc >= 2) {
    strcpy(calm_cmd, argv[1]);
  } else {
    strcpy(calm_cmd, "show");
  }

  apply_env("CALM_CMD", calm_cmd);
  printf("CALM_CMD=%s\n", getenv("CALM_CMD"));

#ifdef _WIN32
  char backslash[] = "\\";
#else
  char slash[] = "/";
#endif

  /*
   * applying CALM_APP_DIR
   */

  if (getenv("CALM_APP_DIR") == NULL) {
    char cwd[FILENAME_MAX];
    getcwd(cwd, sizeof(cwd));
#ifdef _WIN32
    strcat(cwd, backslash);
#else
    strcat(cwd, slash);
#endif
    apply_env("CALM_APP_DIR", cwd);
    printf("CALM_APP_DIR=%s \n", getenv("CALM_APP_DIR"));
  } else {
    printf("inherited CALM_APP_DIR=%s \n", getenv("CALM_APP_DIR"));
  }

  /*
   * applying CALM_HOME
   */
  if (getenv("CALM_HOME") == NULL) {
    char binarydir[FILENAME_MAX];
    strcpy(binarydir, get_binary_dir());
#ifndef _WIN32
    strcat(binarydir, slash);
#endif
    apply_env("CALM_HOME", binarydir);
    printf("CALM_HOME=%s \n", getenv("CALM_HOME"));
  } else {
    printf("inherited CALM_HOME=%s \n", getenv("CALM_HOME"));
  }

  /*
   * applying SBCL_HOME
   */
#ifdef _WIN32
  char *sbcl_home_rel = "sbcl\\lib\\sbcl";
#else
  char *sbcl_home_rel = "/sbcl/lib/sbcl";
#endif
  char sbcl_home[FILENAME_MAX];
  strcpy(sbcl_home, get_binary_dir());
  strcat(sbcl_home, sbcl_home_rel);
  apply_env("SBCL_HOME", sbcl_home);
  printf("SBCL_HOME=%s \n", getenv("SBCL_HOME"));

  /*
   * cd to CALM_HOME
   */
  chdir(getenv("CALM_HOME"));

  /*
   * if it is not building CALM, and there is no calm.core
   * then show tips and build core
   */
  if (access("calm.asd", F_OK) == 0 && strcmp(calm_cmd, "core") != 0 &&
      getenv("CALM_BUILDING") == NULL) {
    firstrun();
    if (access("calm.core", F_OK) != 0) {
#ifdef _WIN32
      system("calm.exe core");
#else
      system("./calm core");
#endif
      if (access("calm.core", F_OK) != 0) {
        // sleep 2 seconds for calm.core to be written to the disk
        printf("waiting for calm.core to be written to the disk...\n");
#ifdef _WIN32
        Sleep(2000);
#else
        sleep(2);
#endif
      }
    }
  }

  /*
   * ==============
   * detecting SBCL path - START
   * ==============
   */

  char entry_cmd[FILENAME_MAX];

#ifdef __APPLE__
  if (getenv("CALM_BUILDING") != NULL) {  // Building, don't set lib env
    strcpy(entry_cmd, get_sbcl_path());
  } else {
    /*
     * Apple won't allow us to modify DYLD_LIBRARY_PATH:
     * https://developer.apple.com/forums/thread/13161
     * https://developer.apple.com/library/archive/documentation/Security/Conceptual/System_Integrity_Protection_Guide/RuntimeProtections/RuntimeProtections.html
     * So the following won't work:
     *     apply_env("DYLD_LIBRARY_PATH", lib_path);
     * We have to prepend this inside the command, like:
     *     DYLD_LIBRARY_PATH=/some/where/my/lib ./my-app
     */
    strcpy(entry_cmd, "DYLD_LIBRARY_PATH=");
    strcat(entry_cmd, lib_env);
    strcat(entry_cmd, " ");
    strcat(entry_cmd, get_sbcl_path());
  }
#elif defined _WIN32
  apply_env("PATH", lib_env);
  printf("PATH=%s \n", getenv("PATH"));
  strcpy(entry_cmd, get_sbcl_path());
#elif defined __linux__
  apply_env("LD_LIBRARY_PATH", lib_env);
  printf("LD_LIBRARY_PATH=%s \n", getenv("LD_LIBRARY_PATH"));
  strcpy(entry_cmd, get_sbcl_path());
#endif

  if (access("calm.asd", F_OK) == 0) {
    /*
     * ==============
     * Running CALM
     * ==============
     */

    if (strcmp(calm_cmd, "sbcl") == 0) {  // running CALM bundled SBCL
      for (int i = 2; i < argc; i++) {
        strcat(entry_cmd, " ");
        strcat(entry_cmd, argv[i]);
      }
    } else if (strcmp(calm_cmd, "s") == 0) {  // running CALM scripts
      return exec_calm_script(argc, argv, 1);
    } else {  // load CALM normally
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
    strcpy(entry_cmd, "DYLD_LIBRARY_PATH=");
    strcat(entry_cmd, lib_env);
    strcat(entry_cmd, " bin/calm-app");
    if (system(entry_cmd) != 0) return 42;
#else
    if (system("bin/calm-app") != 0) return 42;
#endif
  }

  return 0;
}
