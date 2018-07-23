#include <stdbool.h>

typedef struct BaseCommand BaseCommand;
typedef struct RunnableCommand RunnableCommand;
typedef struct CreateCommand CreateCommand;

struct BaseCommand {
  char* statePath;
  char* criu;
  bool systemdCgroup;
  int rootless; // 0: auto, 1: true, 2: false
};

struct RunnableCommand {
  BaseCommand* base;
  char* id;
  bool noPivot;
  bool noNewKeyring;
  char* notifySocket;
  int listenFds;
};

struct CreateCommand {
  RunnableCommand* runnable;
  char* bundle;
  char* consoleSocket;
  char* pidFile;
  int preserveFds;
};
