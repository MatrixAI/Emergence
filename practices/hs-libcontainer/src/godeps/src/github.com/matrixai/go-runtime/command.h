typedef struct BaseCommand BaseCommand;
typedef struct RunnableCommand RunnableCommand;
typedef struct CreateCommand CreateCommand;

typedef enum {false, true} bool;

struct BaseCommand {
  char* statePath;
  char* criu;
  bool systemdCgroup;
  bool* rootless;
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
