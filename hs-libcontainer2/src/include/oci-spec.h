// Source: https://github.com/opencontainers/runtime-spec/blob/master/specs-go/config.go

#include <stdbool.h>

// typedef char* String;
//
// typedef struct {
// 	String version;
// 	Process* process;
// 	Root* root;
// 	String hostname;
// 	Mount* mounts;
// 	Hooks* hooks;
// 	GHashTable* annotations;
//
// 	Linux* linux;
// 	Solaris* solaris;
// 	WIndows* windows;
// 	VM* vm;
// } Spec;
//
// typedef struct {
// 	bool terminal;
// 	Box* consoleSize;
// 	User* user;
// 	String* args;
// 	String* env;
// 	String cwd;
// 	LinuxCapabilities* capabilities;
// 	POSIXRlimit* rlimits;
// 	bool noNewPrivileges;
// 	String apparmorProfile;
// 	int* OOMScoreAdj;
// 	String selinuxLabel;
// } Process;
//
// typedef struct {
// 	String* bounding;
// 	String* effective;
// 	String* inheritable;
// 	String* permitted;
// 	String* ambient;
// } LinuxCapabilities;
//
// typedef struct {
// 	unsigned int height;
// 	unsigned int width;
// } Box;
//
// typedef struct {
// 	uint32_t uid;
// 	uint32_t gid;
// 	uint32_t* additionalGids;
// 	String username;
// } User;
//
// typedef struct {
// 	String path;
// 	bool readOnly;
// } Root;
//
// typedef struct {
// 	String destination;
// 	String type;
// 	String source;
// 	String* options;
// } Mount;
//
// typedef struct {
// 	Hook* prestart;
// 	Hook* poststart;
// 	Hook* poststop;
// } Hooks;
//
// typedef struct {
// 	LinuxIDMapping* uidMapping;
// 	LinuxIDMapping* gidMapping;
// 	GHashTable* sysctl;
// 	LinuxResources* resources;
// 	String cgroupsPath;
// 	LinuxNamespace* namespaces;
// 	LinuxDevice* devices;
// 	LinuxSeccomp* seccomp
// } Linux;

typedef struct {
	char* l3CacheSchema;
} LinuxIntelRdt;

typedef struct {
	LinuxIntelRdt* linuxIntelRdt;
} Test;

// LinuxIntelRdt has container runtime resource constraints
// for Intel RDT/CAT introduced in Linux 4.10 kernel
