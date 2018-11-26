#define _GNU_SOURCE
#include <sys/mman.h>
#include <sys/mount.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/utsname.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <pthread.h>
#include <sched.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

static int proc_id;
static int n_procs;

typedef struct {
    pthread_barrier_t barrier;
    int *ns_fds;
} shared_t;
shared_t *shared;

pid_t *pids;

int iters = 5;

void cleanup(){
    for (int i = 0; i < n_procs; ++i)
        if (pids[i])
            kill(pids[i], SIGINT);
    pthread_barrier_destroy(&shared->barrier);
    munmap((void *)shared, sizeof(shared_t));
    free(pids);

    char tmp_path[50];
    for (int i = 0; i < n_procs; ++i) {
        snprintf(tmp_path, 50, "/tmp/ns/uts%d", i);
        umount(tmp_path);
    }
}

void error(int proc_id, const char *msg){
    if (-1 == proc_id){
        fprintf(stderr, "[Parent] ");
    } else {
        fprintf(stderr, "[Child %d] ", proc_id);
    }
    perror(msg);    
    if (-1 == proc_id){
        cleanup();
    } else {
        kill(getppid(), SIGINT);
    }
    exit(EXIT_FAILURE);
}

void handler(int signum){
    error(-1, "Interrupted by child process");
}

int child(){
    srand(proc_id);
    // unshare to a new uts namespace
    if(-1 == unshare(CLONE_NEWUTS))
        error(proc_id, "unshare UTS namespace failed");

    printf("[Child %d] Entered new namespace\n", proc_id);

    // set hostname
    char hostname[10];
    snprintf(hostname, 10, "child_%d", proc_id);
    if(-1 == sethostname(hostname, strlen(hostname)))
        error(proc_id, "sethostname failed");

    struct utsname uts;
    if(-1 == uname(&uts))
        error(proc_id, "uname failed");
    printf("[Child %d] Current node name: %s\n", proc_id, uts.nodename);

    // wait twice for parent to start and finish bind mount
    pthread_barrier_wait(&shared->barrier);
    pthread_barrier_wait(&shared->barrier);
    
    // join random namespaces
    char tmp_path[50];
    for (int i = 0; i < iters; ++i){
        int target_ns = rand() % n_procs;
        snprintf(tmp_path, 50, "/tmp/ns/uts%d", target_ns);
        int fd = open(tmp_path, O_RDONLY);
        if (-1 == fd)
            error(proc_id, "open failed");
        if (-1 == setns(fd, 0))
            error(proc_id, "setns failed");
        if (-1 == uname(&uts))
            error(proc_id, "uname failed");
        printf("[Child %d] Round %d node name: %s\n", proc_id, i, uts.nodename);
        pthread_barrier_wait(&shared->barrier);
    }

    exit(EXIT_SUCCESS);
}

int main(int argc, char **argv){
    // parse cli option
    int opt;
    while ((opt = getopt(argc, argv, "c:")) != -1) {
        switch (opt) {
        case 'c':
            n_procs = atoi(optarg);
            break;
        default:
            fprintf(stderr, "Usage: %s [-c n_procs]\n",
                    argv[0]);
            exit(EXIT_FAILURE);
        }
    }

    printf("Running namespace switching demo with %d child processes\n", n_procs);

    shared = mmap(NULL, sizeof(pthread_barrier_t) + sizeof(int) * n_procs, PROT_READ|PROT_WRITE, MAP_ANONYMOUS|MAP_SHARED, -1, 0);

    pthread_barrierattr_t attr;
    pthread_barrierattr_init(&attr);
    pthread_barrierattr_setpshared(&attr, 1);
    pthread_barrier_init(&shared->barrier, &attr, n_procs+1);
    pthread_barrierattr_destroy(&attr);

    shared->ns_fds = (int*) (((char *)shared) + sizeof(pthread_barrier_t));

    pids = calloc(n_procs, sizeof(pid_t));

    while (proc_id < n_procs){
        // fork and run child
        pids[proc_id] = fork();
        if(pids[proc_id] < 0){
            cleanup();
            error(-1, "fork failed");
        }

        if(pids[proc_id] == 0)
            child(pids[proc_id]);

        ++proc_id;
    }

    // install signal handler for SIGINT
    struct sigaction sa;
    memset(&sa, 0, sizeof(struct sigaction));
    sa.sa_handler = handler;
    sigaction(SIGINT, &sa, NULL);

    // wait for all children to finish set up 
    pthread_barrier_wait(&shared->barrier);

    // bind mount their uts namespaces
    char ns_path[50], tmp_path[50];

    mkdir("/tmp/ns", 0644);
    
    for (int i = 0; i < n_procs; ++i) {
        snprintf(ns_path, 50, "/proc/%d/ns/uts", pids[i]);
        snprintf(tmp_path, 50, "/tmp/ns/uts%d", i);
        close(creat(tmp_path, 0644));
        if (-1 == mount(ns_path, tmp_path, NULL, MS_BIND, NULL))
            error(-1, "mount failed");
    }

    // bind mount done, run child processes 
    pthread_barrier_wait(&shared->barrier);

    while (iters){
        pthread_barrier_wait(&shared->barrier);
        iters--;
    }

    for (int i = 0; i < n_procs; ++i) {
        waitpid(pids[i], NULL, 0);
    }

    for (int i = 0; i < n_procs; ++i) {
        snprintf(tmp_path, 50, "/tmp/ns/uts%d", i);
        if (-1 == umount(tmp_path))
            error(-1, "umount failed");
    }

    return 0;
}