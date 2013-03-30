#include <nice/agent.h>
#include <sys/socket.h>

typedef struct sockaddr_storage sa_storage;

void set_foundation(NiceCandidate* cand, char* found);
int copy_to_sockaddr_check(NiceAddress* addr, struct sockaddr* sa);
