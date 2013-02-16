#include <nice/agent.h>
#include <sys/socket.h>

struct sockaddr* get_candidate_addr (NiceCandidate* cand);
struct sockaddr* get_candidate_base_addr (NiceCandidate* cand);
void set_candidate_addr (NiceCandidate* cand, struct sockaddr* sin);
void set_candidate_base_addr (NiceCandidate* cand, struct sockaddr* sin);
void set_foundation(NiceCandidate* cand, char* found);
