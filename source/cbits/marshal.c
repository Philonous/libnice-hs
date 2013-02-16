#include <stdlib.h>
#include <nice/agent.h>
#include <sys/socket.h>
#include <string.h>

#include "marshal.h"

struct sockaddr* get_candidate_addr (NiceCandidate* cand) {
  struct sockaddr* buf;
  buf = malloc(sizeof(struct sockaddr));
  nice_address_copy_to_sockaddr(&(cand->addr), buf);
  return buf;
}

struct sockaddr* get_candidate_base_addr (NiceCandidate* cand) {
  struct sockaddr* buf;
  buf = malloc(sizeof(struct sockaddr));
  nice_address_copy_to_sockaddr(&(cand->base_addr), buf);
  return buf;
}

void set_candidate_addr (NiceCandidate* cand, struct sockaddr* sin) {
  NiceAddress addr;
  nice_address_set_from_sockaddr(&addr, sin);
  cand->addr = addr;
}

void set_candidate_base_addr (NiceCandidate* cand, struct sockaddr* sin) {
  NiceAddress addr;
  nice_address_set_from_sockaddr(&addr, sin);
  cand->base_addr = addr;
}

void set_foundation(NiceCandidate* cand, char* found) {
  strncpy(cand->foundation, found, NICE_CANDIDATE_MAX_FOUNDATION);
}
