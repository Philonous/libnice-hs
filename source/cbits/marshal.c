#include <stdlib.h>
#include <nice/agent.h>
#include <sys/socket.h>
#include <string.h>

#include "marshal.h"

#include <stdio.h>

void set_foundation(NiceCandidate* cand, char* found) {
  strncpy(cand->foundation, found, NICE_CANDIDATE_MAX_FOUNDATION);
}
