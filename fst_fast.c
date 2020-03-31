/**
 * FST with as much as possible precompiled
 * @author Jacob Salzberg (jssalzbe)
 * @file fst_fast.c
 */
#include <memory.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// This is basically a header.
// The formal definition is
// (Q, Sigma, Gamma, I, F, Delta)
// Or (States, InputAlphabet, OutputAlphabet, InitialStates, FinalStates,
// TransitionRelation)

// HOWEVER, fastest matching is translate this to
// a directed graph, and store the outgoing edges
// in each vertex

// Vertex definition would look like:
// Header
// # States
// # Initial States
// # Final States
// Body:
// Final states as vector of ints
// Vector of states beginning with the inital states
// Each state contains a 256 long vector of (short, char, char)
// AKA Out-state, junk data, out-character
// The position in the 256-long vector is the in - character
// and the state's index is the state #
// This limits the largets possible FST to 0.5 GB
// The state list will always begin with the initial states.

/**
 * Whether the transition is valid.
 * Only matters for non-determinsitic fsts
 */
#define FST_FLAG_VALID (1 << 0)

/**
 * Whether the fst state is initial
 */
#define FST_FLAG_INITIAL (1 << 1)

/**
 * Whether the fst state is final
 */
#define FST_FLAG_FINAL (1 << 2)

typedef union FstStateEntry FstStateEntry;

union FstStateEntry {
  struct FstStateEntryComponents {
    char flags;
    char outchar;
    unsigned short out_state;
  } components;

  int entry;
};

// Compile the FST a:a
// AKA:
// -> (0) -a:a-> ((1))
//    ||| !a:0    ||| !?:0
//    \ /         |||
//     -          |||
//    (2)-|||?:0  |||
//     -     |||  |||
//    / \    |||  |||
//    |||    |||  |||
//    ||======||  |||
//    |============||
void create_a_to_a(unsigned char *outbuff) {
  for (int i = 0; i < 256; i++) {
    FstStateEntry fse;
    if (i == 'a') {
      fse.components.flags = 0;
      fse.components.flags |= FST_FLAG_VALID;
      fse.components.flags |= FST_FLAG_INITIAL;
      fse.components.out_state = 1;
      fse.components.outchar = 'a';
    } else {
      fse.components.flags = 0;
      fse.components.flags |= FST_FLAG_VALID;
      fse.components.flags |= FST_FLAG_INITIAL;
      fse.components.out_state = 2;
      fse.components.outchar = 0;
    }

    *((FstStateEntry *) outbuff) = fse;
    outbuff += sizeof(FstStateEntry);
  }

  for (int i = 0; i < 256; i++) {
    FstStateEntry fse;
    fse.components.flags = 0;
    fse.components.flags |= FST_FLAG_VALID;
    fse.components.flags |= FST_FLAG_FINAL;
    fse.components.out_state = 2;
    fse.components.outchar = 0;
    *((FstStateEntry *) outbuff) = fse;
    outbuff += sizeof(FstStateEntry);
  }

  for (int i = 0; i < 256; i++) {
    FstStateEntry fse;
    fse.components.flags = 0;
    fse.components.flags |= FST_FLAG_VALID;
    fse.components.out_state = 2;
    fse.components.outchar = 0;
    *((FstStateEntry *) outbuff) = fse;
    outbuff += sizeof(FstStateEntry);
  }
}

/*
 * Using the PEGREG:
 * A <- aa
 * B <- a
 * K <- ab
 * (B/A)K
 * Translated to the FST:
 * Q (states):
 * 0: {q0, a0, b0}
 * 1: {a1, Fb, Kb0}
 * 2: {Kb1}
 * 3: {FKb}
 * 4: {}
 * Transitions:
 * 0 -a:a-> 1
 * 1 -a:a-> 2
 * 2 -b:b-> 3
 * 3 -?:0-> 4
 * 4 -?:0-> 4
 */
void create_pegreg_bak(unsigned char *outbuff) {
  for (int i = 0; i < 256; i++) {
    FstStateEntry fse;
    if (i == 'a') {
      fse.components.flags = 0;
      fse.components.flags |= FST_FLAG_VALID;
      fse.components.flags |= FST_FLAG_INITIAL;
      fse.components.out_state = 1;
      fse.components.outchar = 'a';
    } else {
      fse.components.flags = 0;
      fse.components.flags |= FST_FLAG_VALID;
      fse.components.flags |= FST_FLAG_INITIAL;
      fse.components.out_state = 4;
      fse.components.outchar = 0;
    }

    *((FstStateEntry *) outbuff) = fse;
    outbuff += sizeof(FstStateEntry);
  }

  for (int i = 0; i < 256; i++) {
    FstStateEntry fse;
    if (i == 'a') {
      fse.components.flags = 0;
      fse.components.flags |= FST_FLAG_VALID;
      fse.components.out_state = 2;
      fse.components.outchar = 'a';
    } else {
      fse.components.flags = 0;
      fse.components.flags |= FST_FLAG_VALID;
      fse.components.out_state = 4;
      fse.components.outchar = 0;
    }

    *((FstStateEntry *) outbuff) = fse;
    outbuff += sizeof(FstStateEntry);
  }

  for (int i = 0; i < 256; i++) {
    FstStateEntry fse;
    if (i == 'b') {
      fse.components.flags = 0;
      fse.components.flags |= FST_FLAG_VALID;
      fse.components.out_state = 3;
      fse.components.outchar = 'b';
    } else {
      fse.components.flags = 0;
      fse.components.flags |= FST_FLAG_VALID;
      fse.components.out_state = 4;
      fse.components.outchar = 0;
    }

    *((FstStateEntry *) outbuff) = fse;
    outbuff += sizeof(FstStateEntry);
  }

  for (int i = 0; i < 256; i++) {
    FstStateEntry fse;
    fse.components.flags = 0;
    fse.components.flags |= FST_FLAG_VALID;
    fse.components.flags |= FST_FLAG_FINAL;
    fse.components.out_state = 4;
    fse.components.outchar = 0;
    *((FstStateEntry *) outbuff) = fse;
    outbuff += sizeof(FstStateEntry);
  }

  for (int i = 0; i < 256; i++) {
    FstStateEntry fse;
    fse.components.flags = 0;
    fse.components.flags |= FST_FLAG_VALID;
    fse.components.out_state = 4;
    fse.components.outchar = 0;
    *((FstStateEntry *) outbuff) = fse;
    outbuff += sizeof(FstStateEntry);
  }
}

/*
 * Using the PEGREG:
 * A <- aa
 * B <- a
 * K <- ab
 * (A/B)K
 * Translated to the FST:
 * Q (states):
 * 0: {q0, a0, b0}
 * 1: {q1, b1, Fb, Kb0}
 * 2: {a1, Fa, Ka0, Kb1}
 * 3: {Ka1}
 * 4: {Ka2}
 * 5: {}
 * Transitions:
 * 0 -a:a-> 1
 * 1 -a:a-> 2
 * 2 -a:a-> 3
 * 3 -b:b-> 4
 * 4 -?:0-> 5
 */
void create_pegreg_abk(unsigned char *outbuff) {
  for (int i = 0; i < 256; i++) {
    FstStateEntry fse;
    if (i == 'a') {
      fse.components.flags = 0;
      fse.components.flags |= FST_FLAG_VALID;
      fse.components.flags |= FST_FLAG_INITIAL;
      fse.components.out_state = 1;
      fse.components.outchar = 'a';
    } else {
      fse.components.flags = 0;
      fse.components.flags |= FST_FLAG_VALID;
      fse.components.flags |= FST_FLAG_INITIAL;
      fse.components.out_state = 5;
      fse.components.outchar = 0;
    }

    *((FstStateEntry *) outbuff) = fse;
    outbuff += sizeof(FstStateEntry);
  }

  for (int i = 0; i < 256; i++) {
    FstStateEntry fse;
    if (i == 'a') {
      fse.components.flags = 0;
      fse.components.flags |= FST_FLAG_VALID;
      fse.components.out_state = 2;
      fse.components.outchar = 'a';
    } else {
      fse.components.flags = 0;
      fse.components.flags |= FST_FLAG_VALID;
      fse.components.out_state = 5;
      fse.components.outchar = 0;
    }

    *((FstStateEntry *) outbuff) = fse;
    outbuff += sizeof(FstStateEntry);
  }

  for (int i = 0; i < 256; i++) {
    FstStateEntry fse;
    if (i == 'a') {
      fse.components.flags = 0;
      fse.components.flags |= FST_FLAG_VALID;
      fse.components.out_state = 3;
      fse.components.outchar = 'a';
    } else {
      fse.components.flags = 0;
      fse.components.flags |= FST_FLAG_VALID;
      fse.components.out_state = 5;
      fse.components.outchar = 0;
    }

    *((FstStateEntry *) outbuff) = fse;
    outbuff += sizeof(FstStateEntry);
  }

  for (int i = 0; i < 256; i++) {
    FstStateEntry fse;
    if (i == 'b') {
      fse.components.flags = 0;
      fse.components.flags |= FST_FLAG_VALID;
      fse.components.out_state = 4;
      fse.components.outchar = 'b';
    } else {
      fse.components.flags = 0;
      fse.components.flags |= FST_FLAG_VALID;
      fse.components.out_state = 5;
      fse.components.outchar = 0;
    }

    *((FstStateEntry *) outbuff) = fse;
    outbuff += sizeof(FstStateEntry);
  }

  for (int i = 0; i < 256; i++) {
    FstStateEntry fse;
    fse.components.flags = 0;
    fse.components.flags |= FST_FLAG_VALID;
    fse.components.flags |= FST_FLAG_FINAL;
    fse.components.out_state = 5;
    fse.components.outchar = 0;
    *((FstStateEntry *) outbuff) = fse;
    outbuff += sizeof(FstStateEntry);
  }

  for (int i = 0; i < 256; i++) {
    FstStateEntry fse;
    fse.components.flags = 0;
    fse.components.flags |= FST_FLAG_VALID;
    fse.components.out_state = 5;
    fse.components.outchar = 0;
    *((FstStateEntry *) outbuff) = fse;
    outbuff += sizeof(FstStateEntry);
  }
}

/*
 * And now one where the path taken changes which capture is made
 * A <- aa
 * B <- ab
 * K <- x
 * (A/B)K
 * States
 * 0: {q0, a0, b0}
 * 1: {a1, b1}
 * 2: {Fa, Ka0}
 * 3: {Fka}
 * 4: {Fb, Kb0}
 * 5: {Fkb}
 * 6: {}
 * Transitions
 * 0 -a:a-> 1
 * 1 -a:a-> 2
 * 1 -b:b-> 4
 * 2 -x:x-> 3
 * 3 -?:0-> 6
 * 4 -x:x-> 5
 * 5 -?:0-> 6
 * 6 -?:0-> 6
 */
void create_pegreg_diffmatch(unsigned char *outbuff) {
  for (int i = 0; i < 256; i++) {
    FstStateEntry fse;
    if (i == 'a') {
      fse.components.flags = 0;
      fse.components.flags |= FST_FLAG_VALID;
      fse.components.flags |= FST_FLAG_INITIAL;
      fse.components.out_state = 1;
      fse.components.outchar = 'a';
    } else {
      fse.components.flags = 0;
      fse.components.flags |= FST_FLAG_VALID;
      fse.components.flags |= FST_FLAG_INITIAL;
      fse.components.out_state = 6;
      fse.components.outchar = 0;
    }

    *((FstStateEntry *) outbuff) = fse;
    outbuff += sizeof(FstStateEntry);
  }

  for (int i = 0; i < 256; i++) {
    FstStateEntry fse;
    if (i == 'a') {
      fse.components.flags = 0;
      fse.components.flags |= FST_FLAG_VALID;
      fse.components.out_state = 2;
      fse.components.outchar = 'a';
    } else if (i == 'b') {
      fse.components.flags = 0;
      fse.components.flags |= FST_FLAG_VALID;
      fse.components.out_state = 4;
      fse.components.outchar = 'b';
    } else {
      fse.components.flags = 0;
      fse.components.flags |= FST_FLAG_VALID;
      fse.components.out_state = 6;
      fse.components.outchar = 0;
    }

    *((FstStateEntry *) outbuff) = fse;
    outbuff += sizeof(FstStateEntry);
  }

  for (int i = 0; i < 256; i++) {
    FstStateEntry fse;
    if (i == 'x') {
      fse.components.flags = 0;
      fse.components.flags |= FST_FLAG_VALID;
      fse.components.out_state = 3;
      fse.components.outchar = 'x';
    } else {
      fse.components.flags = 0;
      fse.components.flags |= FST_FLAG_VALID;
      fse.components.out_state = 6;
      fse.components.outchar = 0;
    }

    *((FstStateEntry *) outbuff) = fse;
    outbuff += sizeof(FstStateEntry);
  }

  for (int i = 0; i < 256; i++) {
    FstStateEntry fse;
    fse.components.flags = 0;
    fse.components.flags |= FST_FLAG_VALID;
    fse.components.flags |= FST_FLAG_FINAL;
    fse.components.out_state = 6;
    fse.components.outchar = 0;
    *((FstStateEntry *) outbuff) = fse;
    outbuff += sizeof(FstStateEntry);
  }

  for (int i = 0; i < 256; i++) {
    FstStateEntry fse;
    if (i == 'x') {
      fse.components.flags = 0;
      fse.components.flags |= FST_FLAG_VALID;
      fse.components.out_state = 5;
      fse.components.outchar = 'x';
    } else {
      fse.components.flags = 0;
      fse.components.flags |= FST_FLAG_VALID;
      fse.components.out_state = 6;
      fse.components.outchar = 0;
    }

    *((FstStateEntry *) outbuff) = fse;
    outbuff += sizeof(FstStateEntry);
  }

  for (int i = 0; i < 256; i++) {
    FstStateEntry fse;
    fse.components.flags = 0;
    fse.components.flags |= FST_FLAG_VALID;
    fse.components.flags |= FST_FLAG_FINAL;
    fse.components.out_state = 6;
    fse.components.outchar = 0;
    *((FstStateEntry *) outbuff) = fse;
    outbuff += sizeof(FstStateEntry);
  }

  for (int i = 0; i < 256; i++) {
    FstStateEntry fse;
    fse.components.flags = 0;
    fse.components.flags |= FST_FLAG_VALID;
    fse.components.out_state = 6;
    fse.components.outchar = 0;
    *((FstStateEntry *) outbuff) = fse;
    outbuff += sizeof(FstStateEntry);
  }
}

/**
 * Match a single character on a single state.
 * The FST must be deterministic.
 * @param input the input character
 * @param state_number the number that labels the current state
 * @param states the vector of states
 * @param current_state_start the start of the 256 entries that define the
 * current state
 * @param next_state stores the pointer to the state after this is matched
 */
void match_one_char(char input, char *output, int *state_number,
                    FstStateEntry *states, FstStateEntry *current_state_start,
                    FstStateEntry **next_state) {
  unsigned char uinput = input;
  *output = current_state_start[uinput].components.outchar;
  *state_number = current_state_start[uinput].components.out_state;
  *next_state = states + (*state_number) * 256;
}

/**
 * Returns a malloced (!) string,
 * and sets matched_states to a malloced (!)
 * array of shorts corresponding to what
 * was matched by what state.
 */
char *match_string(char *input, unsigned char *instrbuff, int *match_success,
                   unsigned short **matched_states) {
  /* Output vector */
  char *retval = malloc(sizeof(char) * 10);
  char *string_end = retval;
  int retval_size = 0;
  int retval_capacity = 10;

  /* Matched states */
  /* Size and capacity piggyback off of that of output vector */
  *matched_states = malloc(sizeof(unsigned short) * 10);
  unsigned short *matched_states_end = *matched_states;

  int state_number = 0;
  FstStateEntry *states = (FstStateEntry *) instrbuff;
  FstStateEntry *current_state_start = (FstStateEntry *) instrbuff;
  while (*input) {
    char output;
    match_one_char(*input, &output, &state_number, states, current_state_start,
                   &current_state_start);
    input += 1;
    if (output) {
      if (retval_size == retval_capacity) {
        retval_capacity *= 2;
        retval = realloc(retval, retval_capacity);
        if (!retval) {
          perror("Failed to allocate memory");
          exit(1);
        }
        matched_states = realloc(matched_states, retval_capacity);
      }
      *string_end = output;
      string_end += 1;
      *matched_states_end = state_number;
      matched_states_end += 1;
      retval_size += 1;
    }
  }

  if (retval_size == retval_capacity) {
    retval_capacity *= 2;
    retval = realloc(retval, retval_capacity);
    if (!retval) {
      perror("Failed to allocate memory");
      exit(1);
    }
  }

  *string_end = '\0';
  *matched_states_end = state_number;

  *match_success = !!(current_state_start->components.flags & FST_FLAG_FINAL);
  return retval;
}

/**
 * The starting point of the program
 * @param argc the number of arguments
 * @param argv the argument array
 */
int main(int argc, char **argv) {

  /* Success cases */

  {
    printf("Testing: FST a:a\n");
    unsigned char *instrbuff = malloc(3 * 256 * sizeof(FstStateEntry));
    create_a_to_a(instrbuff);

    unsigned short *matched_states;
    int match_success = 0;
    char *outstr =
        match_string("a", instrbuff, &match_success, &matched_states);

    printf("outstr is: \"%s\"\n", outstr);
    printf("Match success is: %d\n", match_success);

    char *outstrcurr = outstr;
    int i = 0;
    while (*outstrcurr) {
      printf("\"%c\" matched with state %d\n", *outstrcurr, matched_states[i]);
      i += 1;
      outstrcurr += 1;
    }

    free(outstr);
    free(matched_states);
    free(instrbuff);
    printf("Done.\n");
  }

  {
    printf("Testing: PEGREG (B/A)K against \"aab\"\n");
    unsigned char *instrbuff = malloc(5 * 256 * sizeof(FstStateEntry));
    create_pegreg_bak(instrbuff);
    unsigned short *matched_states;
    int match_success = 0;
    char *outstr =
        match_string("aab", instrbuff, &match_success, &matched_states);

    printf("outstr is: \"%s\"\n", outstr);
    printf("Match success is: %d\n", match_success);

    char *outstrcurr = outstr;
    int i = 0;
    while (*outstrcurr) {
      printf("\"%c\" matched with state %d\n", *outstrcurr, matched_states[i]);
      i += 1;
      outstrcurr += 1;
    }

    free(outstr);
    free(matched_states);
    free(instrbuff);
    printf("Done.\n");
  }

  {
    printf("Testing: PEGREG (A/B)K against \"aaab\"\n");
    unsigned char *instrbuff = malloc(6 * 256 * sizeof(FstStateEntry));
    create_pegreg_abk(instrbuff);
    unsigned short *matched_states;
    int match_success = 0;
    char *outstr =
        match_string("aaab", instrbuff, &match_success, &matched_states);

    printf("outstr is: \"%s\"\n", outstr);
    printf("Match success is: %d\n", match_success);

    char *outstrcurr = outstr;
    int i = 0;
    while (*outstrcurr) {
      printf("\"%c\" matched with state %d\n", *outstrcurr, matched_states[i]);
      i += 1;
      outstrcurr += 1;
    }

    free(outstr);
    free(matched_states);
    free(instrbuff);
    printf("Done.\n");
  }

  {
    printf("Testing: PEGREG (aa/bb)x against \"aax\"\n");
    unsigned char *instrbuff = malloc(7 * 256 * sizeof(FstStateEntry));
    create_pegreg_diffmatch(instrbuff);
    unsigned short *matched_states;
    int match_success = 0;
    char *outstr =
        match_string("aax", instrbuff, &match_success, &matched_states);

    printf("outstr is: \"%s\"\n", outstr);
    printf("Match success is: %d\n", match_success);

    char *outstrcurr = outstr;
    int i = 0;
    while (*outstrcurr) {
      printf("\"%c\" matched with state %d\n", *outstrcurr, matched_states[i]);
      i += 1;
      outstrcurr += 1;
    }

    free(outstr);
    free(matched_states);
    free(instrbuff);
    printf("Done.\n");
  }

  /* Failure cases */
  {
    printf("Testing: FST a:a against \"ab\"\n");
    unsigned char *instrbuff = malloc(3 * 256 * sizeof(FstStateEntry));
    create_a_to_a(instrbuff);

    unsigned short *matched_states;
    int match_success = 0;
    char *outstr =
        match_string("ab", instrbuff, &match_success, &matched_states);

    printf("outstr is: \"%s\"\n", outstr);
    printf("Match success is: %d\n", match_success);

    char *outstrcurr = outstr;
    int i = 0;
    while (*outstrcurr) {
      printf("\"%c\" matched with state %d\n", *outstrcurr, matched_states[i]);
      i += 1;
      outstrcurr += 1;
    }

    free(outstr);
    free(matched_states);
    free(instrbuff);
    printf("Done.\n");
  }

  {
    printf("Testing: PEGREG (B/A)K against \"aaab\"\n");
    unsigned char *instrbuff = malloc(5 * 256 * sizeof(FstStateEntry));
    create_pegreg_bak(instrbuff);
    unsigned short *matched_states;
    int match_success = 0;
    char *outstr =
        match_string("aaab", instrbuff, &match_success, &matched_states);

    printf("outstr is: \"%s\"\n", outstr);
    printf("Match success is: %d\n", match_success);

    char *outstrcurr = outstr;
    int i = 0;
    while (*outstrcurr) {
      printf("\"%c\" matched with state %d\n", *outstrcurr, matched_states[i]);
      i += 1;
      outstrcurr += 1;
    }

    free(outstr);
    free(matched_states);
    free(instrbuff);
    printf("Done.\n");
  }

  {
    printf("Testing: PEGREG (A/B)K against \"aab\"\n");
    unsigned char *instrbuff = malloc(6 * 256 * sizeof(FstStateEntry));
    create_pegreg_abk(instrbuff);
    unsigned short *matched_states;
    int match_success = 0;
    char *outstr =
        match_string("aab", instrbuff, &match_success, &matched_states);

    printf("outstr is: \"%s\"\n", outstr);
    printf("Match success is: %d\n", match_success);

    char *outstrcurr = outstr;
    int i = 0;
    while (*outstrcurr) {
      printf("\"%c\" matched with state %d\n", *outstrcurr, matched_states[i]);
      i += 1;
      outstrcurr += 1;
    }

    free(outstr);
    free(matched_states);
    free(instrbuff);
  }
}
