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

typedef struct Fst Fst;

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

typedef struct Fst Fst;

struct Fst {
  int n_states;
  int inital_states;
  int final_states;
};

typedef union FstStateEntry FstStateEntry;

union FstStateEntry {
  short out_state;
  char junk;
  char outchar;
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
  Fst *fst = (Fst *) outbuff;
  fst->n_states = 3;
  fst->inital_states = 1;
  fst->final_states = 1;
  outbuff += sizeof(Fst);

  int *final_states = (int *) outbuff;
  *final_states = 1;
  outbuff += sizeof(int);

  for (int i = 0; i < 256; i++) {
    FstStateEntry fse;
    if (i == 'a') {
      fse.out_state = 1;
      fse.junk = 0;
      fse.outchar = 'a';
    } else {
      fse.out_state = 2;
      fse.junk = 0;
      fse.outchar = 0;
    }

    *((FstStateEntry *) outbuff) = fse;
  }

  for (int i = 0; i < 256; i++) {
    FstStateEntry fse;
    fse.out_state = 2;
    fse.junk = 0;
    fse.outchar = 0;
    *((FstStateEntry *) outbuff) = fse;
  }

  for (int i = 0; i < 256; i++) {
    FstStateEntry fse;
    fse.out_state = 2;
    fse.junk = 0;
    fse.outchar = 0;
    *((FstStateEntry *) outbuff) = fse;
  }
}

/**
 * The starting point of the program
 * @param argc the number of arguments
 * @param argv the argument array
 */
int main(int argc, char **argv) {
}
