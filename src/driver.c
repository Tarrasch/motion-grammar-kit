#include <stdlib.h>

typedef struct observation {
  // Just an example, codegen will generate this
  int field1;
  char field2;
} observation_t;

typedef bool_t int;
typedef bool_t (*predicate_t)(observation_t);
typedef terminal_t int;
typedef nonterminal_t int;
typedef symbol_t int; // terminal_t or nonterminal_t
typedef void (*mutator_t) ();

enum symbol_type_t = {
  NONTERMINAL, // Simply a nonterminal
  PREDICATE,   // A predicate, should been defined before code generation (in the lisp program)
  MUTATOR      // A side effect, you're expected to write one c function for each type of mutator
};

#define MAX_PRODUCTION_LENGTH 50

typedef struct production {
  int num_symbols;
  void *data[MAX_PRODUCTION_LENGTH];
  symbol_type_t types[MAX_PRODUCTION_LENGTH];
} production_t;

#define MAX_PRODUCTIONS 50

typedef struct nonterminal {
  int num_productions;
  production_t *productions[MAX_PRODUCTIONS]; // We don't need this probably
  dfa_t * automata;
} nonterminal_t;

#define STORE_SIZE 2000
#define INC_MOD_SS(var) var = (var+1)%STORE_SIZE;

Observation store[STORE_SIZE];

int curr_beg; // From where we know our observations;
int curr_end; // Position of the end of store

extern observation_t make_observation();

observation_t fetch_observation() {
  if (curr_pos == (curr_end+1)%STORE_SIZE) {
    exit(1); // We ran out of memory :(
  } else {
    observation_t ret = store[curr_end] = make_observation();
    curr_end = (curr_end+1)%STORE_SIZE;
    return ret;
  }
}

observation_t get_observation(int pos) {
  if (pos == curr_end) {
    return fetch_observation();
  } else {
    // assuming it's in range here, namely "pos < curr_end" (but in modulo and such)
    return store[pos];
  }
}

#define MAX_NODES 100
#define MAX_EDGES 500

typedef struct dfa {
  // We assume node 0 is start node
  int num_nodes;
  production_t *final[MAX_NODES]; // -1 if not final, otherwise production id
  int range[MAX_NODES+1]; // the edges for node n are in the span [range[n], range[n+1])
  predicate_t predicates[MAX_EDGES]; // predicate function, if it turns true then take this edge. check these in order!
  int destinations[MAX_EDGES]; // by following edge j you end up at destinations[j]

  // // CONSTRAINT, for convenience, No edge may be edgeless UNLESS it's a final edge (when final[i] != NULL)
} dfa_t;

production_t *run_dfa(dfa_t *automata) {
  int curr_node = 0;
  while (1) {
    if(automata->final[curr_node]) {
      return automata->final[curr_node];
    }
    int beg = range[curr_node], end = range[curr_node+1];
    observation_t obs = get_observation(curr_beg);
    int happened = 0; // Just for sanity check
    for(int i = beg; i < end; i++) {
      if(automata->predicates[i](obs)) { // You can actually skip the last for efficiency but whatever
        curr_node = destinations[i];
        happened = 1;
        break;
      }
    }
    assert(happened); // Otherwise our lisp-code is buggy
  }
}

void run_nonterminal(nonterminal_t *nt) {
beginning:
  production_t *prod = run_dfa(&(nt->automata));
  for(int i = 0; i < prod->num_symbols; i++ ) {
    switch( prod->types[i] ) {
      case NONTERMINAL:
        if(i == prod->num_symbols-1) {
          nt = (nonterminal_t*) prod->data[i];
          goto beginning;
        }
        else {
          run_nonterminal((nonterminal_t*) prod->data[i]);
        }
        break;
      case PREDICATE:
        if(curr_beg == curr_end) {
          fetch_observation(); // We don't care if the observation is correct, we assume it is!
          assert(store[curr_beg]); // Well we can double check I guess :)
        }
        curr_beg = (curr_beg+1)%STORE_SIZE;
        break;
      case MUTATOR:
        if(curr_beg == curr_end) {
          fetch_observation();
          ((mutator_t) prod->data[i])(); // Otherwise the dfa have already fired this guy
        }
        curr_beg = (curr_beg+1)%STORE_SIZE;
        break;
    }
  }
}
