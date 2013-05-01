/*************************/
/* LL-STAR MOTION PARSER */
/*************************/
typedef struct observation {
    int glass_full;
    int coke_empty;
    int found;
} observation_t;
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>

#include "bundle.c"


// Motion parser written in C for LL(*)

typedef int bool_t;
typedef bool_t (*predicate_t)(observation_t);
typedef int terminal_t;
typedef int symbol_t; // terminal_t or nonterminal_t
typedef void (*mutator_t) ();
typedef void* untyped_t;

typedef enum {
  NONTERMINAL, // Simply a nonterminal
  PREDICATE,   // A predicate, should been defined before code generation (in the lisp program)
  MUTATOR,     // A side effect, you're expected to write one c function for each type of mutator
  KLEENE       // Like predicate, but "stay where you are" if true
} symbol_type_t ;

#define MAX_PRODUCTION_LENGTH 50
#define MAX_PRODUCTIONS 50
#define STORE_SIZE 2000
#define MAX_NODES 100
#define MAX_EDGES 500

typedef struct dfa dfa_t;
typedef struct production production_t;
typedef struct nonterminal nonterminal_t;

typedef struct production {
  int num_symbols;
  untyped_t data[MAX_PRODUCTION_LENGTH]; // Either (nonterminal_t, predicate_t, mutator_t)
  symbol_type_t types[MAX_PRODUCTION_LENGTH];
} production_t;

typedef struct nonterminal {
  //int num_productions;
  //production_t *productions[MAX_PRODUCTIONS]; // We don't need this probably
  dfa_t *automata;
} nonterminal_t;

observation_t store[STORE_SIZE];

int curr_beg; // From where we know our observations;
int curr_end; // Position of the end of store

bool_t queue_is_empty () {
  return curr_beg == curr_end;
}

extern observation_t make_observation();

observation_t fetch_observation() {
  assert(curr_beg != (curr_end+1)%STORE_SIZE && "Ran out of memory, queue size is too small");
  observation_t ret = store[curr_end] = make_observation();
  curr_end = (curr_end+1)%STORE_SIZE;
  return ret;
}

observation_t get_observation(int pos, bool_t *is_fetched_observation) {
  if (pos == curr_end) {
    if (is_fetched_observation != NULL) *is_fetched_observation = 1;
    return fetch_observation();
  } else {
    // assuming it's in range here, namely "pos < curr_end" (but in modulo and such)
    if (is_fetched_observation != NULL) *is_fetched_observation = 0;
    return store[pos];
  }
}

/// handle_mutator: Perform the action if it's the first time we observe it
void handle_mutator(mutator_t mutator, bool_t is_fetched_observation) {
  if(is_fetched_observation) {
    mutator();
    draw_game();
    sleep(1);
  }
}

typedef struct dfa {
  int num_nodes;
  int start_node;
  production_t *final[MAX_NODES]; // If null then not a accept state. Otherwise predict to given production
  int range[MAX_NODES+1]; // the edges for node n are in the span [range[n], range[n+1])
  untyped_t edges[MAX_EDGES]; // predicate ==> if it turns true then take this edge. mutator ==> perform if new and always take edge
  symbol_type_t types[MAX_EDGES]; // The type of the edge. (Should only be mutator or predicate)
  int destinations[MAX_EDGES]; // by following edge j you end up at destinations[j]
} dfa_t;

production_t *run_dfa(dfa_t *automata) {
  int pred_pos = curr_beg;
  int curr_node = automata->start_node;
  while (1) {
    if(automata->final[curr_node]) {
      return automata->final[curr_node];
    }
    bool_t is_fetched_observation;
    observation_t obs = get_observation(pred_pos, &is_fetched_observation);
    int beg = automata->range[curr_node], end = automata->range[curr_node+1];
    int happened = 0; // Just for sanity check
    for(int i = beg; i < end; i++) {
      switch( automata->types[i] ) {
        case MUTATOR: {
          mutator_t mutator = (mutator_t) automata->edges[i];
          handle_mutator(mutator, is_fetched_observation);
          assert((beg+1 == end) && "We only expect one edge if there's a mutator");
          happened = 1;
                      }
          break;
        case PREDICATE: {
          predicate_t pred = (predicate_t) automata->edges[i];
          happened = pred(obs);
                        }
          break;
        default: assert(0 && "Invalid type");
      }
      if (happened) {
        pred_pos = (pred_pos+1)%STORE_SIZE;
        curr_node = automata->destinations[i];
        break;
      }
    }
    assert(happened); // Syntax error!
  }
}

void run_nonterminal(nonterminal_t *nt) {
  production_t *prod;
beginning:
  prod = run_dfa(nt->automata);
  for(int i = 0; i < prod->num_symbols; i++ ) {
    switch( prod->types[i] ) {
      case NONTERMINAL:
        if(i == prod->num_symbols-1) {
          nt = (nonterminal_t*) prod->data[i];
          goto beginning; // TCO
        }
        else {
          run_nonterminal((nonterminal_t*) prod->data[i]);
        }
        break;
      case KLEENE: {
        predicate_t pred = (predicate_t) prod->data[i];
        observation_t obs = get_observation(curr_beg, NULL);
        if(pred(obs)) {
          i--;
        }
        curr_beg = (curr_beg+1)%STORE_SIZE;
                   }
        break;
      case PREDICATE: {
        predicate_t pred = (predicate_t) prod->data[i];
        observation_t obs = get_observation(curr_beg, NULL);
        assert(pred(obs) && "Syntax error OR bug in dfa/prediction");
        curr_beg = (curr_beg+1)%STORE_SIZE;
                      }
        break;
      case MUTATOR: {
        mutator_t mutator = (mutator_t) prod->data[i];
        bool_t is_fetched_observation;
        get_observation(curr_beg, &is_fetched_observation);
        handle_mutator(mutator, is_fetched_observation);
        curr_beg = (curr_beg+1)%STORE_SIZE;
                    }
        break;
    }
  }
}

void mutator_serve_glass () {
  // XXX: STUB! ADD YOUR IMPLEMENTATION HERE!
  serve_glass();
}
void mutator_lift () {
  // XXX: STUB! ADD YOUR IMPLEMENTATION HERE!
  is_lifted[robot_pos] = 1;
}
void mutator_put_down () {
  // XXX: STUB! ADD YOUR IMPLEMENTATION HERE!
  is_lifted[robot_pos] = 0;
}
void mutator_pour () {
  // XXX: STUB! ADD YOUR IMPLEMENTATION HERE!
  pour_from(robot_pos);
}
void mutator_discard_can () {
  // XXX: STUB! ADD YOUR IMPLEMENTATION HERE!
  coke_recycle(robot_pos);
}
void mutator_next_line () {
  // XXX: STUB! ADD YOUR IMPLEMENTATION HERE!
  robot_pos = (robot_pos+1)%NUM_COKES;
}
observation_t make_observation () {
  // XXX: STUB! ADD YOUR IMPLEMENTATION HERE!
  observation_t obs = {
    .glass_full = glass_is_full(robot_pos),
    .coke_empty = coke_is_empty(robot_pos),
    .found = cokes[robot_pos] >= 0
  };
  return obs;
}
bool_t predicate_glass_full (observation_t obs) {
return obs.glass_full;
}
bool_t predicate__lp_not_sp_coke_empty_rp_ (observation_t obs) {
return !(obs.coke_empty);
}
bool_t predicate__lp_and_sp__lp_not_sp_glass_full_rp__sp_coke_empty_rp_ (observation_t obs) {
return (!(obs.glass_full)) && (obs.coke_empty);
}
bool_t predicate__lp_and_sp__lp_not_sp_glass_full_rp__sp__lp_not_sp_coke_empty_rp__rp_ (observation_t obs) {
return (!(obs.glass_full)) && (!(obs.coke_empty));
}
bool_t predicate__lp_not_sp_found_rp_ (observation_t obs) {
return !(obs.found);
}
bool_t predicate_coke_empty (observation_t obs) {
return obs.coke_empty;
}
bool_t predicate__lp_and_sp__lp_and_sp__lp_and_sp__lp_not_sp_glass_full_rp__sp_coke_empty_rp__sp__lp_or_sp_glass_full_sp_coke_empty_rp__rp__sp__lp_not_sp_glass_full_rp__rp_ (observation_t obs) {
return (((!(obs.glass_full)) && (obs.coke_empty)) && ((obs.glass_full) || (obs.coke_empty))) && (!(obs.glass_full));
}
bool_t predicate__lp_and_sp__lp_and_sp__lp_not_sp__lp_and_sp__lp_not_sp_glass_full_rp__sp_coke_empty_rp__rp__sp__lp_and_sp__lp_not_sp_glass_full_rp__sp__lp_not_sp_coke_empty_rp__rp__rp__sp__lp_not_sp_glass_full_rp__rp_ (observation_t obs) {
return (((obs.glass_full) || (!(obs.coke_empty))) && ((!(obs.glass_full)) && (!(obs.coke_empty)))) && (!(obs.glass_full));
}
bool_t predicate__lp_and_sp__lp_and_sp__lp_not_sp__lp_and_sp__lp_not_sp_glass_full_rp__sp_coke_empty_rp__rp__sp__lp_or_sp_glass_full_sp_coke_empty_rp__rp__sp_glass_full_rp_ (observation_t obs) {
return (((obs.glass_full) || (!(obs.coke_empty))) && ((obs.glass_full) || (obs.coke_empty))) && (obs.glass_full);
}
bool_t predicate__lp_and_sp__lp_not_sp_coke_empty_rp__sp__lp_not_sp_coke_empty_rp__rp_ (observation_t obs) {
return (!(obs.coke_empty)) && (!(obs.coke_empty));
}
bool_t predicate_found (observation_t obs) {
return obs.found;
}
bool_t predicate__lp_and_sp__lp_not_sp_found_rp__sp__lp_not_sp_found_rp__rp_ (observation_t obs) {
return (!(obs.found)) && (!(obs.found));
}
extern nonterminal_t nonterminal_keep_pouring;
extern nonterminal_t nonterminal_serve_coke;
extern nonterminal_t nonterminal_life;
production_t production_0 = { 2, { (untyped_t)&nonterminal_serve_coke, (untyped_t)&nonterminal_life }, { NONTERMINAL, NONTERMINAL } };// Belonging to nonterminal LIFE
production_t production_1 = { 7, { (untyped_t)&predicate_found, (untyped_t)&predicate__lp_not_sp_coke_empty_rp_, (untyped_t)&mutator_lift, (untyped_t)&nonterminal_keep_pouring, (untyped_t)&predicate_glass_full, (untyped_t)&mutator_put_down, (untyped_t)&mutator_serve_glass }, { PREDICATE, PREDICATE, MUTATOR, NONTERMINAL, PREDICATE, MUTATOR, MUTATOR } };// Belonging to nonterminal SERVE-COKE
production_t production_2 = { 6, { (untyped_t)&predicate_found, (untyped_t)&predicate__lp_not_sp_coke_empty_rp_, (untyped_t)&mutator_lift, (untyped_t)&nonterminal_keep_pouring, (untyped_t)&predicate__lp_and_sp__lp_not_sp_glass_full_rp__sp_coke_empty_rp_, (untyped_t)&mutator_put_down }, { PREDICATE, PREDICATE, MUTATOR, NONTERMINAL, PREDICATE, MUTATOR } };// Belonging to nonterminal SERVE-COKE
production_t production_3 = { 3, { (untyped_t)&predicate__lp_and_sp__lp_not_sp_glass_full_rp__sp__lp_not_sp_coke_empty_rp__rp_, (untyped_t)&mutator_pour, (untyped_t)&nonterminal_keep_pouring }, { PREDICATE, MUTATOR, NONTERMINAL } };// Belonging to nonterminal KEEP-POURING
production_t production_4 = { 0, {  }, {  } };// Belonging to nonterminal KEEP-POURING
production_t production_5 = { 3, { (untyped_t)&predicate_found, (untyped_t)&predicate_coke_empty, (untyped_t)&mutator_discard_can }, { PREDICATE, PREDICATE, MUTATOR } };// Belonging to nonterminal SERVE-COKE
production_t production_6 = { 2, { (untyped_t)&predicate__lp_not_sp_found_rp_, (untyped_t)&mutator_next_line }, { PREDICATE, MUTATOR } };// Belonging to nonterminal SERVE-COKE
dfa_t automata_keep_pouring = { 3, 2, { &production_3, &production_4, NULL }, { 0, 0, 0, 3 }, { (untyped_t)&predicate__lp_and_sp__lp_and_sp__lp_and_sp__lp_not_sp_glass_full_rp__sp_coke_empty_rp__sp__lp_or_sp_glass_full_sp_coke_empty_rp__rp__sp__lp_not_sp_glass_full_rp__rp_, (untyped_t)&predicate__lp_and_sp__lp_and_sp__lp_not_sp__lp_and_sp__lp_not_sp_glass_full_rp__sp_coke_empty_rp__rp__sp__lp_and_sp__lp_not_sp_glass_full_rp__sp__lp_not_sp_coke_empty_rp__rp__rp__sp__lp_not_sp_glass_full_rp__rp_, (untyped_t)&predicate__lp_and_sp__lp_and_sp__lp_not_sp__lp_and_sp__lp_not_sp_glass_full_rp__sp_coke_empty_rp__rp__sp__lp_or_sp_glass_full_sp_coke_empty_rp__rp__sp_glass_full_rp_ }, { PREDICATE, PREDICATE, PREDICATE }, { 1, 0, 1 } };
dfa_t automata_serve_coke = { 10, 4, { &production_1, &production_2, &production_5, &production_6, NULL, NULL, NULL, NULL, NULL, NULL }, { 0, 0, 0, 0, 0, 2, 4, 5, 8, 9, 12 }, { (untyped_t)&predicate__lp_and_sp__lp_not_sp_found_rp__sp__lp_not_sp_found_rp__rp_, (untyped_t)&predicate_found, (untyped_t)&predicate_coke_empty, (untyped_t)&predicate__lp_and_sp__lp_not_sp_coke_empty_rp__sp__lp_not_sp_coke_empty_rp__rp_, (untyped_t)&mutator_lift, (untyped_t)&predicate__lp_and_sp__lp_and_sp__lp_and_sp__lp_not_sp_glass_full_rp__sp_coke_empty_rp__sp__lp_or_sp_glass_full_sp_coke_empty_rp__rp__sp__lp_not_sp_glass_full_rp__rp_, (untyped_t)&predicate__lp_and_sp__lp_and_sp__lp_not_sp__lp_and_sp__lp_not_sp_glass_full_rp__sp_coke_empty_rp__rp__sp__lp_and_sp__lp_not_sp_glass_full_rp__sp__lp_not_sp_coke_empty_rp__rp__rp__sp__lp_not_sp_glass_full_rp__rp_, (untyped_t)&predicate__lp_and_sp__lp_and_sp__lp_not_sp__lp_and_sp__lp_not_sp_glass_full_rp__sp_coke_empty_rp__rp__sp__lp_or_sp_glass_full_sp_coke_empty_rp__rp__sp_glass_full_rp_, (untyped_t)&mutator_pour, (untyped_t)&predicate__lp_and_sp__lp_and_sp__lp_and_sp__lp_not_sp_glass_full_rp__sp_coke_empty_rp__sp__lp_or_sp_glass_full_sp_coke_empty_rp__rp__sp__lp_not_sp_glass_full_rp__rp_, (untyped_t)&predicate__lp_and_sp__lp_and_sp__lp_not_sp__lp_and_sp__lp_not_sp_glass_full_rp__sp_coke_empty_rp__rp__sp__lp_and_sp__lp_not_sp_glass_full_rp__sp__lp_not_sp_coke_empty_rp__rp__rp__sp__lp_not_sp_glass_full_rp__rp_, (untyped_t)&predicate__lp_and_sp__lp_and_sp__lp_not_sp__lp_and_sp__lp_not_sp_glass_full_rp__sp_coke_empty_rp__rp__sp__lp_or_sp_glass_full_sp_coke_empty_rp__rp__sp_glass_full_rp_ }, { PREDICATE, PREDICATE, PREDICATE, PREDICATE, MUTATOR, PREDICATE, PREDICATE, PREDICATE, MUTATOR, PREDICATE, PREDICATE, PREDICATE }, { 3, 5, 2, 6, 7, 1, 8, 0, 9, 1, 8, 0 } };
dfa_t automata_life = { 2, 1, { &production_0, NULL }, { 0, 0, 2 }, { (untyped_t)&predicate__lp_and_sp__lp_not_sp_found_rp__sp__lp_not_sp_found_rp__rp_, (untyped_t)&predicate_found }, { PREDICATE, PREDICATE }, { 0, 0 } };
nonterminal_t nonterminal_keep_pouring = { &automata_keep_pouring };
nonterminal_t nonterminal_serve_coke = { &automata_serve_coke };
nonterminal_t nonterminal_life = { &automata_life };
nonterminal_t *start_nonterminal = &nonterminal_life;
int main () {
  draw_game();
  sleep(3);
  run_nonterminal( start_nonterminal );
  printf("Congratulations, your motion grammar completed successfully! :)\n"  ) ;
}
