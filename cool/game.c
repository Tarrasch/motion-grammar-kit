/*
 * =====================================================================================
 *
 *       Filename:  game.c
 *
 *    Description:  
 *
 *        Version:  1.0
 *        Created:  05/01/2013 02:07:12 AM
 *       Revision:  none
 *       Compiler:  gcc
 *
 *         Author:  YOUR NAME (), 
 *   Organization:  
 *
 * =====================================================================================
 */
#include <stdlib.h>

#define COKE_CAPACITY 5
typedef int coke_t; // -1 no coke else remaining fluid

#define NUM_COKES 6
coke_t cokes[NUM_COKES] = { COKE_CAPACITY,COKE_CAPACITY,COKE_CAPACITY,COKE_CAPACITY, COKE_CAPACITY, COKE_CAPACITY, } ;
int is_lifted[NUM_COKES] = { 0, 0, 0, 0, 0, 0 };

#define GLASS_CAPACITY 7
int glass = 0;
int glasses_served = 0;

int robot_pos = 0;
//int selected_coke = -1;

int glass_is_full() { return glass == GLASS_CAPACITY; }
int coke_is_empty(int id) { assert(id >= 0); return cokes[id] == 0; }
int coke_is_recycled(int id) { assert(id >= 0); return cokes[id] == -1; }
void coke_recycle(int id) { assert(id >= 0); assert(!is_lifted[id]); assert(coke_is_empty(id)); cokes[id] = -1; }
void pour_from(int id) { assert(id >= 0); assert(!coke_is_empty(id)); assert(!glass_is_full()); assert(is_lifted[id]);
  cokes[id]--; glass++;
}
void serve_glass() { assert(glass_is_full()); glass = 0; glasses_served++; }
