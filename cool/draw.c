#include <stdlib.h>
#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#define foru(i, n) for(int i = 0; i < n; i++)

int W=100;
int H=40;

char buffer[40][101];

void scrclr() {
  system("clear");
}

void draw_text(char *str, int r, int c) {
  assert(r < H);
  assert(strlen(str) + c < W);
  memcpy ( &buffer[r][c], str, strlen(str)+0 ); // We dont want stupid \n
}

void draw_text_centered(char *str, int r, int c) {
  draw_text(str, r, c - strlen(str)/2);
}

void clear_buffer() {
  memset( buffer, ' ', sizeof(buffer) );
  foru(i, H) buffer[i][W] = '\0';
}

void draw() {
  scrclr();
  foru(i, H) {
    printf("%s\n", buffer[i]);
  }
}

void test() {
  foru(j, 5) {
    clear_buffer();
    draw_text("hello mom", 15, 15);
    foru(i, 30) buffer[i+j][5*i/(5+j)] = '*';
    draw();
    sleep(1);
  }
}

/* int main() { */
/*   test(); */
/* } */
