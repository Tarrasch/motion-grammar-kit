void draw_cokes() {
  foru(i, NUM_COKES) {
    int r = H - 10 - 5*is_lifted[i];
    int c = 10 + i*10;
    buffer[r][c] = cokes[i] >= 0 ? cokes[i]+'0' : 'X';
    if(coke_is_empty(i)) {
      draw_text_centered("EMPTY", r-2, c);
    }
    if(coke_is_recycled(i)) {
      draw_text_centered("RECYCLED", r-2, c);
    }
    char buf[100];
    sprintf(buf, "COKE %i", i+1);
    draw_text_centered(buf, r-5, c);
    if(is_lifted[i]) {
      draw_text_centered("LIFTED", r-7, c);
    }
  }
}

void draw_glass() {
  int r = H-30;
  int c = W/2;
  char buf[100];
  char *full_desc = glass_is_full() ? "full" : "not full";
  sprintf(buf, "Glass is %s. Current filledness %i", full_desc, glass);
  draw_text_centered(buf, r, c);
  sprintf(buf, "Glasses served %i", glasses_served);
  draw_text_centered(buf, r-5, c);
}

void draw_robot() {
  int r = H-5;
  int c = 10 + 10*robot_pos;
  draw_text_centered("+-=ROBOT=-+", r, c);
}

void draw_game() {
  clear_buffer();
  draw_cokes();
  draw_glass();
  draw_robot();
  draw();
}
