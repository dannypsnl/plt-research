#include <stdio.h>

extern int add(int, int);

int main() {
  printf("test gocc\n");
  printf("add from gocc, add(1, 2) = %d\n", add(1, 2));
}
