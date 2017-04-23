#include <stdio.h>

int main()
{ int lines = 0;
  char c;

  while((c = getchar()) != EOF) {
    if('\n' == c)
      lines++;
  }

  printf("%d lines\n", lines);
}
