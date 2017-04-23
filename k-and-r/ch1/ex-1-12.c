#include <stdio.h>

// Print input one word per line
int main()
{
  int c;
  int inWord = 0;

  while((c = getchar()) != EOF) {
    if(c == ' ' || c == '\n' || c == '\t') {
      if(inWord) {
        inWord = 0;
        putchar('\n');
      }
    } else {
      inWord = 1;
      putchar(c);
    }
  }
}
