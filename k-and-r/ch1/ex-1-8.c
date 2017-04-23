#include <stdio.h>

int main()
{
  int blanks = 0;
  int tabs = 0;
  int newlines = 0;

  char c;
  while((c = getchar()) != EOF) {
    switch(c) {
      case ' ':
        blanks++;
        break;
      case '\t':
        tabs++;
        break;
      case '\n':
        newlines++;
        break;
    }
  }

  printf("%d blanks, %d tabs, %d newlines", blanks, tabs, newlines);
}
