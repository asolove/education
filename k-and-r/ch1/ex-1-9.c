#include <stdio.h>
#include <stdbool.h>

int main()
{
  bool prevBlank = false;
  char c;
  while((c = getchar()) != EOF) {
    if(c == ' ') {
      if(prevBlank) {
        continue;
      } else {
        prevBlank = true;
      }
    } else {
      prevBlank = false;
    }

    putchar(c);
  }
}
