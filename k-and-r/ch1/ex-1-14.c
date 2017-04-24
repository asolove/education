#include <stdio.h>

// Histogram of character frequency
int main()
{
  int c, max;
  int firstChar = 'A';
  int lastChar = 'z';
  int numChars = lastChar - firstChar + 1;

  int count[numChars];
  for(int i=0; i<numChars; i++)
    count[i] = 0;

  while((c = getchar()) != EOF) {
    if(c >= firstChar && c <=lastChar) {
      count[c - firstChar]++;
    }
  }

  max = 0;
  for(int i=0; i<numChars; i++)
    if(count[i] > max)
      max = count[i];

  for(int row=max; row>=1; row--) {
    for(int col=0; col<numChars; col++) {
      if(count[col] >= row)
        putchar('*');
      else
        putchar(' ');
    }
    putchar('\n');
  }
  for(c=firstChar; c<=lastChar; c++) {
    putchar(c);
  }
}
