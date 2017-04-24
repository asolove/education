#include <stdio.h>

#define MAXLINE 1000

void reverse(char s[]);
int my_getline(char l[], int max);

int main()
{
  char line[MAXLINE];
  while(my_getline(line, MAXLINE-1) > 0) {
    reverse(line);
    printf("%s", line);
  }
}

int my_getline(char line[], int max)
{
  int c;
  int i = 0;
  for(i=0; i<max-1 && (c=getchar()) != EOF && c!= '\n'; ++i)
    line[i] = c;
  if(c=='\n') {
    line[i] = c;
    ++i;
  }
  line[i] = '\0';
  return i;
}

void reverse(char s[])
{
  int len = 0;
  char tmp;

  while(s[len] != '\0')
    len++;
  len--;

  for(int i=0; i<=len/2; i++) {
    tmp = s[i];
    s[i] = s[len-i];
    s[len-i] = tmp;
  }
}
