int printf(char const *format, ...);
int a = 5;

int f(int x)
{
  if (x == 1)
    printf("hello");
}

int
main(int argc, char **argv)
{
  char *c = "hello, world\n";
  printf(c);
  int a = 1;
  f (a);
  return 0;
}
