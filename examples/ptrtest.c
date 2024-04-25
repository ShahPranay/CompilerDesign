int printf(const char *format, ...);

int main(int argc, char **argv)
{
  int *a;
  *a = 2;
  printf("Value of a = %d", *a);
}
