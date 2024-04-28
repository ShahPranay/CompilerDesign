int printf(char const *format, ...);

int main()
{
  int itr = 3;

  if (itr)
    printf("itr is valid\n");

  while (itr)
  {
    printf("hello guys\n");    
    itr = itr - 1;
  }
  return 0;
}
