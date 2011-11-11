// The appserver provides argc/argv as appropriate for 64-bit programs.
// This function fixes up the stack for 32-bit programs.
void _fix_args(void** args)
{
  // since we're little-endian, the algorithm is to discard every second
  // word on the stack until argv/envp are exhausted
  int zeros, i;
  for(zeros = 0, i = 1; zeros < 2; i++)
  {
    if(args[2*i] == 0)
      zeros++;
    args[i] = args[2*i];
  }
}
