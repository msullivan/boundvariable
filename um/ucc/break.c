int main()
{
  int i = 0;
  for (; i != 10; i++) {
    
    if (i == 4) 
      continue;
    
    if (i == 7)
      break;
    
    putc(i + 65);
    putc(10);
  }
}
