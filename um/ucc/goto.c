int main()
{
	int i = 0;
  for (; i != 10; i++) {

    putc (46);

		if (i == 7)
			goto DONE;

		if (i == 4) 
			goto NEXT;

		putc(i + 65);
		putc(10);

	NEXT:
	}

 DONE:
}
