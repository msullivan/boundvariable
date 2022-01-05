
void printi(int i)
{
	putc(i + 48);
	putc(10);
}

int main()
{
	char * s = malloc (33);

	print (itoa(4, s));
	print ("\n\n");

	print (itoa(12, s));
	print ("\n\n");

	print (itoa(100, s));
	print ("\n\n");

	print (itoa(194, s));
	print ("\n\n");

	print (itoa(2994, s));
	print ("\n\n");

	print (itoa(-7, s));
	print ("\n\n");

	print (itoa(-37, s));
	print ("\n\n");
}
