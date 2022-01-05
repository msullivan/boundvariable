struct ii { int a; int b };

void f (struct ii arg) {
	putc (arg.a);
	putc (10);
	putc (arg.b);
	putc (10);

	arg.a = 67;
	arg.b = 68;

	putc (arg.a);
	putc (10);
	putc (arg.b);
	putc (10);

}

int main () {
  struct ii s;
	/*
	int *t = (int *)&s;
	t[0] = 65;
	t[1] = 66;
	*/
	s.a = 65;
	s.b = 66;

	putc (s.a);
	putc (10);
	putc (s.b);
	putc (10);

	f (s);

	putc (s.a);
	putc (10);
	putc (s.b);
	putc (10);

}
