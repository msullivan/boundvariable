int main() {
	int a;
	while (EOF != (a = getc())) {
		putc(a);
	}
	return 0;
}
