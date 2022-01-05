void try(int act, int exp) {
	if (act == exp) 
		print ("yes\n");
	else
		print ("no\n");
}

void main() {
	print ("<=\n");
	try (3 <= 0, 0);
	try (3 <= 3, 1);
	try (3 <= 10, 1);

	try (3 <= -1, 0);
	try (3 <= -3, 0);

	try (-3 <= 1, 1);
	try (-3 <= 3, 1);

	try (-3 <= -1, 1);
	try (-3 <= -3, 1);
	try (-3 <= -10, 0);

	print ("<\n");
	try (3 < 0, 0);
	try (3 < 3, 0);
	try (3 < 10, 1);

	try (3 < -1, 0);
	try (3 < -3, 0);

	try (-3 < 1, 1);
	try (-3 < 3, 1);

	try (-3 < -1, 1);
	try (-3 < -3, 0);
	try (-3 < -10, 0);

	print (">\n");
	try (3 > 0, 1);
	try (3 > 3, 0);
	try (3 > 10, 0);

	try (3 > -1, 1);
	try (3 > -3, 1);

	try (-3 > 1, 0);
	try (-3 > 3, 0);

	try (-3 > -1, 0);
	try (-3 > -3, 0);
	try (-3 > -10, 1);

	print (">=\n");
	try (3 >= 0, 1);
	try (3 >= 3, 1);
	try (3 >= 10, 0);

	try (3 >= -1, 1);
	try (3 >= -3, 1);

	try (-3 >= 1, 0);
	try (-3 >= 3, 0);

	try (-3 >= -1, 0);
	try (-3 >= -3, 1);
	try (-3 >= -10, 1);
}
