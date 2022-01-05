{ array[0,NR-1] = (($1*256 + $2)*256 + $3)*256 + $4; }

END {
    arrsz[0] = NR;
    reg[0] = reg[1] = reg[2] = reg[3] = \
	reg[4] = reg[5] = reg[6] = reg[7] = 0;
    arrcnt = 1;
    finger = 0;

    while (1) {
	instr = array[0,finger];
	op = rshift(instr, 28);
	areg = and(rshift(instr, 6), 7);
	breg = and(rshift(instr, 3), 7);
	creg = and(instr, 7);

	finger = finger + 1;
	if (op == 0) {
	    if (reg[creg] != 0) reg[areg] = reg[breg];
	}
	else if (op == 1) {
	    reg[areg] = array[reg[breg],reg[creg]];
	}
	else if (op == 2) {
	    array[reg[areg],reg[breg]] = reg[creg];
	}
	else if (op == 3) {
	    reg[areg] = and(reg[breg] + reg[creg], 4294967295);
	}
	else if (op == 4) {
	    reg[areg] = and(reg[breg] * reg[creg], 4294967295);
	}
	else if (op == 5) {
	    reg[areg] = and(int(reg[breg] / reg[creg]), 4294967295);
	}
	else if (op == 6) {
	    reg[areg] = compl(and(reg[breg], reg[creg]));
	}
	else if (op == 7) {
	    exit;
	}
	else if (op == 8) {
	    arrsz[arrcnt] = reg[creg];
	    reg[breg] = arrcnt;
	    arrcnt = arrcnt + 1;
	}
	else if (op == 9) {
	}
	else if (op == 10) {
	    printf "%c", reg[creg];
	}
	else if (op == 11) {
	    reg[creg] = system("./getchar");
	}
	else if (op == 12) {
	    B = reg[breg];
	    if (B != 0) {
		if (!arrsz[B]) {
		    print "Jump to inactive array";
		    exit;
		}
		for (i=0; i<arrsz[B]; i=i+1) {
		    array[0,i] = array[B,i];
		}
	    }
	    finger = reg[creg];
	}
	else if (op == 13) {
	    r = and(rshift(instr, 25), 7);
	    reg[r] = and(instr, 33554431);
	}
	else {
	    print "This is unacceptable!";
	    exit;
	}
    }
}
