#!/usr/bin/python

import sys

debug = False

class UM:
    def cmov(self, a, b, c):
        if self.reg[c] != 0:
            self.reg[a] = self.reg[b]


    def index(self, a, b, c):
        self.reg[a] = self.mem[self.reg[b]][self.reg[c]]


    def amend(self, a, b, c):
        self.mem[self.reg[a]][self.reg[b]] = self.reg[c]


    def add(self, a, b, c):
        self.reg[a] = (self.reg[b] + self.reg[c]) & 0xFFFFFFFF
        

    def mul(self, a, b, c):
        self.reg[a] = (self.reg[b] * self.reg[c]) & 0xFFFFFFFF


    def div(self, a, b, c):
        self.reg[a] = (self.reg[b] // self.reg[c]) & 0xFFFFFFFF
        

    def nand(self, a, b, c):
        self.reg[a] = (~(self.reg[b] & self.reg[c])) & 0xFFFFFFFF
        

    def halt(self, a, b, c):
        print
        print "*** HALTED ***"
        print " %d instructions executed" % (self.instructions)
        self.done = True


    def alloc(self, a, b, c):
        if len(self.freelist) == 0:
            self.mem.append([0] * self.reg[c])
            self.reg[b] = len(self.mem) - 1
        else:
            i = self.freelist[-1]
            self.mem[i] = [0] * self.reg[c]
            del self.freelist[-1]
            self.reg[b] = i


    def aband(self, a, b, c):
        self.mem[self.reg[c]] = None
        self.freelist.append(self.reg[c])


    def output(self, a, b, c):
        sys.stdout.write(chr(self.reg[c]))
        sys.stdout.flush()


    def input(self, a, b, c):
        s = sys.stdin.read(1)
        if s != "":
            self.reg[c] = ord(s)
        else:
            self.reg[c] = 0xFFFFFFFF

        
    def load(self, a, b, c):
        if self.reg[b] != 0:
            self.mem[0] = list(self.mem[self.reg[b]])
        self.pc = self.reg[c]


    def ortho(self, a, val):
        self.reg[a] = val
        

    dispatch = [cmov, index, amend, add, mul, div, nand, halt, alloc, aband,
                output, input, load, ortho]
    
    
    def __init__(self, program):
        self.mem = [list(program)]
        self.pc = 0
        self.reg = [0] * 8
        self.freelist = []
        self.done = False
        self.instructions = 0
    
    def run(self):
        while not self.done:
            inst = self.mem[0][self.pc]
            self.pc += 1

            opcode = (inst & 0xF0000000) >> 28
            self.instructions += 1
            #print hex(inst)

            if opcode < 13:
                a = (inst & 0x1C0) >> 6
                b = (inst & 0x38) >> 3
                c = (inst & 0x7)

                if debug:
                    print [hex(self.pc - 1), UM.dispatch[opcode].__name__,
                           hex(a), hex(b), hex(c), map(hex, self.reg)]

                UM.dispatch[opcode](self, a, b, c)
            else:
                a = (inst & 0xE000000) >> 25
                val = (inst & 0x1FFFFFF)

                if debug:
                    print [hex(self.pc - 1), UM.dispatch[opcode].__name__,
                           hex(a), hex(val), map(hex, self.reg)]

                UM.dispatch[opcode](self, a, val)

            

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print "Usage: um.py item.um"
        sys.exit(0)

    f = open(sys.argv[1], 'rb')
    program = []

    try:
        import psyco
        psyco.full()
    except:
        pass


    while True:
        w = f.read(4)
        #print map(hex, map(ord, w))
        if w == "":
            break
        word = ((ord(w[0]) & 0xFF) << 24) | ((ord(w[1]) & 0xFF) << 16) \
               | ((ord(w[2]) & 0xFF) << 8) | ((ord(w[3]) & 0xFF))
        program.append(word)


    um = UM(program)
    um.run()
