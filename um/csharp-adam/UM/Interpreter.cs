
using System;
using System.Collections;

namespace UM
{
	
	public class Interpreter
	{

        enum Opcode {
            Cmov,
            Index,
            Amend,
            Add,
            Mul,
            Div,
            Nand,
            Halt,
            Alloc,
            Abandon,
            Out,
            In,
            Load,
            Ortho
        };

        private uint[] Array0;

        private uint PC;

        private readonly uint[] Reg = new uint[8];

        private readonly IList Mem = new ArrayList();

        private readonly IList Freelist = new ArrayList();


        public Interpreter(uint[] InitialProgram)
		{
            Array0 = new uint[InitialProgram.Length];
            InitialProgram.CopyTo(Array0, 0);
            Mem.Add(null);   // reserve initial item for Array0
        }
        
        public void Run()
        {
            while(true) {
                uint inst = Array0[PC++];
                
                Opcode opcode = (Opcode)((inst & 0xF0000000) >> 28);
                byte a = (byte) ((inst & 0x1C0) >> 6);
                byte b = (byte) ((inst & 0x38) >> 3);
                byte c = (byte) (inst & 0x7);

                uint[] array;

//				Console.WriteLine(opcode + " " + a + " " + b + " " + c);
//				for (int i = 0; i < Reg.Length; i++) {
//					Console.WriteLine(" " + Reg[i]);
//				}
//
                switch (opcode) {
                case Opcode.Cmov:
                    if (Reg[c] != 0) {
                        Reg[a] = Reg[b];
                    }
                    break;
                    
                case Opcode.Index:
                    array = (Reg[b] == 0) ? Array0 : (uint[]) Mem[(int) Reg[b]];
                    Reg[a] = array[Reg[c]];
                    break;
                    
                case Opcode.Amend:
                    array = (Reg[a] == 0) ? Array0 : (uint[]) Mem[(int) Reg[a]];
                    array[Reg[b]] = Reg[c];
                    break;
                    
                case Opcode.Add:
                    Reg[a] = Reg[b] + Reg[c];
                    break;
                    
                case Opcode.Mul:
                    Reg[a] = Reg[b] * Reg[c];
                    break;
                    
                case Opcode.Div:
                    Reg[a] = Reg[b] / Reg[c];
                    break;
                    
                case Opcode.Nand:
                    Reg[a] = ~(Reg[b] & Reg[c]);
                    break;
                    
                case Opcode.Halt:
                    Console.WriteLine();
                    Console.WriteLine(" *** HALTED ***");
                    return;
                    
                case Opcode.Alloc:
                    if (Freelist.Count == 0) {
                        Mem.Add(new uint[Reg[c]]);
                        Reg[b] = (uint) (Mem.Count - 1);
                    } else {
                        int fi = Freelist.Count - 1;
                        int i = (int) (uint) Freelist[fi];
                        Mem[i] = new uint[Reg[c]];
                        Freelist.RemoveAt(fi);
                        Reg[b] = (uint) i;
                    }
                    break;
                    
                case Opcode.Abandon:
                    Mem[(int) Reg[c]] = null;
                    Freelist.Add(Reg[c]);
                    break;
                    
                case Opcode.Out:
                    Console.Write((char) Reg[c]);
                    Console.Out.Flush();
                    break;
                    
                case Opcode.In:
                    Reg[c] = (uint) Console.Read();
                    break;
                    
                case Opcode.Load:
                    if (Reg[b] != 0) {
                        uint[] prog = (uint[]) Mem[(int) Reg[b]];
                        Array0 = new uint[prog.Length];
                        prog.CopyTo(Array0, 0);
                    }
                    PC = Reg[c];
                    break;
                    
                case Opcode.Ortho:
                    Reg[(inst & 0xE000000) >> 25] = inst & 0x1FFFFFF;
                    break;
                    
                default:
                    Console.WriteLine("Illegal instruction");
                    return;
                }
            }
        }
	}
	
}
