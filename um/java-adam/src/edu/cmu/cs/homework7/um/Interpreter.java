package edu.cmu.cs.homework7.um;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintStream;
import java.util.Vector;

public class Interpreter implements Runnable {

    private int array0[];

    private int pc;

    private int reg[] = new int[8];

    private Vector mem = new Vector();

    private Vector freeList = new Vector();

    private InputStream in;

    private OutputStream out;

    private PrintStream outP;

    private volatile boolean running = true;

    private volatile boolean paused;

    private final Object synchronizer = new Object();

    private final StatusIndicator status;

    // private boolean tracing;

    public Interpreter(int[] initialProgram, InputStream in, OutputStream out,
            StatusIndicator s) {
        array0 = new int[initialProgram.length];
        mem.addElement(null); // reserve array0

        System.arraycopy(initialProgram, 0, array0, 0, initialProgram.length);
        this.in = in;
        this.out = out;
        outP = new PrintStream(out);
        status = s;
    }

    // public void setTracing(boolean b) {
    // tracing = b;
    // }

    final static public byte OP_CMOV = 0;

    final static public byte OP_INDEX = 1;

    final static public byte OP_AMEND = 2;

    final static public byte OP_ADD = 3;

    final static public byte OP_MUL = 4;

    final static public byte OP_DIV = 5;

    final static public byte OP_NAND = 6;

    final static public byte OP_HALT = 7;

    final static public byte OP_ALLOC = 8;

    final static public byte OP_ABAND = 9;

    final static public byte OP_OUT = 10;

    final static public byte OP_IN = 11;

    final static public byte OP_LOAD = 12;

    final static public byte OP_ORTHO = 13;

    public void pause() {
        paused = true;
    }

    public void resume() {
        paused = false;
        synchronized (synchronizer) {
            synchronizer.notify();
        }
    }

    public void quit() {
        running = false;
    }

    public void run() {
//        long numInsts = 0;
        long startTime = System.currentTimeMillis();
        
        outP.println();
        status.clearStatus();

        while (running) {
            while (paused) {
                status.setStatus("Paused");
                synchronized (synchronizer) {
                    try {
                        synchronizer.wait();
                    } catch (InterruptedException e) {
                    }
                }
            }

            // fetch
            final int inst = array0[pc++];
//            numInsts++;

//            if (numInsts % 1000000 == 0) {
//                status.setStatus(numInsts + " insts");
//            }

            // if (tracing) {
            // System.out.println(Integer.toHexString(inst));
            // }

            // decode
            byte a, b, c;
            int val;

            final byte opcode = (byte) ((inst & 0xF0000000) >>> 28);
            // if (tracing) {
            // System.out.println(Integer.toBinaryString(opcode));
            // }
            if (opcode < 13) {
                a = (byte) ((inst & 0x1C0) >>> 6);
                b = (byte) ((inst & 0x38) >>> 3);
                c = (byte) (inst & 0x7);
                val = 0;
            } else {
                a = (byte) (((inst & 0xE000000)) >>> 25);
                b = 0;
                c = 0;
                val = inst & 0x1FFFFFF;
            }

            // execute
            int array[];
            switch (opcode) {
            case OP_CMOV:
                // trace("CMOV ", a, b, c);
                if (reg[c] != 0) {
                    reg[a] = reg[b];
                }
                break;

            case OP_INDEX:
                // trace("INDEX", a, b, c);
                if (reg[b] == 0) {
                    array = array0;
                } else {
                    array = (int[]) mem.elementAt(reg[b]);
                }
                reg[a] = array[reg[c]];
                break;

            case OP_AMEND:
                // trace("AMEND", a, b, c);
                if (reg[a] == 0) {
                    array = array0;
                } else {
                    array = (int[]) mem.elementAt(reg[a]);
                }
                array[reg[b]] = reg[c];
                break;

            case OP_ADD:
                // trace("ADD ", a, b, c);
                reg[a] = reg[b] + reg[c];
                break;

            case OP_MUL:
                // trace("MUL ", a, b, c);
                reg[a] = reg[b] * reg[c];
                break;

            case OP_DIV:
                // trace("DIV ", a, b, c);
                reg[a] = (int) ((reg[b] & 0xFFFFFFFFL) / (reg[c] & 0xFFFFFFFFL));
                break;

            case OP_NAND:
                // trace("NAND ", a, b, c);
                reg[a] = ~(reg[b] & reg[c]);
                break;

            case OP_HALT:
                // trace("HALT ", a, b, c);
                running = false;
                long totalTime = System.currentTimeMillis() - startTime;
                outP.println();
                outP.println("Wall clock time: " + totalTime + " ms");
                status.setStatus("Halted");
                break;

            case OP_ALLOC:
                // trace("ALLOC", a, b, c);
                if (freeList.isEmpty()) {
                    mem.addElement(new int[reg[c]]);
                    reg[b] = mem.size() - 1;
                } else {
                    // freelist
                    int fi = freeList.size() - 1;
                    int i = ((Integer) freeList.elementAt(fi)).intValue();
                    mem.setElementAt(new int[reg[c]], i);
                    freeList.removeElementAt(fi);
                    reg[b] = i;
                }
                break;

            case OP_ABAND:
                // trace("ABAND", a, b, c);
                if (reg[c] == 0) {
                    throw new Fail("Cannot abandon the zero array");
                }
                mem.setElementAt(null, reg[c]);
                freeList.addElement(new Integer(reg[c]));
                break;

            case OP_OUT:
                // trace("OUT ", a, b, c);
                if (reg[c] > 255) {
                    throw new Fail("Output too large");
                }
                try {
                    out.write((char) reg[c]);
                    out.flush();
                } catch (IOException e1) {
                    e1.printStackTrace();
                }
                break;

            case OP_IN:
                // trace("IN ", a, b, c);
//                status.setStatus("Waiting for input");
                try {
                    reg[c] = in.read();
                } catch (IOException e) {
                    e.printStackTrace();
                    return;
                }
                break;

            case OP_LOAD:
                // trace("LOAD ", a, b, c);
                if (reg[b] != 0) {
                    int prog[] = (int[]) mem.elementAt(reg[b]);
                    array0 = new int[prog.length];
                    System.arraycopy(prog, 0, array0, 0, prog.length);
                }
                pc = reg[c];
                break;

            case OP_ORTHO:
                // trace("ORTHO", a, b, c);
                reg[a] = val;
                break;

            default:
                throw new Fail("Invalid instruction");
            }
        }
    }

    private void trace(String op, byte a, byte b, byte c) {
        if (false) {
            System.out.println(op + " PC: 0x"
                    + Integer.toHexString((pc - 1) * 4) + ", A: 0x"
                    + Integer.toHexString(a) + ", B: 0x"
                    + Integer.toHexString(b) + ", C: 0x"
                    + Integer.toHexString(c));
            for (int i = 0; i < 8; i++) {
                System.out.println(" reg[" + i + "]: "
                        + Integer.toHexString(reg[i]));
            }
        }
    }
}
