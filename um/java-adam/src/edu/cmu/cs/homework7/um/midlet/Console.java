package edu.cmu.cs.homework7.um.midlet;

import java.io.*;

import javax.microedition.lcdui.*;
import javax.microedition.midlet.MIDlet;

import edu.cmu.cs.homework7.um.Interpreter;
import edu.cmu.cs.homework7.um.StatusIndicator;

public class Console implements CommandListener {
    private final MIDlet app;

    private Interpreter interp;

    final TelnetCanvas term = new TelnetCanvas();

    public Console(MIDlet app) {
        // super("Universal Machine");
        this.app = app;

        term.addCommand(Console.EXIT_COMMAND);
        term.addCommand(Console.INPUT_COMMAND);
        term.addCommand(Console.LOAD_COMMAND);
        term.setCommandListener(this);
    }

    public void giveInput(String s) {
        pOut.print(s);
        pOut.flush();
        in.feed(s);
    }

    final StringBuffer buf = new StringBuffer();

    volatile StringItem str;

    private Output out = new Output();

    private Input in = new Input();

    private PrintStream pOut = new PrintStream(out);

    private class Output extends OutputStream {
        public void write(int b) throws IOException {
            term.receive((byte) (b & 0xFF));
        }
    }

    private class Input extends InputStream {
        private StringBuffer buf = new StringBuffer();

        int bufPos;

        void feed(String s) {
            synchronized (buf) {
                buf.append(s);
                buf.notify();
            }
        }

        public int read() throws IOException {
            synchronized (buf) {
                while (true) {
                    if (buf.length() > 0) {
                        int result = buf.charAt(bufPos);
                        bufPos++;
                        if (bufPos >= buf.length()) {
                            buf.setLength(0);
                            bufPos = 0;
                        }
                        return result;
                    } else {
                        try {
                            buf.wait();
                        } catch (InterruptedException e) {
                            e.printStackTrace();
                        }
                    }
                }
            }
        }
    }

    public void quitInterp() {
        if (interp != null) {
            interp.quit();
            interp = null;
        }
    }

    public void pauseInterp() {
        if (interp != null) {
            interp.pause();
        }
    }

    public void resume() {
        // we are called when started the first time or unpaused
        Display.getDisplay(app).setCurrent(term);

        if (interp != null) {
            interp.resume();
        }
    }

    final static Command EXIT_COMMAND = new Command("Exit", Command.EXIT, 1);

    final static Command INPUT_COMMAND = new Command("Enter Input",
            Command.SCREEN, 1);

    final static Command LOAD_COMMAND = new Command("Load Program",
            Command.SCREEN, 2);

    public void commandAction(Command c, Displayable d) {
        if (c == EXIT_COMMAND) {
            quitInterp();
            app.notifyDestroyed();
        } else if (c == INPUT_COMMAND) {
            collectInput();
        } else if (c == LOAD_COMMAND) {
            Display.getDisplay(app).setCurrent(new Chooser(app, this));
        }
    }

    private void collectInput() {
        InputWindow i = new InputWindow(app, this);
        Display.getDisplay(app).setCurrent(i);
    }

    public void loadProgram(String p) {
        quitInterp();

        // System.out.println("loading " + p);
        InputStream in = getClass().getResourceAsStream("/" + p);

        // convert into array
        ByteArrayOutputStream bos = new ByteArrayOutputStream();
        int input;
        try {
            while ((input = in.read()) != -1) {
                bos.write(input);
            }

            in.close();
        } catch (IOException e) {
            e.printStackTrace();
            return;
        }
        byte data[] = bos.toByteArray();

        int words[] = new int[data.length / 4];
        for (int i = 0; i < words.length; i++) {
            int di = i * 4;
            words[i] = ((data[di] & 0xFF) << 24)
                    | ((data[di + 1] & 0xFF) << 16)
                    | ((data[di + 2] & 0xFF) << 8)
                    | ((data[di + 3] & 0xFF) << 0);
        }

        interp = new Interpreter(words, this.in, out, new Status());
        new Thread(interp).start();
    }

    private class Status implements StatusIndicator {
        private boolean statusSet;

        public void setStatus(String s) {
            term.receive("\033[s\033[H\033[1;33;44m\033[K* " + s + "\033[u");
            statusSet = true;
        }

        public void clearStatus() {
            if (statusSet) {
                term.receive("\033[s\033[H\033[K\033[u");
                statusSet = false;
            }
        }

    }

    public void displayTerminal() {
        Display.getDisplay(app).setCurrent(term);
    }

    public void clearDisplay() {
        term.reset();
    }
}
