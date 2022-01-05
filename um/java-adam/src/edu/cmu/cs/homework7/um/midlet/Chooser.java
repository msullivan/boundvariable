package edu.cmu.cs.homework7.um.midlet;

import javax.microedition.lcdui.*;
import javax.microedition.midlet.MIDlet;

public class Chooser extends List implements CommandListener {

    final private MIDlet app;

    final private Console console;

    final private static String programs[] = { "internal-challenge.um",
            "sandmark.umz" };

    public Chooser(MIDlet m, Console c) {
        super("Load Program", IMPLICIT);

        app = m;
        console = c;

        for (int i = 0; i < programs.length; i++) {
            append(programs[i], null);
        }

        addCommand(Console.EXIT_COMMAND);

        setCommandListener(this);
    }

    public void commandAction(Command c, Displayable d) {
        if (c == SELECT_COMMAND) {
            int i = getSelectedIndex();
            console.loadProgram(programs[i]);
            console.clearDisplay();
            console.displayTerminal();
        } else if (c == Console.EXIT_COMMAND) {
            app.notifyDestroyed();
        }
    }
}
