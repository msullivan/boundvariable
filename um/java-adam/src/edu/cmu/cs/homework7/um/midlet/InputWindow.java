package edu.cmu.cs.homework7.um.midlet;

import javax.microedition.lcdui.*;
import javax.microedition.midlet.MIDlet;

public class InputWindow extends TextBox implements CommandListener {
    final private MIDlet app;
    final private Console console;
    
    public InputWindow(MIDlet app, Console console) {
        super("Enter Input", null, 1024, TextField.ANY);
        
        this.app = app;
        this.console = console;
        
        addCommand(OK_COMMAND);
        addCommand(CANCEL_COMMAND);
        setCommandListener(this);
    }
    
    public void commandAction(Command c, Displayable d) {
        if (c == OK_COMMAND) {
            console.displayTerminal();
            console.giveInput(getString() + "\n");
        } else if (c == CANCEL_COMMAND) {
            console.displayTerminal();
        }
    }
    
    final static Command OK_COMMAND = new Command("OK", Command.OK, 1);
    final static Command CANCEL_COMMAND = new Command("Cancel", Command.CANCEL, 1);
}
