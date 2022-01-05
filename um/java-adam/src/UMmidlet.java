

import javax.microedition.midlet.MIDlet;
import javax.microedition.midlet.MIDletStateChangeException;

import edu.cmu.cs.homework7.um.midlet.Console;

public class UMmidlet extends MIDlet {

    final Console c = new Console(this);
    
    protected void startApp() throws MIDletStateChangeException {
        c.resume();
    }

    protected void pauseApp() {
        c.pauseInterp();
    }

    protected void destroyApp(boolean arg0) throws MIDletStateChangeException {
        c.quitInterp();
    }

}
