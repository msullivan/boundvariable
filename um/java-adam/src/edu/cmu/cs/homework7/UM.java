package edu.cmu.cs.homework7;

import java.io.*;

import edu.cmu.cs.homework7.um.Interpreter;
import edu.cmu.cs.homework7.um.StatusIndicator;

public class UM {
    public static void main(String[] args) {
        if (args.length != 1) {
            System.out.println("Usage: " + UM.class.getName() + " file");
            return;
        }

        // load code
        InputStream in = null;

        try {
            in = new FileInputStream(args[0]);
        } catch (FileNotFoundException e) {
            e.printStackTrace();
            return;
        }

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
        if (data.length % 4 != 0) {
            System.out.println("ERROR: partial word read from file");
            return;
        }

        int words[] = new int[data.length / 4];
        for (int i = 0; i < words.length; i++) {
            int di = i * 4;
            words[i] = ((data[di] & 0xFF) << 24)
                    | ((data[di + 1] & 0xFF) << 16)
                    | ((data[di + 2] & 0xFF) << 8)
                    | ((data[di + 3] & 0xFF) << 0);
        }

        Interpreter interp = new Interpreter(words, System.in, System.out,
                new ANSIStatus());
        interp.run();
    }

    static private class ANSIStatus implements StatusIndicator {
        boolean statusSet;
        
        public void setStatus(String s) {
            System.out.print("\033[s\033[H\033[1;33;44m\033[K* " + s + "\033[u");
            statusSet = true;
        }

        public void clearStatus() {
            if (statusSet) {
                System.out.print("\033[s\033[H\033[K\033[u");
                statusSet = false;
            }
        }
    }
}
