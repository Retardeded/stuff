// This is an independent project of an individual developer. Dear PVS-Studio, please check it.
// PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
package solver;


import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class Parameters {
    public final String in;
    public final String out;
    public final boolean verbose;
    public Parameters( String[] args) throws IOException {
        boolean needAssignedIn = true;
        String inTemp = "input.txt";
        boolean needAssignedOut = true;
        String outTemp = "output.txt";
        boolean verboseTemp = false;
        for (int i = 0; i < args.length; ++i) {
            if (needAssignedIn && "-in".equals(args[i])) {
                if (i < args.length - 1) {
                    inTemp = args[i+1];
                    needAssignedIn = false;
                    ++i;
                }
            } else if (needAssignedOut && "-out".equals(args[i])) {
                if (i < args.length - 1) {
                    outTemp = args[i+1];
                    needAssignedOut = false;
                    ++i;
                }
            } else if (!verboseTemp && "-verbose".equals(args[i])) {
                verboseTemp = true;
            }
        }
        in = inTemp;
        out = outTemp;

        BufferedReader br = new BufferedReader(new FileReader(in));
        String line;
        while ((line = br.readLine()) != null) {
            System.out.println(line);
        }
        br.close();

        verbose = true;
    }
}
