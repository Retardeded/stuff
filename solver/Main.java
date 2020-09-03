package solver;

import java.io.IOException;

public class Main {
    public static void main(String[] args) throws IOException {

            final Parameters p = new Parameters(args);
            final Solver s = new Solver(p.in, p.verbose);
            s.solve();
            s.writeSolutionToFile(p.out);

    }
}