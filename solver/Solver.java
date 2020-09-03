// This is an independent project of an individual developer. Dear PVS-Studio, please check it.
// PVS-Studio Static Code Analyzer for C, C++ and C#: http://www.viva64.com
package solver;


import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.Scanner;
import java.util.stream.IntStream;

public class Solver {
    private static final Complex ZERO = new Complex(0.0, 0.0);
    private static final Complex ONE = new Complex(1.0, 0.0);
    private static final Complex MINUS_ONE = new Complex(-1.0, 0.0);

    private final int numberEquations;
    private final int numberVariables;
    private final Complex[][] matrix;
    private final Complex[] solutionPartial;
    private final String[] solutionGeneral;
    private final int[] solutionIndexes;
    private final boolean verbose;
    private NumberSolutions numberSolutions = NumberSolutions.ONE;

    public Solver( Scanner sc) throws NumberFormatException {
        this(sc, false);
    }

    public Solver( Scanner sc, boolean verbose) throws NumberFormatException {
        numberVariables = sc.nextInt();
        final int realNumberEquations = sc.nextInt();
        if (numberVariables < 1 || realNumberEquations < 1) {
            throw new NumberFormatException("wrong input data");
        }
        numberEquations = (realNumberEquations < numberVariables) ? numberVariables : realNumberEquations;
        matrix = new Complex[numberEquations][numberVariables + 1];
        for (int i = 0; i < realNumberEquations; ++i) {
            for (int j = 0; j < numberVariables + 1; ++j) {
                matrix[i][j] = new Complex(sc.next());
            }
        }
        sc.close();

        for (int i = realNumberEquations; i < numberEquations; ++i) {
            for (int j = 0; j < numberVariables + 1; ++j) {
                matrix[i][j] = new Complex();
            }
        }
        solutionPartial = new Complex[numberVariables];
        solutionGeneral = new String[numberVariables];
        solutionIndexes = IntStream.range(0, numberVariables).toArray();
        this.verbose = verbose;
    }

    public Solver(String in, boolean verbose) throws FileNotFoundException {
        this(new Scanner(new File(in)), verbose);
    }

    public NumberSolutions getNumberSolutions() {
        return numberSolutions;
    }

    public String[] getSolutionGeneral() {
        if (numberSolutions != NumberSolutions.MANY) {
            return null;
        }
        return solutionGeneral.clone();
    }

    public Complex[] getSolutionPartial() {
        if (numberSolutions == NumberSolutions.NONE) {
            return null;
        }
        return solutionPartial.clone();
    }

    public void solve() {
        if (verbose) {
            System.out.println("Start solving the equation.");
            System.out.println("Rows manipulation:");
        }
        gausFirstStep();
        if (numberSolutions != NumberSolutions.NONE) {
            gausSecondStep();
            generateSolutions();
            checkThatSolutionIsSane();
        }
        printSolution();
    }

    public void writeSolutionToFile(String out) throws FileNotFoundException {
        final File file = new File(out);
        final PrintWriter printWriter = new PrintWriter(file);
        printSolutionInternal(printWriter);
        printWriter.close();
        if (verbose) {
            System.out.printf("Saved to file %s\n", out);
        }
    }

    private void addKRow1ToRow2(Complex k, int row1, int row2) {
        if (verbose) {
            System.out.printf("%s * R%d +R%d -> R%d\n", k.toString(), row1 + 1, row2 + 1, row2 + 1);
        }
        for (int i = 0; i < numberVariables + 1; ++i) {
            final Complex temp = Complex.multiply(k, matrix[row1][i]);
            matrix[row2][i] = Complex.add(temp, matrix[row2][i]);
        }
    }

    private void checkThatSolutionIsSane() {
        for (int i = numberVariables; i < numberEquations; ++i) {
            Complex sum = new Complex(0.0, 0.0);
            for (int j = 0; j < numberVariables; ++j) {
                final Complex temp = Complex.multiply(solutionPartial[solutionIndexes[j]], matrix[i][solutionIndexes[j]]);
                sum = Complex.add(sum, temp);
            }
            if (!sum.equals(matrix[i][numberVariables])) {
                numberSolutions = NumberSolutions.NONE;
                return;
            }
        }
    }

    private void divideRow(int row, Complex k) {
        if (verbose) {
            System.out.printf("R%d / %s -> R%d\n", row + 1, k.toString(), row + 1);
        }
        final int n = matrix[row].length;
        for (int i = 0; i < n; ++i) {
            matrix[row][i] = Complex.divide(matrix[row][i], k);
        }
    }

    private void gausFirstStep() {
        for (int i = 0; i < numberVariables; ++i) {
            if (matrix[i][i].equals(ZERO)) {
                boolean foundNonZeroElement = false;
                for (int j = i + 1; j < numberEquations; ++j) {
                    if (!matrix[j][i].equals(ZERO)) {
                        swapRows(i, j);
                        foundNonZeroElement = true;
                        break;
                    }
                }
                if (!foundNonZeroElement) {
                    for (int j = i + 1; j < numberVariables; ++j) {
                        if (!matrix[i][j].equals(ZERO)) {
                            swapColumns(i, j);
                            foundNonZeroElement = true;
                            break;
                        }
                    }
                }

                if (!foundNonZeroElement) {
                    for (int k = i + 1; !foundNonZeroElement && k < numberVariables; ++k) {
                        for (int j = i + 1; j < numberEquations; ++j) {
                            if (!matrix[j][k].equals(ZERO)) {
                                swapColumns(k, i);
                                swapRows(j, i);
                                foundNonZeroElement = true;
                                break;
                            }
                        }
                    }
                }

                if (!foundNonZeroElement) {
                    if (matrix[i][numberVariables].equals(ZERO)) {
                        numberSolutions = NumberSolutions.MANY;
                        continue;
                    } else {
                        numberSolutions = NumberSolutions.NONE;
                        return;
                    }
                }
            }

            if (!matrix[i][i].equals(ONE)) {
                divideRow(i, matrix[i][i]);
            }
            for (int j = i + 1; j < numberEquations && j < numberVariables; ++j) {
                final Complex k = Complex.multiply(MINUS_ONE, matrix[j][i]);
                if (!k.equals(ZERO)) {
                    addKRow1ToRow2(k, i, j);
                }
            }
        }
    }

    private void gausSecondStep() {
        for (int i = numberVariables - 1; i >= 0; --i) {
            for (int j = i - 1; j >= 0; --j) {
                final Complex k = Complex.multiply(MINUS_ONE, matrix[j][i]);
                if (!k.equals(ZERO)) {
                    addKRow1ToRow2(k, i, j);
                }
            }
        }
    }

    private void generateSolutions() {
        for (int i = 0; i < numberEquations && i < numberVariables; ++i) {
            solutionPartial[solutionIndexes[i]] = matrix[i][numberVariables];
            if (matrix[i][i].equals(ZERO)) {
                solutionGeneral[solutionIndexes[i]] = "x" + (solutionIndexes[i] + 1);
            } else {
                solutionGeneral[solutionIndexes[i]] = matrix[i][numberVariables].toString();
                for (int j = i + 1; j < numberVariables; ++j) {
                    if (matrix[i][j].equals(ONE)) {
                        solutionGeneral[solutionIndexes[i]] = solutionGeneral[solutionIndexes[i]] + " - x" +
                                (solutionIndexes[j] + 1);
                    } else if (!matrix[i][j].equals(ZERO)) {
                        solutionGeneral[solutionIndexes[i]] = solutionGeneral[solutionIndexes[i]] + " - x" +
                                (solutionIndexes[j] + 1) + " * (" + matrix[i][j].toString() + ")";
                    }
                }
            }
        }
    }

    private void printSolution() {
        if (verbose) {
            printSolutionInternal(new PrintWriter(System.out, true));
        }
    }

    private void printSolutionInternal(PrintWriter printWriter) {
        switch (numberSolutions) {
            case NONE:
                printWriter.println("No solutions");
                break;
            case ONE:
                printWriter.printf("%s\n", solutionPartial[0].toString());
                for (int i = 1; i < solutionPartial.length; ++i) {
                    printWriter.printf("%s\n", solutionPartial[i].toString());
                }
                break;
            case MANY:
                /*
                printWriter.printf("%s\n", solutionGeneral[0]);
                for (int i = 1; i < solutionGeneral.length; ++i) {
                    printWriter.printf("%s\n", solutionGeneral[i]);
                }
                */
                printWriter.println("Infinitely many solutions");
                break;
            default:
                assert (false);
                break;
        }
        printWriter.flush();
    }

    private void printErrorToFile(PrintWriter printWriter) {

        printWriter.println("No solutions");
        printWriter.flush();
    }




    private void swapColumns(int column1, int column2) {
        if (verbose) {
            System.out.printf("C%d <-> C%d\n", column1 + 1, column2 + 1);
        }
        final int n = matrix.length;
        for (int i = 0; i < n; ++i) {
            final Complex temp1 = matrix[i][column1];
            matrix[i][column1] = matrix[i][column2];
            matrix[i][column2] = temp1;
        }
        final int temp2 = solutionIndexes[column1];
        solutionIndexes[column1] = solutionIndexes[column2];
        solutionIndexes[column2] = temp2;
    }

    private void swapRows(int row1, int row2) {
        if (verbose) {
            System.out.printf("R%d <-> R%d\n", row1 + 1, row2 + 1);
        }
        for (int i = 0; i < numberVariables + 1; ++i) {
            final Complex temp = matrix[row1][i];
            matrix[row1][i] = matrix[row2][i];
            matrix[row2][i] = temp;
        }
    }
}
