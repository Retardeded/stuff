package minesweeper;

import java.util.Scanner;
import java.util.Set;
import java.util.HashSet;
import java.util.Random;

class Land {
    public static final char GRASS = '.';
    public static final char FREE = '/';
    public static final char FLAG = '*';
    public static final char MINE = 'X';

    public static final int HEIGHT = 9;
    public static final int WIDTH = 9;

    public static int NUM_OF_MINES;
    public static boolean GAME_OVER = false;
}

class Field {
    private final char[][] field = new char[Land.HEIGHT][Land.WIDTH];
    private final Set<Integer> mines = new HashSet<>();
    private final Set<Integer> flags = new HashSet<>();
    private final Set<Integer> untouched = new HashSet<>();

    public Field() {
        for (int i = 0; i < Land.HEIGHT; i++) {
            for (int j = 0; j < Land.WIDTH; j++) {
                field[i][j] = Land.GRASS;
                untouched.add(i * Land.HEIGHT + j);
            }
        }
    }

    public void setMines(int i, int j) {
        Random random = new Random();
        int forbidden = i * Land.HEIGHT + j;

        while (mines.size() < Land.NUM_OF_MINES) {
            int tmp = random.nextInt(Land.HEIGHT * Land.WIDTH);
            if (tmp != forbidden) {
                mines.add(tmp);
            }
        }
    }

    public char countOfNearMines(int i, int j) {
        int count = getCell(i - 1, j - 1) +
                getCell( i - 1, j) +
                getCell( i - 1, j + 1) +
                getCell( i, j - 1) +
                getCell( i, j + 1) +
                getCell( i + 1, j - 1) +
                getCell( i + 1, j) +
                getCell( i + 1, j + 1);
        if (count == 0) {
            return Land.FREE;
        } else {
            return (char) (count + 48);
        }
    }

    public int getCell(int i, int j) {
        if (i >= 0 && i < Land.HEIGHT && j >=0 && j < Land.WIDTH &&
                mines.contains(i * Land.HEIGHT + j)) {
            return 1;
        } else {
            return 0;
        }
    }

    public boolean isOver() {
        if (Land.GAME_OVER) {
            return true;
        }
        if (Land.NUM_OF_MINES > mines.size()) {
            return false;
        } else {
            boolean tmp = flags.equals(mines) || untouched.equals(mines);
            if (tmp) {
                System.out.print(toString());
                System.out.println("Congratulations! You found all mines!");
            }
            return tmp;
        }
    }

    public void setFlag(int i, int j) {
        int pos = i * Land.HEIGHT + j;
        if (flags.contains(pos)) {
            flags.remove(pos);
        } else {
            flags.add(pos);
        }
    }

    public void claimFree(int i, int j) {
        if (i < 0 || i > Land.HEIGHT - 1) return;
        if (j < 0 || j > Land.WIDTH - 1) return;

        if (Land.NUM_OF_MINES > mines.size()) {
            setMines(i, j);
        }
        if (mines.contains(i * Land.HEIGHT + j)) {
            gameLost();
        } else if (field[i][j] == Land.GRASS) {
            untouched.remove(i * Land.HEIGHT + j);
            flags.remove(i * Land.HEIGHT + j);
            char count = countOfNearMines(i, j);
            field[i][j] = count;
            if (count == Land.FREE) {
                claimFree(i - 1, j - 1);
                claimFree(i - 1, j );
                claimFree(i - 1, j + 1);
                claimFree(i, j - 1);
                claimFree(i, j + 1);
                claimFree(i + 1, j - 1);
                claimFree(i + 1, j);
                claimFree(i + 1, j + 1);
            }
        }
    }

    public void gameLost() {
        for (Integer mine : mines) {
            int i = mine / Land.HEIGHT;
            int j = mine % Land.HEIGHT;
            field[i][j] = Land.MINE;
        }
        System.out.print(toString());
        System.out.println("You stepped on a mine and failed!");
        flags.clear();
        Land.GAME_OVER = true;
    }

    @Override
    public String toString() {
        StringBuilder string = new StringBuilder();

        string.append(" │123456789│\n");
        string.append("—│—————————│\n");

        for (int i = 0; i < Land.HEIGHT; i++) {
            string.append((char) (i + 49));
            string.append("|");

            for (int j = 0; j < Land.WIDTH; j++) {
                if (flags.contains(i * Land.HEIGHT + j)) {
                    string.append(Land.FLAG);
                } else {
                    string.append(field[i][j]);
                }
            }
            string.append("|\n");
        }
        string.append("—│—————————│\n");

        return string.toString();
    }
}

public class Main {
    public static void main(String[] args) {

        Field field = new Field();

        Scanner scanner = new Scanner(System.in);

        System.out.println("How many mines do you want on the field? ");
        Land.NUM_OF_MINES = scanner.nextInt();

        while (!field.isOver()) {
            System.out.print(field);
            System.out.print("Set/unset mines marks or claim a cell as free: ");
            int j = scanner.nextInt() - 1;
            int i = scanner.nextInt() - 1;
            String operation = scanner.nextLine();

            if (operation.contains("mi")) {
                field.setFlag(i, j);
            } else {
                field.claimFree(i, j);
            }
        }
    }
}