package life;

import javax.swing.*;
import java.awt.*;

public class GameOfLife extends JFrame {

    private final JLabel genLabel;
    private final JLabel aliveLabel;
    private final JLabel[][] labels;
    private final JPanel panel;
    private final int[][] universe;
    private int generation;
    private Timer timer;
    private final int delay = 300;

    public GameOfLife() {
        super("Game of life");
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        setSize(270, 350);
        setLocationRelativeTo(null);

        genLabel = new JLabel("Generation #0");
        genLabel.setName("GenerationLabel");
        genLabel.setBounds(10, 0, 100, 30);
        add(genLabel);

        aliveLabel = new JLabel("Alive: 0");
        aliveLabel.setName("AliveLabel");
        aliveLabel.setBounds(10, 30, 100, 30);
        add(aliveLabel);

        JToggleButton playToggleButton = new JToggleButton("Pause");
        playToggleButton.setName("PlayToggleButton");
        playToggleButton.setBounds(110, 5, 140, 20);
        playToggleButton.addActionListener(e -> {
            if (playToggleButton.isSelected()) {
                timer.stop();
                playToggleButton.setText("Paused");
            } else {
                timer.start();
                playToggleButton.setText("Pause");
            }
        });
        add(playToggleButton);

        JButton resetButton = new JButton("Reset");
        resetButton.setName("ResetButton");
        resetButton.setBounds(110, 35, 140, 20);
        resetButton.addActionListener(e -> restart());
        add(resetButton);

        int size = 10;
        panel = new JPanel();
        panel.setBounds(0, 60, 250, 250);
        panel.setLayout(new GridLayout(size, size, 0, 0));
        labels = new JLabel[size][size];
        universe = new int[size][size];

        for (int i = 0; i < size; i++) {
            for (int j = 0; j < size; j++) {
                labels[i][j] = new JLabel();
                labels[i][j].setBackground(Color.BLACK);
                panel.add(labels[i][j]);
            }
        }
        add(panel);
        createUniverse();

        setLayout(null);
        setVisible(true);

        timer = new Timer(delay, e -> updateGeneration());
        timer.start();
    }

    public void createUniverse() {
        int size = universe.length;
        for (int i = 0; i < size; i++) {
            for (int j = 0; j < size; j++) {
                universe[i][j] = (int) Math.round(Math.random());
                labels[i][j].setOpaque(universe[i][j] == 1);
            }
        }
    }

    private void updateGeneration() {
        generation++;
        int[][] p = java.util.Arrays.stream(universe).map(int[]::clone).toArray(int[][]::new);
        int aliveCount = 0;
        int size = universe.length;

        for (int i = 0; i < size; i++) {
            for (int j = 0; j < size; j++) {
                int n = i == 0 ? size - 1: i - 1;
                int w = j == 0 ? size - 1: j - 1;
                int s = i == size - 1? 0 : i + 1;
                int e = j == size - 1? 0 : j + 1;

                int alive = p[i][w] + p[i][e] + p[n][j] + p[s][j] + p[n][w] + p[n][e] + p[s][w] + p[s][e];

                if (p[i][j] == 1 && (alive < 2 || alive > 3)) {
                    universe[i][j] = 0;
                } else if (p[i][j] == 0 && alive == 3) {
                    universe[i][j] = 1;
                }
                if (universe[i][j] == 1) {
                    aliveCount++;
                }
                labels[i][j].setOpaque(universe[i][j] == 1);
            }
        }
        panel.repaint();
        genLabel.setText("Generation #" + generation);
        aliveLabel.setText("Alive: " + aliveCount);
    }

    public void restart() {
        generation = 0;
        createUniverse();
        updateGeneration();
    }

    public static void main(String[] args) {
        new GameOfLife();
    }
}