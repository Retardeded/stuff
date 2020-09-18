package engine;

import java.util.Arrays;

public class Guess {
    private int[] answer;

    public int[] getAnswer() {
        return answer;
    }

    public void setAnswer(int[] answer) {
        this.answer = answer;
    }

    @Override
    public String toString() {
        return "Guess{" +
                "answer=" + Arrays.toString(answer) +
                '}';
    }
}