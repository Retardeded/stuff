package engine;

public class Answer {
    public final static Answer CORRECT_ANSWER = new Answer(true, "Congratulations, you're right!");
    public final static Answer WRONG_ANSWER = new Answer(false, "Wrong answer! Please, try again.");

    private final boolean success;
    private final String feedback;

    private Answer(boolean success, String feedback) {
        this.success = success;
        this.feedback = feedback;
    }

    public boolean getSuccess() {
        return success;
    }

    public String getFeedback() {
        return feedback;
    }
}