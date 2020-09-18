package engine;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.Size;
import java.util.Arrays;
import java.util.Objects;

public class Question {
    @NotBlank
    private final String title;

    @NotBlank
    private final String text;

    @NotEmpty
    @Size(min = 2)
    private final String[] options;

    @JsonProperty(access = JsonProperty.Access.WRITE_ONLY)
    private final int[] answer;

    private int id;

    @JsonCreator
    public Question(@JsonProperty("title") String title,
                    @JsonProperty("text") String text,
                    @JsonProperty("options") String[] options,
                    @JsonProperty("answer") int[] answer) {
        this.title = title;
        this.text = text;
        this.options = options.clone();
        Arrays.sort(options);
        this.answer = Objects.requireNonNullElse(answer, new int[]{});
    }

    public String getTitle() {
        return title;
    }

    public String getText() {
        return text;
    }

    public String[] getOptions() {
        return options.clone();
    }

    public boolean isCorrect(int[] options) {
        int[] copy = options.clone();
        Arrays.sort(copy);
        return Arrays.equals(copy, answer);
    }

    public void setId(int id) {
        this.id = id;
    }

    public int getId() {
        return id;
    }

    @Override
    public String toString() {
        return "Question{" +
                "title='" + title + '\'' +
                ", text='" + text + '\'' +
                ", options=" + Arrays.toString(options) +
                ", answer=" + Arrays.toString(answer) +
                ", id=" + id +
                '}';
    }
}