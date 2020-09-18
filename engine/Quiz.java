package engine;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import org.springframework.stereotype.Component;

import javax.persistence.*;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;
import java.util.*;


@Entity
@JsonInclude(JsonInclude.Include.NON_NULL)
public class Quiz {

    @JsonIgnore
    private static int idcount = 0;

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    int id;

    @NotNull
    @NotBlank(message = "Please, specify title")
    String title;

    @NotNull
    @NotBlank(message = "Please, specify text")
    String text;


    @NotNull
    @Size(min = 2)
    @ElementCollection
    List<String> options;

    @ElementCollection
    List<Integer> answer;

    public Quiz() {
        idcount = idcount + 1;
        this.id = idcount;
    }


    public int getId() {
        return id;
    }

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public String getText() {
        return text;
    }

    public void setText(String text) {
        this.text = text;
    }

    public List<String> getOptions() {

        return options;
    }

    public void setOptions(ArrayList<String> options) {
        this.options = options;
    }

    public List<Integer> getAnswer() {
        if(!(answer == null)) {
            Collections.sort(answer);}else{answer = new ArrayList<>();}
        return answer;
    }

    public void setAnswer(List<Integer> answer) {
        this.answer = answer;
    }

    public boolean solveQuiz(List<Integer> ans){
        if(!ans.isEmpty()) {
            Collections.sort(ans);
        }
        return (ans.equals(getAnswer()));
    }
}