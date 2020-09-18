package engine;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import javax.validation.ConstraintViolationException;
import javax.validation.Valid;
import java.util.*;

@RestController
public class QuizController {

    @Autowired
    QuizRepository quizRepository;

    @GetMapping(path = "/api/quizzes/{id}", produces = "application/json")
    public Object getApiQuiz(@PathVariable Integer id){
        if(quizRepository.existsById(id)){
            Quiz quiz = quizRepository.findById(id)
                    .orElseThrow();
            Map<String, Object> outmap = new TreeMap<>();
            outmap.put("id", id);
            outmap.put("title", quiz.getTitle());
            outmap.put("text", quiz.getText());
            outmap.put("options", quiz.getOptions());
            return outmap;
        }else{
            return new ResponseEntity(HttpStatus.NOT_FOUND);
        }
    }

    @PostMapping(path = "/api/quizzes", consumes = "application/json")
    public Map<String,Object> addQuizzies (@Valid @RequestBody Quiz quiz) {
        quiz = quizRepository.save(quiz);

        TreeMap<String,Object> outmap = new TreeMap<>();
        outmap.put("id", quiz.getId());
        outmap.put("title", quiz.getTitle());;
        outmap.put("text", quiz.getText());
        outmap.put("options", quiz.getOptions());
        return outmap;
    }

    @GetMapping(path = "/api/quizzes", produces = "application/json")
    public Object getAllQuiz(){

        ArrayList<Quiz> quizzes = (ArrayList<Quiz>) quizRepository.findAll();
        if(quizzes.size()!=0){
            ArrayList<HashMap<String, Object>> maps = new ArrayList<>();
            for (Quiz q : quizzes
            ) {
                HashMap<String, Object> outmap = new HashMap<>();
                outmap.put("id", q.getId());
                outmap.put("title", q.getTitle());
                outmap.put("text", q.getText());
                outmap.put("options", q.getOptions());
                maps.add(outmap);
            }
            return maps;
        }else{
            return new ResponseEntity(new ArrayList[0], HttpStatus.OK);
        }
    }


    @PostMapping(path = "/api/quizzes/{id}/solve", consumes = "application/json")
    public Object solveQuiz(@PathVariable Integer id, @RequestBody Map<String, ArrayList<Integer>> answer){
        Quiz quiz = quizRepository.findById(id)
                .orElseThrow();
        if (quiz!=null){
            HashMap <String,Object> ans = new HashMap<>();
            if(quiz.solveQuiz(answer.get("answer"))){
                ans.put("success",true);
                ans.put("feedback", "Congratulations, you're right!");
            }else{
                ans.put("success",false);
                ans.put("feedback", "Wrong answer! Please, try again.");
            }
            return ans;

        }else{
            return new ResponseEntity(HttpStatus.NOT_FOUND);
        }
    }

    @ExceptionHandler(ConstraintViolationException.class)
    @ResponseStatus(HttpStatus.BAD_REQUEST)
    ResponseEntity<String> handleConstraintViolationException(ConstraintViolationException e) {
        return new ResponseEntity<>("not valid due to validation error: " + e.getMessage(), HttpStatus.BAD_REQUEST);
    }

}