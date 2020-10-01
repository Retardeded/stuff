package readability;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

public class Main {
    public static void main(String[] args) {

        try {
            new Application(
                    new TextAnalysis(
                            Files.readString(
                                    Path.of(args[0])))).run();
        } catch (IOException e) {
            e.getMessage();
        }
    }
}