package search;

public class Main {

    public static void main(String[] args) {
        if (args[0].equals("--data")) {
            new SearchEngine().launch(args[1]);
        } else {
            System.out.println("Provide name of file to scan with \"--data <filename>\"");
        }
    }
}