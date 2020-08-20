package flashcards;


import java.io.*;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;

public class Main {

    static String mistakesString = "mistakes:begin";

    public static void main(String[] args) throws IOException {

        boolean importingStuff = false;
        boolean exportingStuff = false;

        String defaultImportFile = "";
        String defaultExportFile = "";

        if(args.length >= 2)
        {
            if(args[0].equals("-import")) {
                importingStuff = true;
                defaultImportFile = args[1];
            }
            else if(args[0].equals("-export")) {
                exportingStuff = true;
                defaultExportFile = args[1];
            }

            if(args.length == 4)
            {
                if(args[2].equals("-import")) {
                    importingStuff = true;
                    defaultImportFile = args[3];
                }
                else if(args[2].equals("-export")) {
                    exportingStuff = true;
                    defaultExportFile = args[3];
                }
            }
        }




        Scanner scanner = new Scanner(System.in);
        TreeMap<String,String> cards = new TreeMap<>();
        TreeMap<String,String> definitions = new TreeMap<>();
        TreeMap<String,Integer> mistakes = new TreeMap<>();

        ArrayList<String> log = new ArrayList<>();

        String menuString = "Input the action (add, remove, import, export, ask, exit, log, hardest card, reset stats):";
        String filename = "mySave";
        //String filePath = "D:\\todel\\git\\testing\\";

        if(importingStuff)
            ImportCards(scanner, cards, definitions, log, mistakes, defaultImportFile);

        while(true)
        {
            System.out.println(menuString);
            log.add(menuString);
            String query = scanner.nextLine();
            log.add(query);

            if(query.equals("add")) {
                AddCard(scanner, cards, definitions, log);
            }
            else if(query.equals("remove")) {

                RemoveCard(scanner, cards, definitions, log, mistakes);
            }
            else if(query.equals("import"))
            {
                ImportCards(scanner, cards, definitions, log, mistakes, "");
            }
            else if(query.equals("export"))
            {
                ExportCards(scanner, cards, log, mistakes, "");
            }
            else if(query.equals("log"))
            {
                ExportLog(scanner, log);
            }
            else if(query.equals("ask"))
            {
                AskQuestions(scanner, cards, definitions, log, mistakes);
            }
            else if(query.equals("hardest card"))
            {
                PrintHardestCard(log, mistakes);
            }
            else if(query.equals("reset stats"))
            {
                ResetStatistics(log, mistakes);
            }
            else if(query.equals("exit"))
            {
                System.out.println("Bye bye!");

                if(exportingStuff)
                    ExportCards(scanner, cards, log, mistakes, defaultExportFile);

                break;
            }

            System.out.println();
            log.add("");
        }

    }

    private static void ResetStatistics(ArrayList<String> log, TreeMap<String, Integer> mistakes)
    {
        for(String card: mistakes.keySet())
        {
            mistakes.replace(card,0);
        }

        System.out.println("Card statistics has been reset.");
        log.add("Card statistics has been reset.");
    }

    private static void PrintHardestCard(ArrayList<String> log, TreeMap<String, Integer> mistakes) {

        int mostMistakes = 0;

        ArrayList<String> hardestCards = new ArrayList<>();

        if(mistakes.size() == 0)
        {
            System.out.println("There are no cards with errors.");
            log.add("There are no cards with errors.");
            return;
        }

        for(String card: mistakes.keySet())
        {
            if(mistakes.get(card) > mostMistakes)
            {
                mostMistakes = mistakes.get(card);
                //System.out.println(card + " : " + mistakes.get(card));
            }
        }

        if(mostMistakes == 0)
        {
            System.out.println("There are no cards with errors.");
            log.add("There are no cards with errors.");
            return;
        }

        for(String card: mistakes.keySet())
        {
            if(mistakes.get(card) == mostMistakes)
            {
                hardestCards.add(card);
            }
        }

        String output = "The hardest cards are";
        boolean oneHardestCard = false;

        if(hardestCards.size() == 1)
            oneHardestCard = true;

        if(oneHardestCard)
            output = "The hardest card is";

        for(int i = 0; i < hardestCards.size(); i++)
        {
            if(i < hardestCards.size()-1)
            {
                output += " \"" + hardestCards.get(i) + "\",";
            }
            else
            {
                output += " \"" + hardestCards.get(i) + "\".";
            }
        }

        if(oneHardestCard)
            output += " You have " + mostMistakes + " errors answering it.";
        else
            output += " You have " + mostMistakes + " errors answering them.";

        System.out.println(output);
        log.add(output);

    }

    private static void AskQuestions(Scanner scanner, TreeMap<String, String> cards, TreeMap<String, String> definitions, ArrayList<String> log, TreeMap<String, Integer> mistakes) {
        System.out.println("How many times to ask?");
        log.add("How many times to ask?");
        int n = Integer.parseInt(scanner.nextLine());
        log.add(Integer.toString(n));


        while(n > 0)
        {
            for(String card: cards.keySet())
            {
                System.out.println("Print the definition of \"" + card + "\":");
                log.add("Print the definition of \"" + card + "\":");

                String answer = scanner.nextLine();
                log.add(answer);

                if(answer.equals(cards.get(card))) {
                    System.out.println("Correct answer.");
                    log.add("Correct answer.");
                }
                else
                {
                    if(definitions.get(answer) != null)
                    {
                        System.out.println("Wrong answer. The correct one is \"" + cards.get(card) + "\", you've just written the definition of \"" + definitions.get(answer) + "\".");
                        log.add("Wrong answer. The correct one is \"" + cards.get(card) + "\", you've just written the definition of \"" + definitions.get(answer) + "\".");
                    }
                    else
                    {
                        System.out.println("Wrong answer. The correct one is \"" + cards.get(card) + "\".");
                        log.add("Wrong answer. The correct one is \"" + cards.get(card) + "\".");
                    }

                    if(mistakes.get(card) == null)
                    {
                        mistakes.put(card, 1);
                    }
                    else
                    {
                        mistakes.replace(card,mistakes.get(card) + 1);
                    }
                }

                n--;
                if(n == 0)
                    break;
            }
        }


    }

    private static void ExportLog(Scanner scanner, ArrayList<String> log) throws IOException {
        System.out.println("File name:");
        log.add("File name:");
        String name = scanner.nextLine();
        log.add(name);

        PrintWriter writer = new PrintWriter(name);
        writer.print("");
        writer.close();

        BufferedWriter buffer = new BufferedWriter(new FileWriter(name, true));

        for(int i = 0; i < log.size(); i++)
        {
            String line = log.get(i);
            buffer.write(line);
            buffer.newLine();
        }

        buffer.close();

        System.out.println("The log has been saved.");
        log.add("The log has been saved.");
    }

    private static void ExportCards(Scanner scanner, TreeMap<String, String> cards, ArrayList<String> log, TreeMap<String, Integer> mistakes, String fileName) throws IOException {

        // import and export, getting current dir

        if(fileName.equals(""))
        {
            System.out.println("File name:");
            log.add("File name:");
            fileName = scanner.nextLine();
            log.add(fileName);
        }

        PrintWriter writer = new PrintWriter(fileName);
        writer.print("");
        writer.close();

        BufferedWriter buffer = new BufferedWriter(new FileWriter(fileName, true));


        for(String card: cards.keySet())
        {
            String line = card + ":" + cards.get(card);
            buffer.write(line);
            buffer.newLine();
        }

        buffer.write(mistakesString);
        buffer.newLine();

        for (String card: mistakes.keySet())
        {
            String line = card + ":" + mistakes.get(card);
            buffer.write(line);
            buffer.newLine();
        }

        buffer.close();

        System.out.println(cards.size() +  " cards have been saved.");
        log.add(cards.size() +  " cards have been saved.");

    }

    private static void ImportCards(Scanner scanner, TreeMap<String, String> cards, TreeMap<String, String> definitions, ArrayList<String> log, TreeMap<String, Integer> mistakes, String fileName) throws IOException {

        if(fileName.equals(""))
        {
            System.out.println("File name:");
            log.add("File name:");
            fileName = scanner.nextLine();
            log.add(fileName);
        }

        int numberOfCards = 0;
        File f = new File(fileName);
        if(f.exists() && !f.isDirectory()) {

            List<String> data = Files.readAllLines(Paths.get(fileName));
            boolean importingMistakes = false;

            for(int i = 0; i < data.size(); i++) {

                //System.out.println(data.get(i));
                //System.out.println();

                if(data.get(i).equals(mistakesString))
                {
                    importingMistakes = true;
                    numberOfCards = i;
                    continue;
                }

                String[] parts = data.get(i).split(":");

                if(!importingMistakes)
                {
                    if(cards.get(parts[0]) != null)
                    {
                        definitions.remove(cards.get(parts[0]));
                        cards.remove(parts[0]);
                    }

                    cards.put(parts[0], parts[1]);
                    definitions.put(parts[1], parts[0]);
                }
                else
                {
                    if(mistakes.get(parts[0]) != null)
                    {
                        mistakes.remove(cards.get(parts[0]));
                    }

                    mistakes.put(parts[0], Integer.parseInt(parts[1]));
                }


            }

            System.out.println(numberOfCards + " cards have been loaded.");
            log.add(numberOfCards + " cards have been loaded.");
        }
        else
        {
            System.out.println("File not found.");
            log.add("File not found.");
        }

        // https://hyperskill.org/projects/66/stages/359/implement
    }

    private static void RemoveCard(Scanner scanner, TreeMap<String, String> cards, TreeMap<String, String> definitions, ArrayList<String> log, TreeMap<String, Integer> mistakes) {
        System.out.println("The card:");
        log.add("The card:");
        String card = scanner.nextLine();
        log.add(card);
        if(!cards.containsKey(card))
        {
            System.out.println("Can't remove \"" + card + "\": there is no such card.");
            log.add("Can't remove \"" + card + "\": there is no such card.");
        }
        else
        {
            definitions.remove(cards.get(card));
            cards.remove(card);
            mistakes.remove(card);

            System.out.println("The card has been removed.");
            log.add("The card has been removed.");
        }
    }

    private static void AddCard(Scanner scanner, TreeMap<String, String> cards, TreeMap<String, String> definitions, ArrayList<String> log) {
        System.out.println("The card:");
        log.add("The card:");
        String card;
        String definition;

        card = scanner.nextLine();
        log.add(card);
        if(!cards.containsKey(card)) {
            System.out.println("The definition of the card:");
            log.add("The definition of the card:");
            definition = scanner.nextLine();
            log.add(definition);
            if(!cards.containsValue(definition)){
                cards.put(card, definition);
                definitions.put(definition, card);

                System.out.println("The pair (\"" + card + "\":\"" + definition + "\") has been added.");
                log.add("The pair (\"" + card + "\":\"" + definition + "\") has been added.");
            }
            else
                System.out.println("The definition \"" + definition +"\" already exists.");
            log.add("The definition \"" + definition +"\" already exists.");
        }
        else
        {
            System.out.println("The card \"" + card + "\" already exists.");
            log.add("The card \"" + card + "\" already exists.");
        }
    }
}
