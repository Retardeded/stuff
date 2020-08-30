package search;

import java.io.BufferedInputStream;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.util.*;

class SearchEngine {
    final List<String> people = new ArrayList<>();
    final Map<String, Set<Integer>> invertedIndex = new HashMap<>();
    PersonFinderTemplate finder;

    public void launch(String fileName) {
        try (Scanner scanner = new Scanner(new BufferedInputStream(new FileInputStream(fileName)))) {
            int lineNumber = 0;
            while (scanner.hasNextLine()) {
                String line = scanner.nextLine();
                people.add(line);

                for (String word :
                        line.toLowerCase().split(" ")) {
                    if (invertedIndex.containsKey(word)) {
                        invertedIndex.get(word).add(lineNumber);
                    } else {
                        Set<Integer> newSet = new HashSet<>();
                        newSet.add(lineNumber);
                        invertedIndex.put(word, newSet);
                    }
                }
                lineNumber++;
            }
        } catch (FileNotFoundException e) {
            System.out.println("File not found. Try to rerun program with correct name of file.");
            return;
        }

        Scanner scannerOfUserInput = new Scanner(System.in);

        while (true) {
            printMenu();
            switch (scannerOfUserInput.nextLine()) {
                case "1":
                    outer:
                    while (true) {
                        System.out.println("\nSelect a matching strategy: ALL, ANY, NONE");
                        String matchingStrategy = scannerOfUserInput.nextLine();
                        switch (matchingStrategy) {
                            case "ALL":
                                finder = new AllPersonFinder();
                                break outer;
                            case "ANY":
                                finder = new AnyPersonFinder();
                                break outer;
                            case "NONE":
                                finder = new NonePersonFinder();
                                break outer;
                            default:
                                System.out.println("Incorrect matching strategy");
                        }
                    }
                    System.out.println("\nEnter a name or email to search all suitable people.");
                    finder.findAPerson(scannerOfUserInput.nextLine());
                    break;
                case "2":
                    printAllPeople();
                    break;
                case "0":
                    System.out.println("\nBye!");
                    System.exit(0);
                default:
                    System.out.println("Incorrect option! Try again.");
            }
        }
    }

    public void printMenu() {
        System.out.println("\n=== Menu ===");
        System.out.println("1. Find a person");
        System.out.println("2. Print all people");
        System.out.println("0. Exit");
    }

    public void printAllPeople() {
        System.out.println("\n=== List of people ===");
        people.forEach(System.out::println);
    }

    private abstract class PersonFinderTemplate {
        Set<String> found = new HashSet<>();

        void findAPerson(String input) {
            find(input);
            print();
            found.clear();
        }

        void print() {
            if (found.isEmpty()) {
                System.out.println("No matching people found.");
            } else {
                found.forEach(System.out::println);
            }
        }

        abstract void find(String input);
    }

    private class AllPersonFinder extends PersonFinderTemplate {
        @Override
        void find(String input) {
            for (int line :
                    invertedIndex.getOrDefault(input.toLowerCase(), Set.of())) {
                found.add(people.get(line));
            }
        }
    }

    private class AnyPersonFinder extends PersonFinderTemplate {
        @Override
        public void find(String input) {
            for (String word :
                    input.toLowerCase().split(" ")) {
                for (int line :
                        invertedIndex.getOrDefault(word, Set.of())) {
                    found.add(people.get(line));
                }
            }
        }
    }

    private class NonePersonFinder extends PersonFinderTemplate {
        @Override
        public void find(String input) {
            Set<Integer> presentedInLines = new HashSet<>();
            for (String word :
                    input.toLowerCase().split(" ")) {
                presentedInLines.addAll(invertedIndex.getOrDefault(word, Set.of()));
            }
            for (int i = 0; i < people.size(); i++) {
                if (!presentedInLines.contains(i)) {
                    found.add(people.get(i));
                }
            }
        }
    }
}