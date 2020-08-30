package phonebook;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Hashtable;
import java.util.List;

public class Main {
    public static void main(String[] args) {
        long start;
        long stop;
        int count = 0;

        try {
            List<String> directory = Files.readAllLines(Paths.get(
                    "D:\\todel\\git\\Phone Book\\directory.txt"));
            List<String> names = Files.readAllLines(Paths.get(
                    "D:\\todel\\git\\Phone Book\\find.txt"));


            System.out.println("Start searching (linear search)...");
            // Linear searching.

            start = System.currentTimeMillis();
            for (String name : names) {
                for (String info : directory) {
                    if (info.split(" ", 2)[1].equals(name)) {
                        count++;
                    }
                }
            }
            stop = System.currentTimeMillis();

            System.out.printf("Found %d / %s entries. Time taken: %d min. %d sec %d ms.\n",
                    count, names.size(), (stop-start) / 60000,
                    ((stop-start) / 1000)%60, (stop-start) % 1000);

            count = 0;
            System.out.println("Start searching (bubble sort + jump search)...");
            start = System.currentTimeMillis();
            long startSortTime = System.currentTimeMillis();

            for (int i = 0; i < names.size(); i++) {
                for (int j = i; j < names.size(); j++) {
                    if (names.get(i).compareToIgnoreCase(names.get(j)) > 0) {
                        String lName = names.get(i);
                        String uName = names.get(j);
                        names.add(i, uName);
                        names.add(j, lName);
                        names.remove(i + 1);
                        names.remove(j + 1);

                    }
                }
            }


            long stopSortTime = System.currentTimeMillis();

            long startSearchTime = System.currentTimeMillis();
            for (String info : directory) {
                count += jumpSearch(names, info.split(" ", 2)[1]);
            }
            long stopSearchTime = System.currentTimeMillis();
            stop = System.currentTimeMillis();
            //System.out.println(names);

            System.out.printf("Found %d / %s entries. Time taken: %d min. %d sec %d ms.\n",
                    count, names.size(), (stop-start) / 60000,
                    ((stop-start) / 1000)%60, (stop-start) % 1000);

            System.out.printf("Sorting time: %d min. %d sec. %d ms.\n",
                    (stopSortTime - startSortTime) / 60000,
                    (int) ((stopSortTime - startSortTime) / 1000)%60,
                    (stopSortTime - startSortTime) % 1000);

            System.out.printf("Searching time: %d min. %d sec. %d ms.\n",
                    (stopSearchTime-startSearchTime) / 60000,
                    ((stopSearchTime-startSearchTime) / 1000)%60,
                    (stopSearchTime-startSearchTime) % 1000);


            count = 0;
            names.clear();
            names = Files.readAllLines(Paths.get(
                    "D:\\todel\\git\\Phone Book\\find.txt"));
            start = System.currentTimeMillis();
            System.out.println("Start searching (quick sort + binary search)...");

            long startSortTimeQ = System.currentTimeMillis();

            quicksort(names, 0, names.size()-1);


            long stopSortTimeQ = System.currentTimeMillis();

            long startSearchTimeQ = System.currentTimeMillis();
            for (String info : directory) {
                count += binarySearch(names, info.split(" ", 2)[1], 0, names.size()-1);
            }
            count = 0;
            for (String info : directory) {
                count += binarySearch(names, info.split(" ", 2)[1], 0, names.size()-1);
            }
            long stopSearchTimeQ = System.currentTimeMillis();
            stop = System.currentTimeMillis();
            //System.out.println(names);

            System.out.printf("Found %d / %s entries. Time taken: %d min. %d sec %d ms.\n",
                    count, names.size(), (stop-start) / 60000,
                    ((stop-start) / 1000)%60, (stop-start) % 1000);

            System.out.printf("Sorting time: %d min. %d sec. %d ms.\n",
                    (stopSortTimeQ - startSortTimeQ) / 60000,
                    (int) ((stopSortTimeQ - startSortTimeQ) / 1000)%60,
                    (stopSortTimeQ - startSortTimeQ) % 1000);

            System.out.printf("Searching time: %d min. %d sec. %d ms.\n",
                    (stopSearchTimeQ-startSearchTimeQ) / 60000,
                    ((stopSearchTimeQ-startSearchTimeQ) / 1000)%60,
                    (stopSearchTimeQ-startSearchTimeQ) % 1000);

            count = 0;
            names.clear();
            names = Files.readAllLines(Paths.get(
                    "D:\\todel\\git\\Phone Book\\find.txt"));
            start = System.currentTimeMillis();
            System.out.println("Start searching (hash table)...");

            long startSortTimeH = System.currentTimeMillis();

            Hashtable<String, String> hashtable = new Hashtable<>();
            for (String s: directory
            ) {
                String[] strings = s.split(" ", 2);
                hashtable.put(strings[1], strings[0]);
            }

            long stopSortTimeH = System.currentTimeMillis();

            long startSearchTimeH = System.currentTimeMillis();
            for(String person: names)
            {
                if(hashtable.get(person) != null)
                    count++;
            }
            long stopSearchTimeH = System.currentTimeMillis();
            stop = System.currentTimeMillis();
            //System.out.println(names);

            System.out.printf("Found %d / %s entries. Time taken: %d min. %d sec %d ms.\n",
                    count, names.size(), (stop-start) / 60000,
                    ((stop-start) / 1000)%60, (stop-start) % 1000);

            System.out.printf("Creating time: %d min. %d sec. %d ms.\n",
                    (stopSortTimeH - startSortTimeH) / 60000,
                    (int) ((stopSortTimeH - startSortTimeH) / 1000)%60,
                    (stopSortTimeH - startSortTimeH) % 1000);

            System.out.printf("Searching time: %d min. %d sec. %d ms.\n",
                    (stopSearchTimeH-startSearchTimeH) / 60000,
                    ((stopSearchTimeH-startSearchTimeH) / 1000)%60,
                    (stopSearchTimeH-startSearchTimeH) % 1000);
        } catch (IOException e) {
            e.printStackTrace();
        }

    }

    public static void quicksort(List<String> array, int left, int right) {
        if (left < right) {
            int pivotIndex = partition(array, left, right); // the pivot is already on its place
            quicksort(array, left, pivotIndex - 1); // sort the left subarray
            quicksort(array, pivotIndex + 1, right); // sort the right subarray
        }
    }

    public static int partition(List<String> array, int left, int right) {
        String pivot = array.get(right); // choose the rightmost element as the pivot
        int partitionIndex = left; // the first element greater than the pivot

        /* move large values into the right side of the array */
        for (int i = left; i < right; i++) {
            if (array.get(i).compareTo(pivot) <= 0) {
                swap(array, i, partitionIndex);
                partitionIndex++;
            }
        }

        swap(array, partitionIndex, right); // put the pivot on a suitable position
        return partitionIndex;
    }

    public static void swap(List<String> array, int i, int j) {
        String temp = array.get(i);
        array.set(i,array.get(j));
        array.set(j, temp);
    }

    public static int binarySearch(List<String> array, String target, int left, int right) {

        if (left > right) {
            return 0; // search interval is empty, the element is not found
        }

        int mid = left + (right - left) / 2; // the index of the middle element

        if (target.equals(array.get(mid))) {
            return 1; // the element is found, return its index
        } else if (target.compareTo(array.get(mid)) < 0) {
            return binarySearch(array, target, left, mid - 1); // go to the left subarray
        } else {
            return binarySearch(array, target, mid + 1, right); // go the the right subarray
        }
    }

    private static int jumpSearch(List<String> names, String target) {

        int currentRight = 0; // right border of the current block
        int prevRight = 0; // right border of the previous block

        /* If array is empty, the element is not found */
//        if (names.size() == 0) {
//            return -1;
//        }

        /* Check the first element */
        if (target.equalsIgnoreCase(names.get(currentRight))) {
            return 1;
        }

        /* Calculating the jump length over array elements */
        int jumpLength = (int) Math.sqrt(names.size());

        /* Finding a block where the element may be present */
        while (currentRight < names.size() - 1) {

            /* Calculating the right border of the following block */
            currentRight = Math.min(names.size() - 1, currentRight + jumpLength);

            if (target.compareToIgnoreCase(names.get(currentRight)) <= 0) {
                break; // Found a block that may contain the target element
            }

            prevRight = currentRight; // update the previous right block border
        }

        /* Doing linear search in the found block */
        return  backwardSearch(names, target, prevRight, currentRight);
    }

    private static int backwardSearch(List<String> array, String target, int leftExcl, int rightIncl) {
        for (int i = rightIncl; i > leftExcl; i--) {
            if (array.get(i).equals(target)) {
                return 1;
            }
        }
        return 0;
    }
}