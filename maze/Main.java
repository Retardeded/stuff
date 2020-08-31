package maze;

import java.io.*;
import java.util.*;

public class Main {
    public static void main(String[] args) {
        Maze maze = new Maze(); //empty maze
        int[][] mazeMatrix = null;

        Scanner inputScanner = new Scanner(System.in);

        boolean printDisPlayAndSaveOption = false;
        printMenu(printDisPlayAndSaveOption);

        int option;
        String path;

        do {
            option = inputScanner.nextInt();
            inputScanner.nextLine(); //to consume the \n
            switch (option) {
                case 0: //exit
                    System.exit(0);
                    break;
                case 1: //generate
                    System.out.println("Enter the size of a new maze");
                    int size = inputScanner.nextInt();

                    if (size < 6) {
                        System.out.println("Size is too small!");
                    } else {
                        maze = new Maze(size, size); //squared maze
                        maze.generateMaze();
                        mazeMatrix = maze.getMazeMatrix();

                        // print the menu again
                        System.out.println();
                        printDisPlayAndSaveOption = true;
                        printMenu(printDisPlayAndSaveOption);
                    }
                    break;
                case 2: //load
                    System.out.println("Please write a path to a file");
                    path = inputScanner.nextLine();

                    File loadFile = new File(path);

                    try {
                        maze.loadMaze(loadFile);
                        System.out.println("File loaded successfully.\n");
                    } catch (FileNotFoundException e) {
                        System.out.println("There was a problem loading the file:" + path + ".");
                    }

                    printDisPlayAndSaveOption = true;
                    printMenu(printDisPlayAndSaveOption);

                    break;
                case 3: //save
                    if (printDisPlayAndSaveOption) {
                        System.out.println("Please write a path to a file");
                        path = inputScanner.nextLine();

                        File saveFile = new File(path);

                        try {
                            maze.saveMaze(saveFile);
                            System.out.println("File saved successfully.\n");
                        } catch (FileNotFoundException e) {
                            System.out.println("There was a problem saving the file to " + path + ".");
                        }

                        printMenu(printDisPlayAndSaveOption);

                    } else {
                        System.out.println("Incorrect option. Please try again.");
                        printMenu(false);
                    }
                    break;
                case 4: //display
                    if (printDisPlayAndSaveOption) {
                        maze.printMaze();
                    } else {
                        System.out.println("Incorrect option. Please try again.");
                        printMenu(false);
                    }
                    break;
                case 5: //display
                    if (printDisPlayAndSaveOption) {
                        maze.findEscape();
                        maze.printMazeAndEscape();
                    } else {
                        System.out.println("Incorrect option. Please try again.");
                        printMenu(false);
                    }
                    break;
                default:
                    System.out.println("Incorrect option. Please try again.");
                    printMenu(false);
                    break;
            }

        } while (option != 0);

    }

    public static void printMenu(boolean printDisplayOption) {
        System.out.println("=== Menu ===");
        System.out.println("1. Generate a new maze");
        System.out.println("2. Load a maze");
        if (printDisplayOption) {
            System.out.println("3. Save the maze");
            System.out.println("4. Display the maze");
            System.out.println("5. Find the escape");
        }
        System.out.println("0. Exit");
    }
}

class Point {
    public int x;
    public int y;
    public Point previousPoint;
    boolean explored = false;

    Point(int x, int y) {
        this.x = x;
        this.y = y;
    }

    Point(int x, int y, Point previousPoint) {
        this.x = x;
        this.y = y;
        this.previousPoint = previousPoint;
        this.explored = true;
    }
}

class Maze {
    private static final String WALL_CHAR = "\u2588";
    private static final String PASS_CHAR = " ";
    private static final String ESCAPE_CHAR = "//";
    public int[][] mazeMatrix;
    public Point entrancePoint;
    public Point exitPoint;
    public int[][] routeMatrix;
    private int width; //including walls
    private int height; //including walls
    private boolean debugMode = false; //prints several parts of the procedure for debugging
    private int nodesPerRow;
    private int totalNodes;

    public Maze() {
        // create empty maze
    }

    public Maze(int height, int width) {
        this.width = width;
        this.height = height;
    }

    public Maze(int height, int width, boolean debugMode) {
        this(height, width);
        this.debugMode = debugMode;
    }


    public void generateMaze() {
        int[][] adjacencyMatrix = generateAdjacencyMatrix();
        int[][] minimumSpanningTree = findMinimumSpanningTree(adjacencyMatrix);

        int[][] maze = new int[this.height][this.width];

        /*
            in this maze, the value of 0 means a pass and the value of 1 means a wall
         */

        //fills the maze with walls
        for (int[] row : maze) {
            Arrays.fill(row, 1);
        }

        //iterate over every node to add passes to the connections
        int currentNode = 0;
        for (int i = 0; i < this.height; i++) {
            for (int j = 0; j < this.width; j++) {
                //since all the nodes are added to the impair columns and rows (because the first one -0- is a wall)
                if (i != 0 && j != 0 && i != this.height - 1 && j != this.width - 1) {
                    if (i % 2 != 0 && j % 2 != 0) { //if it's a node
                        maze[i][j] = 0; // each node is also a pass

                        /*
                        iterate over the "currentNode" row of the minimum spanning tree to see which
                        node the "currentNode" is attached to.
                         */

                        for (int k = 0; k < minimumSpanningTree.length; k++) {
                            if (minimumSpanningTree[currentNode][k] == 1) {

                                // both of this equations were obtained empirically with trial and error.
                                int nodeRow = getNodeRow(k);
                                int nodeColumn = getNodeColumn(k);

                                //convert the space between the current node and the found node to a pass
                                //the node itself will become a pass when the condition
                                //written above is true (i % 2 != 0 && j % 2 != 0)

                                int edgeRow = (int) Math.floor(((double) nodeRow + i) / 2);
                                int edgeCol = (int) Math.floor(((double) nodeColumn + j) / 2);

                                maze[edgeRow][edgeCol] = 0;
                                //escape[edgeRow][edgeCol] = 1;
                            }
                        }
                        currentNode++;

                    }
                }

            }
        }

        //Generate the entrance and exit next to any of the nodes
        Random random = new Random();

        int entranceNode = random.nextInt(nodesPerRow); //any of the first nodes
        int exitNode = this.totalNodes - random.nextInt(nodesPerRow); // any of the last few

        int entranceNodeColumn = getNodeColumn(entranceNode);
        int entranceNodeRow = getNodeRow(entranceNode);

        int exitNodeColumn = getNodeColumn(exitNode);
        int exitNodeRow = getNodeRow(exitNode);

        maze[entranceNodeRow - 1][entranceNodeColumn] = 0;
        entrancePoint = new Point(entranceNodeColumn, entranceNodeRow-1);
        /*
        in the case of the exit it can happen that if the height is a pair number, there will be 2 rows of
        walls, so I have to check that as well.
         */

        maze[exitNodeRow + 1][exitNodeColumn] = 0;
        exitPoint = new Point(exitNodeColumn, exitNodeRow+1);
        if (exitNodeRow + 2 != this.height) {
            maze[exitNodeRow + 2][exitNodeColumn] = 0;
            exitPoint = new Point(exitNodeColumn, exitNodeRow+2);
        }

        this.mazeMatrix = maze;

        //print the maze
        this.printMaze();
    }

    public void findEscape() {

        Queue<Point> q = new LinkedList<>();
        int[][] maze = new int[this.height][this.width];
        HashMap<Integer,Boolean> exploredPoints = new HashMap<>();
        q.add(entrancePoint);
        Point currentPoint = null;
        while(q.size() > 0) {
            currentPoint = q.remove();
            exploredPoints.put(hashValue(currentPoint.x,currentPoint.y), true);

            if(currentPoint.x == exitPoint.x && currentPoint.y == exitPoint.y) {
                System.out.println("Path found");
                break;
            }

            if(currentPoint.x < mazeMatrix.length && exploredPoints.get(hashValue(currentPoint.x+1, currentPoint.y)) == null && mazeMatrix[currentPoint.y][currentPoint.x+1] == 0) {
                q.add(new Point(currentPoint.x+1, currentPoint.y, currentPoint));
            }
            if(currentPoint.x > 0 && exploredPoints.get(hashValue(currentPoint.x-1, currentPoint.y)) == null && mazeMatrix[currentPoint.y][currentPoint.x-1] == 0) {
                q.add(new Point(currentPoint.x-1, currentPoint.y, currentPoint));
            }
            if(currentPoint.y < mazeMatrix.length && exploredPoints.get(hashValue(currentPoint.x, currentPoint.y+1)) == null && mazeMatrix[currentPoint.y+1][currentPoint.x] == 0) {
                q.add(new Point(currentPoint.x, currentPoint.y+1, currentPoint));
            }
            if(currentPoint.y > 0 && exploredPoints.get(hashValue(currentPoint.x, currentPoint.y-1)) == null && mazeMatrix[currentPoint.y-1][currentPoint.x] == 0) {
                q.add(new Point(currentPoint.x, currentPoint.y-1, currentPoint));
            }
        }

        while(currentPoint.previousPoint != null) {
            maze[currentPoint.y][currentPoint.x] = 1;
            currentPoint = currentPoint.previousPoint;
        }
        maze[currentPoint.y][currentPoint.x] = 1;

        this.routeMatrix = maze;
    }

    public int hashValue(int x, int y) {
        return x * 1000 + y;
    }

    public void printMaze() {
        if (this.mazeMatrix == null) {
            System.out.println("The maze is null.");
        } else {
            for (int i = 0; i < this.height; i++) {
                for (int j = 0; j < this.width; j++) {
                    System.out.print(this.mazeMatrix[i][j] == 1 ? this.WALL_CHAR + this.WALL_CHAR : this.PASS_CHAR + this.PASS_CHAR);
                }
                System.out.print("\n");
            }
        }
    }

    public void printMazeAndEscape() {
        if (this.mazeMatrix == null) {
            System.out.println("The maze is null.");
        } else {
            for (int i = 0; i < this.height; i++) {
                for (int j = 0; j < this.width; j++) {

                        if(this.routeMatrix[i][j] == 1 )
                            System.out.print(this.ESCAPE_CHAR);
                        else
                            System.out.print(this.mazeMatrix[i][j] == 1 ? this.WALL_CHAR + this.WALL_CHAR : this.PASS_CHAR + this.PASS_CHAR);
                }
                System.out.print("\n");
            }
        }
    }

    public void saveMaze(File pathToFile) throws FileNotFoundException {
        try (PrintWriter writer = new PrintWriter(pathToFile)) {
            for (int i = 0; i < this.mazeMatrix.length; i++) {
                for (int j = 0; j < this.mazeMatrix[i].length; j++) {
                    writer.print(this.mazeMatrix[i][j] == 1 ? this.WALL_CHAR + this.WALL_CHAR : this.PASS_CHAR + this.PASS_CHAR);
                }
                writer.print("\n");
            }
        }
    }

    public void loadMaze(File pathToFile) throws FileNotFoundException {
        //since I don't know the size of the array being loaded to the class, I insert everything to an arrayList
        List<int[]> listOfLines = new ArrayList<>();
        String line;
        try (Scanner fileReader = new Scanner(pathToFile)) {
            while (fileReader.hasNext()) {
                line = fileReader.nextLine();

                int[] lineArray = new int[line.length() / 2];

                //iterate through all the line to form the int array
                //since every char is written twice, increment i by 2
                for (int i = 0; i < line.length(); i+=2) {
                    String charAtPosition = String.valueOf(line.charAt(i));

                    lineArray[i/2] = charAtPosition.equals(this.WALL_CHAR) ? 1 : 0;
                }

                listOfLines.add(lineArray);
            }

            //the convert the listOfLines to int[][] array
            int[][] matrix = new int[listOfLines.size()][];

            for (int i = 0; i < listOfLines.size(); i++) {
                matrix[i] = listOfLines.get(i);
            }

            //then assign it to the class matrix
            this.mazeMatrix = matrix;
            this.height = listOfLines.size();
            this.width = mazeMatrix[0].length;
            boolean hasEntrance = false;

            for(int i = 0; i < this.height; i++) {
                for(int j = 0; j < this.width; j++) {
                    if(i == 0 || i == this.height-1 || j == 0 || j == this.width-1) {
                        if(this.mazeMatrix[i][j] == 0) {
                            if(!hasEntrance) {
                                hasEntrance = true;
                                this.entrancePoint = new Point(j, i);
                            }
                            else
                                this.exitPoint = new Point(j, i);
                        }
                    }
                }
            }
        }
    }

    private int getNodeColumn(int nodeNumber) {
        /*
        returns the column in the maze where a certain node would be located, considering there are this.nodesPerRow
        and that they are placed left to right from row 1 col 1 every 2 columns and every 2 rows.
        obtained empirically.
         */
        return 1 + 2 * (nodeNumber - this.nodesPerRow * (int) Math.floor((nodeNumber / (double) this.nodesPerRow)));
    }

    private int getNodeRow(int nodeNumber) {
        /*
        returns the row in the maze where a certain node would be located, considering there are this.nodesPerRow
        and that they are placed left to right from row 1 col 1 every 2 columns and every 2 rows.
        obtained empirically.
         */
        return (int) Math.floor((nodeNumber / (double) this.nodesPerRow)) * 2 + 1;
    }


    private int[][] generateAdjacencyMatrix() {
        /*
        The total number of nodes are half the height ( minus 2 on each dimension for the wall, rounded up) times
        half the width ( minus 2 on each dimension for the wall, rounded up)

         for a matrix of w = 11  and h = 7:

         w  w   w   w   w   w   w   w   w   w   w
         w  0       1       2       3       4   w
         w      w        w      w       w       w
         w  5       6       7       8       9   w
         w      w        w      w       w       w
         w  10      11      12      13      14  w
         w  w   w   w   w   w   w   w   w   w   w

         the white spaces are just the edges of those nodes

         */

        this.nodesPerRow = (int) Math.ceil(((double) this.width - 2) / 2);
        int nodesPerColumn = (int) Math.ceil(((double) this.height - 2) / 2);

        this.totalNodes = nodesPerRow * nodesPerColumn;

        int[][] adjacencyMatrix = new int[totalNodes][totalNodes];

        Random rand = new Random();

        //iterate through all the possible nodes and create relationships with their right and upper neighbors
        int node = 0;
        for (int i = 0; i < nodesPerColumn; i++) {
            for (int j = 0; j < nodesPerRow; j++) {


                if (j < nodesPerRow - 1) {
                    // if it's not the last column, add an edge to its right neighbor
                    // since the adjacencyMatrix is symmetric, the same value is added to its symmetric counterpart

                    //random between 1 .. width * height + 1, empirically defined (and != 0)
                    int edgeRight = rand.nextInt(width * height) + 1;

                    adjacencyMatrix[node][node + 1] = edgeRight;
                    adjacencyMatrix[node + 1][node] = edgeRight;

                }

                if (i > 0) {
                    //it it's not the first row, add an edge to its upper neighbor
                    // since the adjacencyMatrix is symmetric, the same value is added to its symmetric counterpart

                    //random between 1 .. width * height + 1, empirically defined (and != 0)
                    int edgeTop = rand.nextInt(width * height) + 1;
                    int topNeighbor = node - nodesPerRow; //refer to the node matrix drawn on top

                    adjacencyMatrix[node][topNeighbor] = edgeTop;
                    adjacencyMatrix[topNeighbor][node] = edgeTop;
                }


                if (node == totalNodes - 1) {
                    break;
                } else {
                    node++;
                }
            }
        }

        if (this.debugMode) {
            //if it's in debug mode, print the adjacency matrix
            System.out.println("---------------- Adjacency Matrix ----------------");
            System.out.print("\t");
            for (int k = 0; k < totalNodes; k++) {
                System.out.print(k + "\t");
            }
            System.out.print("\n");
            for (int i = 0; i < totalNodes; i++) {
                System.out.print(i + "|\t");
                for (int j = 0; j < totalNodes; j++) {
                    System.out.print(adjacencyMatrix[i][j] + "\t");
                }
                System.out.print("\n");
            }
            System.out.println("---------------- /Adjacency Matrix ----------------");
        }


        return adjacencyMatrix;
    }

    private int[][] findMinimumSpanningTree(int[][] adjacencyMatrix) {
        //using Prim's algorithm finds the minimum spanning tree that connects all nodes.
        //starting at node 0 (first row of the adjacency matrix) iterate over all the matrix
        int[][] minimumSpanningTree = new int[adjacencyMatrix.length][adjacencyMatrix.length];
        Set<Integer> setOfAddedNodes = new HashSet<>();

        setOfAddedNodes.add(0);
        while (setOfAddedNodes.size() < adjacencyMatrix.length) { //iterate until the set of nodes contain every node.

            //the default min value is the max of the random number generator for the matrix, it's empirical.
            int minEdgeValue = 2 * (int) Math.pow(adjacencyMatrix.length, 2);

            int nextNode = 0;
            int currentNode = 0;

            for (int node : setOfAddedNodes) {

                // for each node that was previously added, check its connections to every other node.
                for (int j = 0; j < adjacencyMatrix.length; j++) {

                    /*
                        a node will be selected as a candidate only if:
                        * its edge has a value < than the previous min for this iteration
                        * it's != 0
                        * it's not contained in the set of already added nodes
                     */
                    if (adjacencyMatrix[node][j] < minEdgeValue && adjacencyMatrix[node][j] > 0) {
                        if (!setOfAddedNodes.contains(j)) {
                            minEdgeValue = adjacencyMatrix[node][j];
                            currentNode = node;
                            nextNode = j;
                        }
                    }
                }
            }

            //adds the next node (the one with the lower weight of all the available ones)
            setOfAddedNodes.add(nextNode);

            //adds the current edge to the tree with a value of 1
            minimumSpanningTree[currentNode][nextNode] = 1;
            minimumSpanningTree[nextNode][currentNode] = 1;
        }

        if (this.debugMode) {
            //if it's in debug mode, print the Minimum Spanning Tree matrix
            System.out.println("---------------- Minimum Spanning Tree (Prim) ----------------");
            System.out.print("\t");
            for (int k = 0; k < adjacencyMatrix.length; k++) {
                System.out.print(k + "\t");
            }
            System.out.print("\n");
            for (int i = 0; i < adjacencyMatrix.length; i++) {
                System.out.print(i + "|\t");
                for (int j = 0; j < adjacencyMatrix.length; j++) {
                    System.out.print(minimumSpanningTree[i][j] + "\t");
                }
                System.out.print("\n");
            }
            System.out.println("---------------- /Minimum Spanning Tree (Prim) ----------------");
        }

        return minimumSpanningTree;
    }

    public int[][] getMazeMatrix() {
        return mazeMatrix;
    }

    public int getWidth() {
        return width;
    }

    public void setWidth(int width) {
        this.width = width;
    }

    public int getHeight() {
        return height;
    }

    public void setHeight(int height) {
        this.height = height;
    }
}