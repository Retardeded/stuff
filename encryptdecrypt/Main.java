package encryptdecrypt;

import java.io.*;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Scanner;

public class Main {
    public static void main(String[] args) throws IOException {

        System.out.println(Arrays.toString(args));
        HashMap<String,String> modes = new HashMap<>();
        String data = "";

        for(int i = 0; i < args.length-1; i++) {
            modes.put(args[i], args[i+1]);
        }

        String mode = modes.getOrDefault("-mode","enc");
        int key = Integer.parseInt(modes.getOrDefault("-key", "0"));
        String alg = modes.getOrDefault("-alg", "shift");
        data = modes.getOrDefault("-data", "");
        if(data.equals("")) {
            File input = new File(modes.get("-in"));
            if(input != null) {
                try (BufferedReader br = new BufferedReader(new FileReader(input))) {
                    String line;
                    while ((line = br.readLine()) != null) {
                        data += line;
                    }
                }
            }
        }
        File output = new File(modes.get("-out"));
        if(modes.get("-out") != null) {
            //System.out.println(output.getName());
            output.createNewFile();
        }
        char[] chars = data.toCharArray();

        switch (mode) {
            case "enc":
                getEncryption(chars, key, output, alg);
                break;
            case "dec":
                getDecryption(chars, key, output, alg);
                break;
            default:
                System.out.println("Error");
                break;
        }
    }

    public static void getDecryption(char[] chars, int shift, File output, String alg) throws IOException {

        StringBuilder sb = new StringBuilder();
        if(alg.equals("unicode"))
            unicodeDecrypt(chars, shift, sb);
        else {
            shiftDecrypt(chars, shift, sb);
        }
        if(output.isFile()) {
                FileWriter writeFile = new FileWriter(output);
                writeFile.write(sb.toString());
                writeFile.close();
            }
            else {
                System.out.println(sb.toString());
            }
    }

    private static void unicodeDecrypt(char[] chars, int shift, StringBuilder sb) {
        for (char item : chars) {
            char shiftItem = (char) (item - shift);
            sb.append(shiftItem);
        }
    }

    private static void shiftDecrypt(char[] chars, int shift, StringBuilder sb) {
        for (char item : chars) {
                char shiftItem = item;
                    if (item >= 'a' && item <= 'z') {
                        shiftItem = (char) (((item - 'a' + 26 - shift) % 26 ) + 'a');
                    }
               sb.append(shiftItem);
            }
    }

    private static void unicodeEncrypt(char[] chars, int shift, StringBuilder sb) {
        for (char item : chars) {
            char shiftItem = (char) (item + shift);
            sb.append(shiftItem);
        }
    }

    private static void shiftEncrypt(char[] chars, int shift, StringBuilder sb) {
        for (char item : chars) {
            char shiftItem = item;
            if (item >= 'a' && item <= 'z') {
                shiftItem = (char) (((item - 'a' + shift) % 26 ) + 'a');
            }
            sb.append(shiftItem);
        }
    }

    public static void getEncryption(char[] chars, int shift, File output, String alg) throws IOException {

        StringBuilder sb = new StringBuilder();
        if(alg.equals("unicode"))
            unicodeEncrypt(chars, shift, sb);
        else {
            shiftEncrypt(chars, shift, sb);
        }
        if(output.isFile()) {
            FileWriter writeFile = new FileWriter(output);
            writeFile.write(sb.toString());
            writeFile.close();
        }
        else {
            System.out.println(sb.toString());
        }
    }
}