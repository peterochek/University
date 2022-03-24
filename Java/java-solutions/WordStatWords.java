import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Locale;

public class WordStatWords {
    private static boolean isTextual(char c) {
        return Character.isLetter(c) || Character.getType(c) == Character.DASH_PUNCTUATION || c == '\'';
    }

    public static void main(String[] args) {
        HashMap<String, Integer> map = new HashMap<>();
        String instance;

        try {
            BufferedReader reader = new BufferedReader(new InputStreamReader(new FileInputStream(args[0]), StandardCharsets.UTF_8));

            StringBuilder sb = new StringBuilder();

            while (true) {
                int code = reader.read();

                char ch = (char) code;

                if (code == -1) {
                    break;
                }

                if (isTextual(ch)) {
                    sb.append(ch);
                } else {
                    if (sb.length() > 0) {
                        instance = sb.toString();
                        map.merge(instance.toLowerCase(Locale.ROOT), 1, Integer::sum);
                        sb.setLength(0);
                    }
                }
            }
            reader.close();
        } catch (FileNotFoundException e) {
            System.out.println(e.getMessage());
        } catch (IOException e) {
            e.printStackTrace();
        }

        try (BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(args[1]), StandardCharsets.UTF_8))) {
            String[] words = map.keySet().toArray(new String[0]);
            Arrays.sort(words);

            for (String word : words) {
                writer.write(word + " " + map.get(word));
                writer.newLine();
            }
        } catch (
                FileNotFoundException e) {
            System.out.println("Cannot open file: " + e.getMessage());
        } catch (
                IOException e) {
            System.out.println("Cannot write: " + e.getMessage());
        }
    }
}
