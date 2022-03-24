import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.LinkedHashMap;
import java.util.Locale;
import java.util.Map;

public class Wspp {
    public static void main(String[] args) {
        Map<String, IntList> map = new LinkedHashMap<>();
        int index = 1;

        try (NewScanner scanner = new NewScanner(new FileInputStream(args[0]), StandardCharsets.UTF_8)) {
            while (scanner.hasNext()) {
                String word = scanner.next(c -> Character.isLetter(c) || Character.getType(c) == Character.DASH_PUNCTUATION || c == '\'');

                if (word != null) {
                    word = word.toLowerCase(Locale.ROOT);

                    if (map.get(word) == null) {
                        map.put(word, new IntList(index));
                    } else {
                        map.get(word).add(index);
                    }

                    index++;
                }
            }
        } catch (FileNotFoundException fileNotFoundException) {
            fileNotFoundException.printStackTrace();
        }

        try (BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(args[1]), StandardCharsets.UTF_8))) {
            String[] words = map.keySet().toArray(new String[0]);

            for (String word : words) {
                if (map.get(word) != null) {
                    writer.write(word + " " + map.get(word).len + " " + map.get(word).listToString(false));
                }
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
