import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.LinkedHashMap;
import java.util.Locale;
import java.util.Map;

public class WsppSecondG {
    public static void main(String[] args) {
        Map<String, Stat> map = new LinkedHashMap<>();
        int index = 1;
        int curLine = 0;
        Map<String, Integer> mapLine = new LinkedHashMap<>();

        try (NewScanner scanner = new NewScanner(new FileInputStream(args[0]), StandardCharsets.UTF_8)) {
            while (scanner.hasNext()) {
                String word = scanner.next(c -> Character.isLetter(c) || Character.getType(c) == Character.DASH_PUNCTUATION || c == '\'');

                if (word != null) {
                    word = word.toLowerCase(Locale.ROOT);

                    if (map.get(word) == null) {
                        map.put(word, new Stat(index));
                    } else {
                        map.get(word).addCount();
                    }

                    mapLine.merge(word, 1, Integer::sum);

                    if (mapLine.get(word) % 2 == 0) {
                        map.get(word).addToList(index);
                    }

                    index++;
                } else {
                    if (scanner.numLines - curLine > 0) {
                        curLine++;
                        mapLine = new LinkedHashMap<>();
                    }
                }
            }
        } catch (FileNotFoundException fileNotFoundException) {
            fileNotFoundException.printStackTrace();
        }

        try (BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(args[1]), StandardCharsets.UTF_8))) {
            String[] words = map.keySet().toArray(new String[0]);

            for (String word : words) {
                writer.write(word + " " + map.get(word).getCount());
                if (map.get(word) != null) {
                    writer.write(map.get(word).listToString());
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
