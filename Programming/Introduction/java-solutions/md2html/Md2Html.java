package md2html;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static md2html.Constants.*;

public class Md2Html {
    private static BufferedWriter writer;
    private static List<String> p;

    static Map<String, Integer> tagsString = new HashMap<>(Map.of(
            EMPHASIS_STAR, 0,
            EMPHASIS_UNDERLINE, 0,
            STRONG, 0,
            STRIKEOUT, 0,
            CODE, 0,
            PRE, 0,
            ESCAPING, 0
    ));

    public static void main(String[] args) {
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(new FileInputStream(args[0]), StandardCharsets.UTF_8))) {
            writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(args[1]), StandardCharsets.UTF_8));
            p = new ArrayList<>();

            while (true) {
                String line = reader.readLine();
                if (line == null) {
                    break;
                } else {
                    if (line.isEmpty()) {
                        printParagraph();
                        p.clear();
                    } else {
                        p.add(line);
                    }
                }
            }

            printParagraph();

            reader.close();
            writer.close();
        } catch (IOException e) {
            //asd
        }

    }

    private static void printParagraph() throws IOException {
        if (p.isEmpty()) {
            return;
        }
        int headerLevel = headingLevel(p.get(0));
        if (headerLevel > 0) {
            String newParagraph = p.get(0).substring(headerLevel + 1);
            p.set(0, newParagraph);
        }
        printOuter(headerLevel);
        writer.newLine();
    }

    private static int headingLevel(String line) {
        int depth = 0;
        while (line.charAt(depth) == '#') {
            depth++;
        }
        if (Character.isWhitespace(line.charAt(depth))) {
            return depth;
        } else {
            return -1;
        }
    }

    private static void printOuter(int depth) throws IOException {
        writer.write(depth > 0 ? "<h" + depth + ">" : "<p>");
        printInside();
        writer.write(depth > 0 ? "</h" + depth + ">" : "</p>");
    }

    private static void printInside() throws IOException {
        for (String line : p) {
            for (int i = 0; i < line.length(); i++) {
                char c = line.charAt(i);
                if (tagsString.get(mdHtml.get(c)) != null) {
                    if (c == '*') {
                        if (isDouble(i, line, c)) {
                            i++;
                            tagsString.merge(STRONG, 1, Integer::sum);
                        } else {
                            tagsString.merge(EMPHASIS_STAR, 1, Integer::sum);
                        }
                    } else if (c == '_') {
                        if (isDouble(i, line, c)) {
                            i++;
                            tagsString.merge(STRONG, 1, Integer::sum);
                        } else {
                            tagsString.merge(EMPHASIS_UNDERLINE, 1, Integer::sum);
                        }
                    } else if (c == '-') {
                        i++;
                        tagsString.merge(STRIKEOUT, 1, Integer::sum);
                    } else if (c == '`') {
                        if (isTriple(i, line, c)) {
                            i += 2;
                            tagsString.merge(PRE, 1, Integer::sum);
                        } else {
                            tagsString.merge(CODE, 1, Integer::sum);
                        }
                    } else if (c == '\\') {
                        i++;
                    }
                }
            }
        }

        tagsString.replaceAll((key, value) -> value - value % 2);

        for (int i = 0; i < p.size(); i++) {
            if (i > 0) writer.newLine();

            String line = p.get(i);

            for (int j = 0; j < line.length(); j++) {
                char c = line.charAt(j);


                if (c == '*') {
                    j = forDouble(j, line, c, EMPHASIS_STAR) ? j + 1 : j;
                } else if (c == '_') {
                    j = forDouble(j, line, c, EMPHASIS_UNDERLINE) ? j + 1 : j;
                } else if (c == '-') {
                    if (isDouble(j, line, c) && tagsString.get(mdHtml.get(c)) > 0) {
                        if (tagsString.get(mdHtml.get(c)) % 2 == 0) {
                            writer.write("<s>");
                        } else {
                            writer.write("</s>");
                        }
                        j++;
                        tagsString.put(STRIKEOUT, tagsString.get(STRIKEOUT) - 1);
                    } else {
                        writer.write(line.charAt(j));
                    }
                } else if (c == '`') {
                    if (isTriple(j, line, c) && tagsString.get(PRE) > 0) {
                        writer.write("<pre>");
                        j += 3;

                        outer:
                        while (i < p.size()) {
                            while (j < line.length()) {
                                if (line.charAt(j) == '`' && isTriple(j, line, '`')) {
                                    j += 2;
                                    writer.write("</pre>");
                                    break outer;
                                }
                                writer.write(line.charAt(j));
                                j++;
                            }
                            writer.newLine();
                            j = 0;
                            i++;
                            if (i < p.size()) {
                                line = p.get(i);
                            }
                        }

                        tagsString.put(PRE, tagsString.get(PRE) - 2);
                    } else {
                        if (tagsString.get(mdHtml.get(c)) % 2 == 0 && tagsString.get(CODE) > 0) {
                            writer.write("<code>");
                        } else {
                            writer.write("</code>");
                        }

                        tagsString.put(CODE, tagsString.get(CODE) - 1);
                    }
                } else if (c == '\\') {
                    j++;
                    writer.write(line.charAt(j));
                } else {
                    if (unique.containsKey(c)) {
                        writer.write(unique.get(c));
                    } else {
                        writer.write(line.charAt(j));
                    }
                }
            }
        }
    }

    public static boolean isDouble(int i, String line, Character c) {
        return i + 1 < line.length() && c == line.charAt(i + 1);
    }

    public static boolean isTriple(int i, String line, Character c) {
        return i + 2 < line.length() && c == line.charAt(i + 1) && c == line.charAt(i + 2);
    }

    public static boolean forDouble(int j, String line, Character c, String emphasis) throws IOException {
        if (isDouble(j, line, c) && tagsString.get(STRONG) > 0) {
            if (tagsString.get(STRONG) % 2 == 0) {
                writer.write("<strong>");
            } else {
                writer.write("</strong>");
            }
            tagsString.put(STRONG, tagsString.get(STRONG) - 1);
            return true;
        } else {
            if (tagsString.get(emphasis) > 0) {
                if (tagsString.get(emphasis) % 2 == 0) {
                    writer.write("<em>");
                } else {
                    writer.write("</em>");
                }
                tagsString.put(emphasis, tagsString.get(emphasis) - 1);
            } else {
                writer.write(line.charAt(j));
            }
        }
        return false;
    }
}