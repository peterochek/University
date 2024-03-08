import java.io.IOException;
import java.text.ParseException;
import java.util.List;


public class Main {
    public static void main(String[] args) throws ParseException, IOException {
        Parser parser = new Parser();

        List<String> exprs = List.of("(2 + 3) * 5", "8 - (4 / 2)", "10 * (6 / 2)", "(7 + 3) / 2", "4 + 5 - (6 * 2)", "(9 / 3) + (12 - 7)", "3 * (4 + 2) / 2", "(8 - 2) * (10 / 5)", "5 + 2 * (9 / 3)", "(6 * 3) - 7 / 2", "10 - (4 + 3) * 2", "(15 / 3) + 2 - 1", "4 + (6 * 2) - 8 / 2", "(10 - 5) * (3 + 1)", "7 / 2 + (12 - 4) * 2");

        List<String> inputParts = exprs.stream().map(expr -> "val x: Int =" + expr + ";").toList();

        String input = inputParts.stream().reduce("", (x, y) -> x + y);

        Tree tree = parser.parse(input);
        tree.toDot("graph");
    }
}
