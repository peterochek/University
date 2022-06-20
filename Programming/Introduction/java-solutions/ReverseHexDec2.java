import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.Locale;

public class ReverseHexDec2 {
    public static void main(String[] args) {

        NewScanner in = new NewScanner(System.in, StandardCharsets.UTF_8);
        int[][] matrix = new int[1][1];
        int matrixLen = 1, curLine = 0;

        int lineLen = 1, curInt = 0;
        int numLines = 0;
        while (in.hasNext()) {
            if (curInt == lineLen) {
                matrix[curLine] = Arrays.copyOf(matrix[curLine], lineLen * 10);
                lineLen *= 10;
            }

            String instance = in.next(c -> Character.isDigit(c) || Character.isLetter(c) || c == '-');

            if (instance != null) {
                instance = instance.toLowerCase(Locale.ROOT);
                if (instance.startsWith("0x")) {
                    matrix[curLine][curInt] = (int) Long.parseLong(instance.substring(2), 16);
                } else {
                    matrix[curLine][curInt] = Integer.parseInt(instance, 10);
                }
                curInt++;
            }

            if (in.numLines - numLines > 0) {
                numLines = in.numLines;
                if (curLine == matrixLen - 1) {
                    matrix = Arrays.copyOf(matrix, matrixLen * 2);
                    matrixLen *= 2;
                }
                matrix[curLine] = Arrays.copyOf(matrix[curLine], curInt);
                lineLen = 1;
                curInt = 0;
                curLine++;
                matrix[curLine] = new int[1];
            }
        }

        matrix = Arrays.copyOf(matrix, numLines);

        in.close();

        for (int i = matrix.length - 1; i >= 0; --i) {
            for (int j = matrix[i].length - 1; j >= 0; --j) {
                System.out.print("0x" + Integer.toHexString(matrix[i][j]) + " ");
            }
            System.out.println();
        }
    }
}