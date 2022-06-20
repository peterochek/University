import java.util.Scanner;

public class J {
    public static void print(int[][] graph) {
        for (int[] line : graph) {
            for (int num : line) {
                System.out.print(num);
            }

            System.out.println();
        }
    }

    public static void main(String[] args) {
        Scanner input = new Scanner(System.in);

        int n = input.nextInt();

        int[][] ways = new int[n][n];

        for (int i = 0; i < n; i++) {
            String line = input.next();

            for (int j = 0; j < line.length(); j++) {
                ways[i][j] = Character.getNumericValue(line.charAt(j));
            }
        }

        input.close();

        for (int i = 0; i < n; i++) {
            for (int j = i + 1; j < n; j++) {
                if (ways[i][j] != 0) {
                    for (int k = j + 1; k < n; k++) {
                        ways[i][k] -= ways[j][k];
                        ways[i][k] %= 10;
                    }

                    ways[i][j] = 1;
                }
            }
        }

        print(ways);
    }
}