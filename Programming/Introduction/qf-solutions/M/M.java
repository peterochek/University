import java.util.HashMap;
import java.util.Scanner;

public class M {
    public static void main(String[] args) {
        Scanner input = new Scanner(System.in);

        int t = input.nextInt();

        for (int l = 0; l < t; l++) {
            int n = input.nextInt();

            int[] days = new int[n];

            for (int i = 0; i < n; i++) {
                days[i] = input.nextInt();
            }

            HashMap<Integer, Integer> C = new HashMap<>();

            int count = 0;

            for (int j = n - 2; j > 0; j--) {
                C.merge(days[j + 1], 1, Integer::sum);

                for (int i = 0; i < j; i++) {
                    int a_k = 2 * days[j] - days[i];
                    if (C.get(a_k) != null) {
                        count += C.get(a_k);
                    }
                }
            }

            System.out.println(count);
        }

        input.close();
    }
}