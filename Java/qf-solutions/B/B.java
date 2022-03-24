import java.util.Scanner;

public class B {

    public static void main(String[] args) {
        Scanner input = new Scanner(System.in);

        int n = input.nextInt();

        input.close();

        int first = -710 * 25000;

        for (int i = 0; i < n; i++) {
            System.out.println(first);
            first += 710;
        }
    }
}