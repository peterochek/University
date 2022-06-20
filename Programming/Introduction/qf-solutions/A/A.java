import java.util.Scanner;

public class A {

    public static void main(String[] args) {
        Scanner input = new Scanner(System.in);

        int a = input.nextInt();
        int b = input.nextInt();
        int n = input.nextInt();

        int summary = (int) (2 * Math.ceil(((float) n - b) / (b - a)));

        System.out.println(summary + 1);
    }
}
