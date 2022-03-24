import java.util.Scanner;

public class I {

    public static void main(String[] args) {
        Scanner input = new Scanner(System.in);

        int n = input.nextInt();

        int xl = Integer.MAX_VALUE;
        int xr = Integer.MIN_VALUE;
        int yl = Integer.MAX_VALUE;
        int yr = Integer.MIN_VALUE;

        for (int i = 0; i < n; i++) {
            int x = input.nextInt();
            int y = input.nextInt();
            int h = input.nextInt();

            xl = Math.min(x - h, xl);
            xr = Math.max(x + h, xr);
            yl = Math.min(y - h, yl);
            yr = Math.max(y + h, yr);
        }

        input.close();

        int h = (int) Math.ceil(Math.max(xr - xl, yr - yl) / 2.0);

        int x = (xr + xl) / 2;
        int y = (yr + yl) / 2;

        System.out.println(x + " " + y + " " + h);
    }
}
