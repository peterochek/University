package search;

public class BinarySearch {

    //Pre: ∀i: 1 <= i <= n - 1 => arr[i + 1] <= arr[i]
    //Post: 0 <= idx <= n - 1, arr[idx] <= x < arr[idx - 1]
    public static int recursive(int x, int l, int r, int[] arr) {
        if (r - l == 1) {
            //l + 1 = r
            //arr[l + 1] <= x < arr[l] => idx = l + 1
            return l + 1;
        } else {
            //l + 1 != r => l < r
            int m = (l + r) / 2;
            if (arr[m] <= x) {
                //arr[m] <= x
                return recursive(x, l, m, arr); //r` = m => arr[r`] <= x
                //r` - l` < r - l => len` < len
            } else {
                //x < arr[m]
                return recursive(x, m, r, arr); //l` = m => x < arr[l`]
                //r` - l` < r - l => len` < len
            }
        }
    }

    //Pre: ∀i: 1 <= i <= n - 1 => arr[i + 1] <= arr[i]
    //Post: 0 <= idx <= n, arr[idx] <= x < arr[idx - 1]
    public static int iterative(int x, int[] arr) {
        int l = -1, r = arr.length; //-1 <= l < r <= n
        //Inv: -1 <= l <= m < r <= a.length
        while (r - l > 1) {
            //l < r - 1
            int m = (l + r) / 2;
            //l <= m < r
            if (arr[m] <= x) {
                //arr[m] <= x
                r = m;
                //r` = m => arr[r`] <= x
            } else {
                //x < arr[m]
                l = m;
                //l` = m => x < arr[l`]
            }
            //m = (l + r) / 2 && ((r` = r && l = m) || (l = l` && r = m)) => r` - l` < r - l => len` < len)
        }
        //r - l <= 1 => r <= l + 1
        //arr[r] <= x < arr[l]
        return l + 1;
    }

    public static void main(String[] args) {
        int n = args.length - 1;

        int x = Integer.parseInt(args[0]);

        int[] arr = new int[n];
        for (int i = 0; i < n; i++) {
            arr[i] = Integer.parseInt(args[i + 1]);
        }

        //System.out.println(recursive(x, -1, arr.length, arr));
        System.out.println(iterative(x, arr));
    }
}