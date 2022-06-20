package search;

public class BinarySearchMin {

    //Pre: two consecutive arrays (dec, inc)...
    //Post: 0 <= idx <= n - 1, min(arr) = arr[idx]
    public static int iterative(int[] a) {
        int l = 0, r = a.length - 1;

        //Inv: arr[m] < arr[l] && arr[m] < arr[r]
        while (l < r) {
            //l < r
            int m = (l + r) / 2;
            //l <= m < r

            if (a[m] < a[m + 1]) {
                // idx(min) in [l; m]
                r = m;
                //r` = m
            } else {
                // idx(min) in [m + 1; r]
                l = m + 1;
                //l` = m
            }
            //m = (l + r) / 2 && ((r` = r && l = m) || (l = l` && r = m)) => r` - l` < r - l => len` < len)
        }
        //m = l
        //l >= r => (l + r) / 2 = l => found number

        return a[l];
    }

    //Pre: two consecutive arrays (dec, inc)...
    //Post: 0 <= idx <= n - 1, min(arr) = arr[idx]
    public static int recursive(int l, int r, int[] a) {
        int m = (l + r) / 2;

        if (r - l <= 2) {
            //r - l <= 2
            return Math.min(Math.min(a[l], a[m]), a[r]);
            //idx in [l; r] => idx = l || idx = m || idx = r (depends on length)
        }

        //r - l > 2
        if (a[m - 1] > a[m + 1]) {
            //[m - 1] > a[m + 1]
            return recursive(m, r, a);
            //r - l` < r - l => len` < len
        } else {
            //a[m + 1] >= a[m - 1]
            return recursive(l, m, a);
            //r` - l < r - l => len` < len
        }
    }

    //Pre: two consecutive arrays (dec, inc)...
    //Post (printed): 0 <= idx <= n - 1, min(arr) = arr[idx]
    public static void main(String[] args) {
        int n = args.length;

        int[] arr = new int[n];
        for (int i = 0; i < n; i++) {
            arr[i] = Integer.parseInt(args[i]);
        }

        //System.out.println(iterative(arr));
        System.out.println(recursive(0, arr.length - 1, arr));
    }
}
