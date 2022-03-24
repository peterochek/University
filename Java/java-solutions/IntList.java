import java.util.Arrays;

public class IntList {
    private int[] arr;
    public int len;

    public IntList(int val) {
        arr = new int[]{val};
        len = 1;
    }

    public void add(int x) {
        if (len == arr.length) {
            arr = Arrays.copyOf(arr, len * 2);
        }
        arr[len++] = x;
    }

    public String listToString(boolean second) {
        if (second) {
            StringBuilder res = new StringBuilder();
            if (len == 1) return "";
            for (int i = 1; i < len; i++) {
                res.append(" ").append(arr[i]);
            }
            return res.toString();
        } else {
            StringBuilder res = new StringBuilder();
            for (int i = 0; i < len; i++) {
                res.append(arr[i]);
                if (i != len - 1) {
                    res.append(" ");
                }
            }
            return res.toString();
        }
    }
}