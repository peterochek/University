public class Sum {
    public static void main(String[] args) {
        int res = 0;
        for (String arg : args) {
            int k = 0, len = 0;
            int n = arg.length();
            for (int i = 0; i < n; ++i) {
                char a = arg.charAt(i);
                if (!Character.isWhitespace(a) && i < n - 1) {
                    len++;
                    continue;
                }
                if (len > 0 || (i == n - 1 && !Character.isWhitespace(a))) {
                    if (!Character.isWhitespace(a))
                        len++;
                    String str = arg.substring(k, k + len);
                    if (len > 2 && str.substring(0, 2).equalsIgnoreCase("0x")) {
                        str = str.substring(2);
                        res += Long.parseUnsignedLong(str, 16);
                    } else res += Long.parseLong(str, 10);
                }
                len = 0;
                k = i + 1;
            }
        }
        System.out.println(res);
    }
}