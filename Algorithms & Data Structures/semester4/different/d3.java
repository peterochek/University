import java.util.*;

public class Main {
    static int n, m;
    static final int MAX_VALUE = Integer.MAX_VALUE;

    static class E {
        int u, v;
        int f, max, c;
        int ord = -1;

        E(int u, int v, int f, int max, int c) {
            this.u = u;
            this.v = v;
            this.f = f;
            this.max = max;
            this.c = c;
        }

        E() {
            this.u = 0;
            this.v = 0;
            this.f = 0;
            this.max = 0;
            this.c = 0;
        }

        E rev() {
            return new E(v, u, -f, 0, -c);
        }

        public String toString() {
            return String.format("(%d, %d, %d, %d, %d, %d)", u, v, f, max, c, ord);
        }
    }

    static List<List<E>> es;
    static long[] dist;
    static int[] same;
    static E[] prev;

    static long ans;
    static int cnt = 0;

    static void append(int u, int v, int max, int c) {
        E e = new E(u, v, 0, max, c);
        es.get(u).add(e);
        es.get(v).add(e.rev());

        es.get(u).get(es.get(u).size() - 1).ord = es.get(v).size() - 1;
        es.get(v).get(es.get(v).size() - 1).ord = es.get(u).size() - 1;
    }

    static boolean compare(E e) {
        return dist[e.v] > dist[e.u] + e.c && e.f < e.max;
    }

    static void impl() {
        while (true) {
            cnt += 1;
            Deque<Integer> queue = new ArrayDeque<>();

            same = new int[n + 1];
            dist = new long[n + 1];
            Arrays.fill(dist, MAX_VALUE);
            prev = new E[n + 1];
            Arrays.fill(prev, new E());

            dist[0] = 0;
            queue.add(0);

            while (!queue.isEmpty()) {
                int start = queue.pollFirst();
                same[start] = 2;

                for (E e : es.get(start)) {
                    if (compare(e)) {
                        dist[e.v] = dist[e.u] + e.c;

                        if (same[e.v] == 0) {
                            queue.add(e.v);
                        } else if (same[e.v] == 2) {
                            queue.addFirst(e.v);
                        }

                        same[e.v] = 1;
                        prev[e.v] = e;
                    }
                }
            }

            long rem = MAX_VALUE;

            if (dist[n - 1] == MAX_VALUE) {
                break;
            } else {
                for (int vrtx = n - 1; vrtx != 0; vrtx = prev[vrtx].u) {
                    E e = prev[vrtx];
                    rem = Math.min(rem, e.max - e.f);
                }

                for (int vrtx = n - 1; vrtx != 0; vrtx = prev[vrtx].u) {
                    E e = prev[vrtx];
                    E rev = es.get(e.v).get(e.ord);

                    e.f += rem;
                    rev.f -= rem;

                    ans += rem * e.c;
                }
            }

//            System.out.println("cnt: " + cnt);
//            System.out.println("prev: " + Arrays.toString(prev));
//            System.out.println("same: " + Arrays.toString(same));
//            System.out.println("dist: " + Arrays.toString(dist));
//            System.out.print("es: [");
//            for (List<E> e : es) {
//                System.out.print(e + " ");
//            }
//            System.out.println("]");
        }
    }

    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        n = scanner.nextInt();
        m = scanner.nextInt();

        es = new ArrayList<>(2 * n + 2);

        for (int i = 0; i <= 2 * n + 1; i++) {
            es.add(new ArrayList<>());
        }

        for (int i = 1; i <= n; i++) {
            append(0, n + i, 1, 0);
            append(i, n + i, MAX_VALUE, 0);
            append(i, 2 * n + 1, 1, 0);
            append(n + i, i, MAX_VALUE, scanner.nextInt());
        }

        for (int i = 0; i < m; i++) {
            int u = scanner.nextInt();
            int v = scanner.nextInt();
            int cost = scanner.nextInt();
            append(n + u, v, MAX_VALUE, cost);
        }

        n = 2 * (n + 1);

        impl();

        System.out.println(ans);
    }
}