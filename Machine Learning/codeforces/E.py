import math
from collections import defaultdict


def main():
    kx, ky = map(int, input().split())
    n = int(input())

    cnt_x = defaultdict(int)
    cnt_obj = defaultdict(int)

    for _ in range(n):
        x1, x2 = map(int, input().split())
        cnt_x[x1] += 1
        cnt_obj[(x1, x2)] += 1

    ans = 0
    for (x1, x2), value in cnt_obj.items():
        ans -= value * math.log(value / cnt_x[x1])

    print(ans / n)


if __name__ == "__main__":
    main()
