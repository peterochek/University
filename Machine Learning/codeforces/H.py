from collections import defaultdict


def main():
    k1, k2 = map(int, input().split())
    n = int(input())
    cnt_x1 = [0 for _ in range(k1 + 1)]
    cnt_x2 = [0 for _ in range(k2 + 1)]
    cnt_obj = defaultdict(float)

    for _ in range(n):
        x1, x2 = map(int, input().split())
        cnt_x1[x1] += 1
        cnt_x2[x2] += 1
        cnt_obj[(x1, x2)] += 1.0

    ans = n
    for (x1, x2), cnt in cnt_obj.items():
        exp = cnt_x1[x1] * cnt_x2[x2] / n
        ans -= exp - (cnt - exp) ** 2 / exp

    print(ans)


if __name__ == "__main__":
    main()
