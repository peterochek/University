from collections import defaultdict


def main():
    _ = int(input())
    n = int(input())
    occ = defaultdict(list)
    total_x = defaultdict(int)

    for _ in range(n):
        x, y = map(int, input().split())
        total_x[x] += y
        occ[x].append(y)

    ans = 0
    for k, vs in occ.items():
        mean = total_x[k] / len(vs)
        for v in vs:
            ans += (v - mean) ** 2

    print(ans / n)


if __name__ == "__main__":
    main()
