from collections import defaultdict


def calc_dist(xs):
    dist = 0
    b = len(xs) - 1
    for i, el in enumerate(xs):
        dist += el * (2 * i - b)

    return 2 * dist


def main():
    _ = int(input())
    n = int(input())

    xs = []
    occ = defaultdict(list)

    for _ in range(n):
        x, y = map(int, input().split())
        xs.append(x)
        occ[y].append(x)

    inner_dist = calc_dist(sorted(xs))

    groups_dist = sum(map(lambda group: calc_dist(sorted(group)), occ.values()))

    print(groups_dist)
    print(inner_dist - groups_dist)


if __name__ == "__main__":
    main()
