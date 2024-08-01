def main():
    _, _, k = map(int, input().split())

    line = map(int, input().split())

    objs = []
    for i, cls in enumerate(line):
        objs.append((i, cls))

    sorted_by_cls = sorted(objs, key=lambda x: x[1])

    round_robin = 0
    splits = [[] for _ in range(k)]

    for idx, cls in sorted_by_cls:
        splits[round_robin].append(idx + 1)
        round_robin = (round_robin + 1) % k

    for part in splits:
        print(len(part), *part)


if __name__ == "__main__":
    main()
