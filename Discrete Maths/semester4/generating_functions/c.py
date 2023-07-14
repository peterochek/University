k = int(input())

a = list(map(int, input().split())) + [0]
c = list(map(int, input().split())) + [0]

q = [1] + [-c[i] for i in range(k)]
p = []

for i in range(k):
    tmp = 0
    for j in range(i + 1):
        tmp += a[i - j] * q[j]
    p.append(tmp)


def last_non_zero():
    for i in range(k - 1, -1, -1):
        if p[i] != 0:
            return i
    return 0


idx = len(p) - next(i for i, val in enumerate(reversed(p), 1) if val != 0)

print(idx)
print(' '.join(map(str, p[:idx + 1])))
print(k)
print(' '.join(map(str, q)))
