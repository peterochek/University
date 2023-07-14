def fact(n):
    el = 1
    for i in range(1, n + 1):
        el = el * i

    return el


def comb(n, k):
    return fact(n) // (fact(k) * fact(n - k))


r = int(input())

d = int(input())

quasi = list(map(int, input().split()))

c = []

for i in range(d + 2):
    c.append(((-1) ** i) * (r ** i) * comb(d + 1, i))

k = d + 1

a = []

for i in range(k):
    tmp = 0
    for j in range(d + 1):
        tmp += quasi[j] * (i ** j)
    a.append(tmp * (r ** i))

a.append(0)

q = c
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
