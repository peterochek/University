from fractions import Fraction

computed = {}


def stirling(n, k):
    key = str(n) + "," + str(k)

    if key in computed.keys():
        return computed[key]
    if n == k == 0:
        return 1
    if n > 0 and k == 0:
        return 0
    if k > n:
        return 0
    result = stirling(n - 1, k - 1) + (n - 1) * stirling(n - 1, k)
    computed[key] = result
    return result


def fact(n):
    if n == 0:
        return 1
    el = 1
    for i in range(1, n + 1):
        el = el * i

    return el


def comb(n, k):
    return fact(n) // (fact(k) * fact(n - k))


r, k = map(int, input().split())

p = list(map(int, input().split()))

coefs = [[] for _ in range(k + 1)]

for order in range(k + 1):
    for j in range(k + 1):
        if j <= order:
            coefs[order].append(((-r) ** j) * comb(order, j))
        else:
            coefs[order].append(0)

# coefs = coefs[::-1]

# print(coefs)

dcbas = []

# in future store as tuple (int, pow)

for taken in range(1, k + 2):
    sum_except_cur = 0
    for order in range(taken - 1):
        sum_except_cur += coefs[-1 - order][-taken] * dcbas[order]
    dcbas.append(Fraction((p[-taken] - sum_except_cur), coefs[-taken][-taken]))

final_coefs = [0 for _ in range(k + 1)]

for pw in range(k + 1):
    for order in range(len(dcbas)):
        final_coefs[pw] += Fraction((dcbas[order] * stirling(order + 1, pw + 1)), fact(order))


def gcd(a, b):
    while b:
        a, b = b, a % b
    return a


for coef in final_coefs:
    print(f'{coef.numerator}/{coef.denominator}', end=' ')

# for n in range(1, 20):
#     for k in range(1, 20):
#         print(stirling(n, k), end=' ')
#     print()
