import numpy as np

n = int(input())

data = []

for _ in range(n):
    x, y = map(int, input().split())
    data.append((x, y))

data_array = np.array(data)
x = data_array[:, 0]
y = data_array[:, 1]

x_ranks = x.argsort().argsort()
y_ranks = y.argsort().argsort()

rank_diffs_squared = (x_ranks - y_ranks) ** 2

corr = 1 - (6 * np.sum(rank_diffs_squared)) / (n ** 3 - n)

print(corr)
