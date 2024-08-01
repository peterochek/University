import numpy as np


def pearson_correlation_coefficient(N, K, a, b):
    correlations = np.zeros(K)
    b_std = np.std(b)

    for i in range(1, K + 1):
        mask = (a == i)
        sum_mask = np.sum(mask)

        if sum_mask > 0:
            covariance = np.mean((mask - np.mean(mask)) * (b - np.mean(b)))
            correlations[i - 1] = covariance * sum_mask / np.std(mask)

    return np.sum(correlations) / (N * b_std)


N, K = map(int, input().split())
a = np.array(input().split(), dtype=int)
b = np.array(input().split(), dtype=int)

print(pearson_correlation_coefficient(N, K, a, b))
