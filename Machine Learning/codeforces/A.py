from bisect import bisect_left

import numpy as np
from sklearn.neighbors import NearestNeighbors


def my_knn(sorted_objects, queries):
    n, m = len(sorted_objects), len(queries)
    x_sorted = [obj[0] for obj in sorted_objects]
    y_vals = [obj[1] for obj in sorted_objects]

    y_prefix = [0] * (n + 1)
    for i in range(1, n + 1):
        y_prefix[i] = y_prefix[i - 1] + y_vals[i - 1]

    ans = []

    for i in range(m):
        x_q, k_q = queries[i]

        if k_q > n:
            ans.append(-1)
            continue

        pos = bisect_left(x_sorted, x_q)

        left = max(0, pos - k_q)
        right = min(n, pos + k_q)

        best_l, best_r = left, right
        min_dist = float('inf')

        for start in range(max(0, pos - k_q), min(pos + 1, n - k_q + 1)):
            end = start + k_q
            if end > n:
                break
            dist = max(abs(x_sorted[start] - x_q), abs(x_sorted[end - 1] - x_q))
            if dist < min_dist:
                min_dist = dist
                best_l, best_r = start, end

        if best_l + 1 > 0 and x_sorted[best_l + 1] == x_sorted[best_l]:
            ans.append(-1)
        elif best_r < n and x_sorted[best_r - 1] == x_sorted[best_r]:
            ans.append(-1)
        else:
            total_y = y_prefix[best_r] - y_prefix[best_l]
            ans.append(total_y / k_q)

    return ans


def sklearn_knn(sorted_objects, queries):
    n, m = len(sorted_objects), len(queries)

    x_sorted = [x for x, _ in sorted_objects]
    y_sorted = [y for _, y in sorted_objects]

    # Use sklearn for correct answers
    X = np.array(x_sorted).reshape(-1, 1)  # Reshape data for sklearn
    nbrs = NearestNeighbors(n_neighbors=n, algorithm='auto').fit(X)
    y_array = np.array(y_sorted)

    ans = []

    for x_q, k_q in queries:
        if k_q > n:
            ans.append(-1)
        if k_q == n:
            ans.append(y_array.mean())
        else:
            distances, indices = nbrs.kneighbors([[x_q]], n_neighbors=k_q)
            next_distance, _ = nbrs.kneighbors([[x_q]], n_neighbors=k_q + 1)
            if next_distance[0][k_q - 1] == next_distance[0][k_q]:
                sklearn_average = -1
            else:
                sklearn_average = y_array[indices].mean()
            ans.append(sklearn_average)

    return ans


def generate_data(n, max_abs_x=100, max_y=100):
    x = np.random.randint(-max_abs_x, max_abs_x + 1, size=n)
    y = np.random.randint(1, max_y + 1, size=n)
    return list(zip(x, y))


def test_knn_implementation(n=10, queries=10, max_abs_x=10, max_y=10):
    data = generate_data(n, max_abs_x=max_abs_x, max_y=max_y)
    queries = [(np.random.randint(-max_abs_x, max_abs_x), np.random.randint(1, n + 1)) for _ in range(queries)]

    data_sorted = sorted(data)

    print(data_sorted)

    my_ans = my_knn(data_sorted, queries)
    sklearn_ans = sklearn_knn(data_sorted, queries)

    for my, sk, q in zip(my_ans, sklearn_ans, queries):
        if my != sk:
            print(q)
            print(my)
            print(sk)


def fast_test():
    data_sorted = [(-9, 3), (-7, 10), (-6, 6), (-6, 9), (-5, 6), (-4, 3), (-4, 9), (-3, 6), (-2, 4), (-1, 8)]
    queries = [(-4, 1)]

    my_ans = my_knn(data_sorted, queries)

    print(my_ans)


if __name__ == '__main__':
    test_knn_implementation()
    # fast_test()