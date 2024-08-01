import numpy as np


def convolve2d(matrix, kernel):
    m = matrix.shape[0] - kernel.shape[0] + 1
    result = np.zeros((m, m))
    for i in range(m):
        for j in range(m):
            result[i, j] = np.sum(matrix[i:i + k, j:j + k] * kernel)
    return result


def find_kernel(input_matrix, output_matrix, kernel_size):
    n = input_matrix.shape[0]
    m = output_matrix.shape[0]
    k = kernel_size

    kernel = np.random.rand(k, k)

    for _ in range(10000):
        conv_output = convolve2d(input_matrix, kernel)

        error = output_matrix - conv_output
        gradient = -convolve2d(input_matrix[-k:, -k:], error) / (m * m)

        kernel -= 0.01 * gradient

        if np.max(np.abs(error)) < 1e-6:
            break

    return kernel


n = 5
m = 3
k = n - m + 1

input_matrix = np.random.randint(0, 256, (n, n))
true_kernel = np.random.rand(k, k)
output_matrix = convolve2d(input_matrix, true_kernel)

estimated_kernel = find_kernel(input_matrix, output_matrix, k)
print("Estimated Kernel:")
print(estimated_kernel)
