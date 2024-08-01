import pandas as pd
import numpy as np


chat_id = 285458518  # Ваш chat ID, не меняйте название переменной


def ecdf(data):
    n = len(data)
    x = np.sort(data)
    y = np.arange(1, n + 1) / n
    return x, y


def ks_test(x: np.array, y: np.array) -> bool:
    n = len(x)
    x_sorted, x_ecdf = ecdf(x)
    y_sorted, y_ecdf = ecdf(y)

    combined_sorted = np.sort(np.concatenate([x_sorted, y_sorted]))

    ecdf_x = np.searchsorted(x_sorted, combined_sorted, side="right") / len(x)
    ecdf_y = np.searchsorted(y_sorted, combined_sorted, side="right") / len(y)

    ks_stat = np.max(np.abs(ecdf_x - ecdf_y))

    critical_value = 1.22385 / np.sqrt(n)

    is_same_distribution = ks_stat < critical_value

    return is_same_distribution


def solution(x: np.array, y: np.array) -> bool:
    is_same = ks_test(x, y)
    return not is_same
