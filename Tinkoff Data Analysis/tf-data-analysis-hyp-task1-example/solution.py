import pandas as pd
import numpy as np


chat_id = 285458518  # Ваш chat ID, не меняйте название переменной


def solution(x_success: int, x_cnt: int, y_success: int, y_cnt: int) -> bool:
    x_rate = x_success / x_cnt
    y_rate = y_success / y_cnt

    se_x = (x_rate * (1 - x_rate) / x_cnt) ** 0.5
    se_y = (y_rate * (1 - y_rate) / y_cnt) ** 0.5

    z_score = (y_rate - x_rate) / ((se_x**2 + se_y**2) ** 0.5)

    critical_z = 1.4051

    return z_score > critical_z
