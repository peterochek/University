import pandas as pd
import numpy as np


chat_id = 285458518 # Ваш chat ID, не меняйте название переменной

def test_profitability_threshold(profit_data):
    sample_mean = np.mean(profit_data)
    sample_std = np.std(profit_data, ddof=1)
    n = len(profit_data)
    
    t_stat = (sample_mean - 500) / (sample_std / np.sqrt(n))
    
    critical_t_value = 1.286
    
    reject_null = t_stat > critical_t_value
    
    return reject_null

def solution(x) -> bool:
    return test_profitability_threshold(x)
