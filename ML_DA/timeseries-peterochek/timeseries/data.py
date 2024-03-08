import numpy as np
import yfinance as yf
from .constants import TICKER

class Data:
    data = None
    last_train_log_value = None
    raw = None
    
    def __init__(self) -> None:
        df = yf.download(TICKER, start="2002-01-01", end="2022-01-01", interval="1mo")
        self.raw = df["Close"]
        self.data = df["Close"].values
    
    def difference(self, x, d=1):
        if d == 0:
            return x
        else:
            x = np.diff(x)
            return self.difference(x, d - 1)

    def raw_data(self):
        return self.raw

    def undo_difference(self, x, d=1):
        if d == 1:
            return np.cumsum(x)
        else:
            x = np.cumsum(x)
            return self.undo_difference(x, d - 1)
        
    def fwd(self):
        data = np.log(self.data)
        
        train_len = int(0.8 * len(data))
        self.last_train_log_value = data[train_len - 1]
        
        return train_len, np.diff(data)
    
    def bkwd(self, data):
        data = data.squeeze()
    
        data = data.tolist()
        data = [self.last_train_log_value] + data
        data = np.array(data)
        
        data = self.undo_difference(data)
    
        return np.exp(data)
