import numpy as np


class LinearRegressionRidge:
    def __init__(self, lam=0.1):
        self.lam = lam

    def fit(self, X, y):
        n, d = X.shape
        I = np.eye(d)
        self.w = np.linalg.inv(X.T @ X + self.lam * I) @ X.T @ y

    def predict(self, X):
        return np.sign(X @ self.w)
