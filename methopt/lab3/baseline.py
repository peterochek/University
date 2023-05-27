import numpy as np


class Baseline:
    def __init__(self, max_iter: int = 20):
        self.max_iter = max_iter

        self.fits = []
        self.residuals = []

    def loss_fn(self, weights):
        y_pred = self.x @ weights

        s = np.mean((y_pred - self.y) ** 2)

        return np.sqrt(s)

    def fit(
        self, x: np.ndarray, y: np.ndarray, init_guess: np.ndarray = None
    ) -> np.ndarray:
        raise NotImplementedError("define fit function")

    def jacobian(self, x: np.ndarray):
        N = x.shape[0]
        gradient = []
        eps = abs(x[i]) * np.finfo(np.float32).eps
        for i in range(N):
            #   eps = 1.0
            x_copy = 1.0 * x[i]
            f0 = self.loss_fn(x)
            x[i] = x[i] + eps
            f1 = self.loss_fn(x)
            gradient.append(np.array([f1 - f0]) / eps)
            x[i] = x_copy
        return np.array(gradient).reshape(x.shape)

    def residual(self, x: np.ndarray, weights: np.ndarray) -> np.ndarray:
        y_fit = x @ weights
        return y_fit - self.y

    def predict(self, x) -> np.ndarray:
        return x @ self.weights
