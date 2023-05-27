import numpy as np
from baseline import Baseline


class SGD(Baseline):
    def __init__(self, lr=1e-3, max_iter=1000):
        super().__init__(max_iter)
        self.lr=lr

    def fit(
        self, x: np.ndarray, y: np.ndarray, init_guess: np.ndarray = None
    ) -> np.ndarray:
        self.x = x
        self.y = y
        self.weights = init_guess

        for i in range(self.max_iter):
            self.fits.append(self.predict(self.x))
            self.residuals.append(self.residual(self.x, self.weights))

            grad = self.jacobian(self.weights)
            
            self.weights -= self.lr * grad
            
            self.lr *= 0.999

        return self.weights, self.fits, self.residuals
