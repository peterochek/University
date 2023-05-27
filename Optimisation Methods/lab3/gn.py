import numpy as np
from numpy.linalg import pinv

from baseline import Baseline


class GN(Baseline):
    def __init__(
        self,
        tolerance_difference: float = 10 ** (-4),
        tolerance: float = 10 ** (-4),
    ):
        super().__init__()
        self.tolerance_difference = tolerance_difference
        self.tolerance = tolerance

    def fit(
        self, x: np.ndarray, y: np.ndarray, init_guess: np.ndarray = None
    ) -> np.ndarray:
        self.x = x
        self.y = y
        self.weights = init_guess

        for k in range(self.max_iter):
            self.fits.append(self.predict(self.x))
            self.residuals.append(self.residual(self.x, self.weights))

            residual = self.residual(self.x, self.weights)
            jacobian = self.long_jacobian(self.weights)
            pinv_jacobian = pinv(jacobian)
            self.weights = self.weights - pinv_jacobian @ residual

        return self.weights, self.fits, self.residuals

    def long_jacobian(
        self, weights: np.ndarray, step: float = 10 ** (-6)
    ) -> np.ndarray:
        y0 = self.residual(self.x, weights)

        jacobian = []
        for i, parameter in enumerate(weights):
            tmp_weights = weights.copy()
            tmp_weights[i] += step
            y = self.residual(self.x, tmp_weights)
            derivative = (y - y0) / step
            jacobian.append(derivative)
        jacobian = np.array(jacobian).T

        return jacobian
