from random import choice

import numpy as np
from numpy import ndarray
from tqdm import trange


class SVM:
    def __init__(
        self,
        kernel: str = "rbf",
        C: float = 1,
        tol: float = 10e-4,
        gamma: float = 0.1,
        r: float = 1.0,
        d: int = 3,
        epochs=1000,
    ):
        self._C = C
        self._tol = tol
        self._set_kernel_type(kernel, gamma, r, d)
        self._b = 0.0
        self.epochs = epochs
        self.alphas = None

    def _set_kernel_type(self, kernel: str, gamma: float, r: float, d: int):
        if kernel.lower() == "linear":
            self._kernel = self._linear_kernel
        elif kernel.lower() == "rbf":
            self._kernel = self._rbf_kernel
            self._gamma = gamma
        elif kernel.lower() == "poly":
            self._kernel = self._poly_kernel
            self._gamma = gamma
            self._r = r
            self._d = d
        else:
            raise AttributeError(f'A kernel type "{kernel}" does not exist!')

    def _linear_kernel(self, x1: ndarray, x2: ndarray) -> float:
        return np.inner(x1, x2)

    def _rbf_kernel(self, x1: ndarray, x2: ndarray) -> float:
        x1 = x1[np.newaxis, :] if np.ndim(x1) == 1 else x1
        x2 = x2[np.newaxis, :] if np.ndim(x2) == 1 else x2
        euclidean_dist = (
            np.linalg.norm(x1[:, :, np.newaxis] - x2.T[np.newaxis, :, :], axis=1) ** 2
        )

        return np.exp(-self._gamma * np.squeeze(euclidean_dist))

    def _poly_kernel(self, x1: ndarray, x2: ndarray) -> float:
        return (self._gamma * np.inner(x1, x2) + self._r) ** self._d

    def _get_prediction(self, x: ndarray) -> float:
        return np.sum(self._alphas * self._y * self._kernel(self._X, x)) + self._b

    def _get_bounds(self, i: int, j: int) -> tuple:
        if self._y[i] != self._y[j]:
            L = max(0, self._alphas[j] - self._alphas[i])
            H = min(self._C, self._C + self._alphas[j] - self._alphas[i])
        else:
            L = max(0, self._alphas[i] + self._alphas[j] - self._C)
            H = min(self._C, self._alphas[i] + self._alphas[j])
        return L, H

    def _get_error(self, i: int) -> float:
        return self._get_prediction(self._X[i]) - self._y[i]

    def _get_eta(self, x1: ndarray, x2: ndarray) -> float:
        return 2 * self._kernel(x1, x2) - self._kernel(x1, x1) - self._kernel(x2, x2)

    def _clip_alpha(self, L: float, H: float, alpha: float) -> float:
        if alpha >= H:
            return H
        elif alpha <= L:
            return L
        return alpha

    def _get_threshold(
        self, i: int, j: int, new_a1: float, new_a2: float, e1: float, e2: float
    ) -> float:
        k_ii = self._kernel(self._X[i], self._X[i])
        k_jj = self._kernel(self._X[j], self._X[j])
        k_ij = self._kernel(self._X[i], self._X[j])

        b1 = (
            self._b
            - e1
            - self._y[i] * (new_a1 - self._alphas[i]) * k_ii
            - self._y[j] * (new_a2 - self._alphas[j]) * k_ij
        )
        b2 = (
            self._b
            - e2
            - self._y[i] * (new_a1 - self._alphas[i]) * k_ij
            - self._y[j] * (new_a2 - self._alphas[j]) * k_jj
        )

        if 0 < self._alphas[i] < self._C:
            return b1
        elif 0 < self._alphas[j] < self._C:
            return b2
        return (b1 + b2) / 2

    def _step(self, i: int, j: int, e1: float) -> bool:
        L, H = self._get_bounds(i, j)
        e2 = self._get_error(j)
        eta = self._get_eta(self._X[i], self._X[j])

        if eta >= 0 or L >= H:
            return False

        new_a2 = self._clip_alpha(
            L, H, self._alphas[j] - (self._y[j] * (e1 - e2)) / eta
        )
        new_a1 = self._alphas[i] + self._y[i] * self._y[j] * (self._alphas[j] - new_a2)

        self._b = self._get_threshold(i, j, new_a1, new_a2, e1, e2)
        self._alphas[i] = new_a1
        self._alphas[j] = new_a2
        return True

    def fit(self, X: ndarray, y: ndarray, epochs=None):
        if epochs is None:
            epochs = self.epochs

        self._X = X
        self._y = y
        
        if self.alphas is None:
            self.alphas = np.random.uniform(0, self._C, size=len(y))

        for _ in range(epochs):
            for i, a in enumerate(self._alphas):
                e = self._get_error(i)
                if (self._y[i] * e < -self._tol and a < self._C) or (
                    self._y[i] * e > self._tol and a > 0
                ):
                    j = choice(list(range(i)) + list(range(i + 1, len(self._y))))
                    self._step(i, j, e)

    def predict(self, X_test: ndarray) -> tuple:
        return np.sign(np.array([self._get_prediction(x) for x in X_test]))

    def get_support_vectors(self, zero: float = 10e-5) -> ndarray:
        return self._X[self._alphas > zero]
