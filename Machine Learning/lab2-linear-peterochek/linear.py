import numpy as np


class LinearClassificationGD:
    def __init__(
        self,
        learning_rate=0.001,
        regularization="elastic_net",
        lam=0.1,
        alpha=0.5,
        epochs=1000,
        risk="squared",
    ):
        self.learning_rate = learning_rate
        self.regularization = regularization
        self.lam = lam
        self.alpha = alpha
        self.epochs = epochs
        self.risk = risk
        self.w = None

    def fit(self, X, y, epochs=None):
        if epochs is None:
            epochs = self.epochs
        _, d = X.shape
        if self.w is None:
            self.w = np.random.uniform(-1, 1, d)
        for _ in range(epochs):
            y_pred = X @ self.w
            gradient = self._compute_gradient(X, y, y_pred)
            self.w -= self.learning_rate * gradient

    def predict(self, X):
        return np.sign(X @ self.w)

    def _compute_gradient(self, X, y, y_pred):
        if self.risk == "squared":
            gradient = -2 * X.T @ (y - y_pred)
        elif self.risk == "log":
            gradient = -X.T @ (y / (1 + np.exp(y * y_pred)))
        elif self.risk == "hinge":
            gradient = -X.T @ (y * (y_pred < 1).astype(int))
        else:
            raise ValueError("Unsupported risk")

        if self.regularization == "elastic_net":
            gradient += self.lam * (
                (1 - self.alpha) * self.w + self.alpha * np.sign(self.w)
            )

        return gradient
