import numpy as np
from tqdm import trange

from regressor_tree import CustomDecisionTreeRegressor


class CustomGradientBoostingClassifier:
    def __init__(
        self,
        n_estimators=100,
        learning_rate=0.1,
        min_samples_split=2,
        min_samples_leaf=1,
        max_depth=None,
    ):
        self.n_estimators = n_estimators
        self.learning_rate = learning_rate
        self.models = []
        self.min_samples_leaf = min_samples_leaf
        self.min_samples_split = min_samples_split
        self.max_depth = max_depth

    def fit(self, X, y):
        residuals = y

        for _ in range(self.n_estimators):
            tree = CustomDecisionTreeRegressor(
                min_samples_split=self.min_samples_split,
                min_samples_leaf=self.min_samples_leaf,
                max_depth=self.max_depth,
            )
            tree.fit(X, residuals)
            self.models.append(tree)

            predictions = tree.predict(X)
            residuals = residuals - self.learning_rate * predictions

    def predict(self, X):
        ensemble_predictions = np.zeros(len(X))

        for tree in self.models:
            ensemble_predictions += self.learning_rate * tree.predict(X)

        return (ensemble_predictions > 0.5).astype(int)

    def get_params(self, deep=True):

        return {
            "n_estimators": self.n_estimators,
            "learning_rate": self.learning_rate,
            "max_depth": self.max_depth,
        }

    def set_params(self, **parameters):
        for parameter, value in parameters.items():
            setattr(self, parameter, value)
        return self
