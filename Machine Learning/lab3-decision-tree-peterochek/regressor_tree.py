import numpy as np
from joblib import Parallel, delayed

from tree import Node, Tree


class CustomDecisionTreeRegressor(Tree):
    def __init__(self, max_depth=None, min_samples_split=2, min_samples_leaf=1):
        super().__init__(max_depth, min_samples_split, min_samples_leaf)

    def _build_tree(self, X: np.ndarray, y: np.ndarray, depth=0) -> Node | float:
        n_samples, _ = X.shape

        if (
            depth == self.max_depth
            or n_samples < self.min_samples_split
            or n_samples < self.min_samples_leaf
        ):
            return np.mean(y)

        best_feature, best_threshold = self._find_best_split(X, y)
        if best_feature is None:
            return np.mean(y)

        left_idx = X[:, best_feature] < best_threshold
        right_idx = X[:, best_feature] >= best_threshold
        left_subtree = self._build_tree(X[left_idx], y[left_idx], depth + 1)
        right_subtree = self._build_tree(X[right_idx], y[right_idx], depth + 1)

        return Node(best_feature, best_threshold, left_subtree, right_subtree)

    def _find_best_split(self, X: np.ndarray, y: np.ndarray):
        n_samples, n_features = X.shape
        best_feature, best_threshold = None, None
        best_mse = np.inf

        for feature_index in range(n_features):
            sorted_indices = np.argsort(X[:, feature_index])
            sorted_X, sorted_y = X[sorted_indices], y[sorted_indices]
            for i in range(1, n_samples):
                if sorted_X[i, feature_index] == sorted_X[i - 1, feature_index]:
                    continue
                threshold = (
                    sorted_X[i, feature_index] + sorted_X[i - 1, feature_index]
                ) / 2
                left_y, right_y = sorted_y[:i], sorted_y[i:]
                mse = self._mse_split(left_y, right_y)
                if (
                    mse < best_mse
                    and len(left_y) >= self.min_samples_leaf
                    and len(right_y) >= self.min_samples_leaf
                ):
                    best_mse = mse
                    best_feature = feature_index
                    best_threshold = threshold

        return best_feature, best_threshold

    def _mse_split(self, left_y: np.ndarray, right_y: np.ndarray):
        left_mean = np.mean(left_y)
        right_mean = np.mean(right_y)
        left_mse = np.mean((left_y - left_mean) ** 2)
        right_mse = np.mean((right_y - right_mean) ** 2)
        n_left, n_right = len(left_y), len(right_y)
        n_total = n_left + n_right
        return (n_left / n_total) * left_mse + (n_right / n_total) * right_mse
