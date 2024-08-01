from typing import Tuple

import numpy as np
from joblib import Parallel, delayed

from tree import Node, Tree


class CustomDecisionTreeClassifier(Tree):
    def __init__(self, max_depth=None, min_samples_split=2, min_samples_leaf=1):
        super().__init__(max_depth, min_samples_split, min_samples_leaf)

    def _build_tree(self, X: np.ndarray, y: np.ndarray, depth=0) -> Node | int:
        n_samples, _ = X.shape
        unique_classes, counts = np.unique(y, return_counts=True)
        most_common_class = unique_classes[np.argmax(counts)]

        if (
            depth == self.max_depth
            or n_samples < self.min_samples_split
            or np.max(counts) == n_samples
            or np.min(counts) < self.min_samples_leaf
        ):
            return most_common_class

        best_feature, best_threshold = self._find_best_split(X, y)
        if best_feature is None:
            return most_common_class

        left_idx = X[:, best_feature] < best_threshold
        right_idx = X[:, best_feature] >= best_threshold
        left_subtree = self._build_tree(X[left_idx], y[left_idx], depth + 1)
        right_subtree = self._build_tree(X[right_idx], y[right_idx], depth + 1)

        return Node(best_feature, best_threshold, left_subtree, right_subtree)

    def best_split_for_feature(
        self, feature_index: int, X: np.ndarray, y: np.ndarray, n_samples: int
    ):
        thresholds, _ = zip(*sorted(zip(X[:, feature_index], y)))
        best_feature_impurity = np.inf
        best_threshold = None
        for i in range(1, n_samples):
            if thresholds[i] == thresholds[i - 1]:
                continue
            split_threshold: float = (thresholds[i] + thresholds[i - 1]) / 2
            left_y, right_y = y[:i], y[i:]
            impurity = self._gini_impurity(left_y, right_y)
            if (
                impurity < best_feature_impurity
                and len(left_y) >= self.min_samples_leaf
                and len(right_y) >= self.min_samples_leaf
            ):
                best_feature_impurity = impurity
                best_threshold = split_threshold
        return feature_index, best_feature_impurity, best_threshold

    def _find_best_split(self, X: np.ndarray, y: np.ndarray) -> Tuple[int, float]:
        n_samples, n_features = X.shape

        results = Parallel(n_jobs=-1, verbose=0)(
            delayed(self.best_split_for_feature)(feature_index, X, y, n_samples)
            for feature_index in range(n_features)
        )

        best_feature, best_threshold = None, None
        best_impurity = np.inf
        for feature_index, impurity, threshold in results:
            if impurity < best_impurity:
                best_impurity = impurity
                best_feature = feature_index
                best_threshold = threshold

        return best_feature, best_threshold

    def gini(self, y: np.ndarray) -> float:
        _, counts = np.unique(y, return_counts=True)
        probabilities = counts / counts.sum()
        return 1 - np.sum(probabilities**2)

    def _gini_impurity(self, left_y: np.ndarray, right_y: np.ndarray):
        left_impurity = self.gini(left_y)
        right_impurity = self.gini(right_y)
        n_left, n_right = len(left_y), len(right_y)
        n_total = n_left + n_right
        return (n_left / n_total) * left_impurity + (n_right / n_total) * right_impurity
