import numpy as np
from joblib import Parallel, delayed


class Node:
    def __init__(
        self,
        best_feature: int,
        best_threshold: float,
        left_subtree: "Node",
        right_subtree: "Node",
    ) -> None:
        self.feature = best_feature
        self.threshold = best_threshold
        self.left = left_subtree
        self.right = right_subtree


class Tree:
    def __init__(self, max_depth=12, min_samples_split=2, min_samples_leaf=1):
        self.max_depth = max_depth
        self.min_samples_split = min_samples_split
        self.min_samples_leaf = min_samples_leaf
        self.tree = None

    def fit(self, X: np.ndarray, y: np.ndarray):
        self.tree = self._build_tree(X, y)

    def predict(self, X: np.ndarray):
        return np.array([self._predict(inputs, self.tree) for inputs in X])

    def _predict(self, inputs, node: Node | int):
        if isinstance(node, Node):
            if inputs[node.feature] < node.threshold:
                return self._predict(inputs, node.left)
            else:
                return self._predict(inputs, node.right)
        return node

    def get_depth(self):
        return self._get_depth(self.tree)

    def _get_depth(self, node: Node | int):
        if not isinstance(node, Node):
            return 0
        left_depth = self._get_depth(node.left)
        right_depth = self._get_depth(node.right)
        return max(left_depth, right_depth) + 1

    def get_params(self, deep=True):

        return {
            "max_depth": self.max_depth,
            "min_samples_split": self.min_samples_split,
            "min_samples_leaf": self.min_samples_leaf,
        }

    def set_params(self, **parameters):
        for parameter, value in parameters.items():
            setattr(self, parameter, value)
        return self
