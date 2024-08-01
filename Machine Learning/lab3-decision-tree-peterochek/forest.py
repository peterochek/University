import numpy as np
from tqdm import trange

from classification_tree import CustomDecisionTreeClassifier


class RandomForestClassifier:
    def __init__(
        self,
        n_estimators=100,
        max_depth=None,
        min_samples_split=2,
        min_samples_leaf=1,
        max_features=None,
    ):
        self.n_estimators = n_estimators
        self.max_depth = max_depth
        self.min_samples_split = min_samples_split
        self.min_samples_leaf = min_samples_leaf
        self.max_features = max_features
        self.trees = []

    def fit(self, X: np.ndarray, y: np.ndarray):
        self.trees = []
        n_samples, n_features = X.shape
        self.max_features = self.max_features or n_features

        for _ in trange(self.n_estimators):
            tree = CustomDecisionTreeClassifier(
                max_depth=self.max_depth,
                min_samples_split=self.min_samples_split,
                min_samples_leaf=self.min_samples_leaf,
            )
            idxs = np.random.choice(n_samples, n_samples, replace=True)
            subset_X = X[idxs, :]
            subset_y = y[idxs]
            feature_idxs = np.random.choice(
                n_features, self.max_features, replace=False
            )
            tree.fit(subset_X[:, feature_idxs], subset_y)
            self.trees.append((tree, feature_idxs))

    def predict(self, X: np.ndarray):
        tree_votes = [
            tree.predict(X[:, feature_idxs]) for tree, feature_idxs in self.trees
        ]
        votes = np.array(tree_votes).T
        return np.array([np.bincount(v).argmax() for v in votes])
