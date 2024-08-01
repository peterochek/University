import numpy as np
from tqdm import trange
from knn import NearestNeighbor


class Lowess(NearestNeighbor):
    def __init__(
        self, n_neighbors=1, weights="gaussian", outer_kernel="gaussian", max_iter=10
    ):
        super().__init__(n_neighbors=n_neighbors, weights=weights)
        self.outer_kernel = outer_kernel
        self.max_iter = max_iter

    def fit(self, x, y):
        self.x_train = x
        self.y_train = y
        self.orig_xtrain = self.x_train.copy()
        self.orig_ytrain = self.y_train.copy()

    def corrected_weights(self):
        corrected_weights = np.ones(len(self.x_train))

        for i in trange(len(self.x_train)):
            self.x_train = np.delete(self.orig_xtrain, i, axis=0)
            self.y_train = np.delete(self.orig_ytrain, i, axis=0)

            x_i = self.orig_xtrain[i]
            x_i = np.reshape(x_i, (1, -1))

            y_i = self.orig_ytrain[i]

            corrected_weights[i] = self.correct_weight(x_i, y_i)

        return corrected_weights

    def correct_weight(self, x_i, y_i):
        dists = self.distances(x_i)
        weights = self.posteriori_weights(dists)  # size = (1, x_train)
        neighbors = self.choose_neighbors(dists)
        class_votes = self.vote(x_i, neighbors, weights)

        return self.apply_kernel(1 - class_votes[0, y_i], self.outer_kernel)
