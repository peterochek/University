import numpy as np
from sklearn.metrics import pairwise_distances
from sklearn.metrics.pairwise import cosine_distances


class NearestNeighbor:
    def __init__(
        self,
        n_neighbors=1,
        weights="gaussian",
        metric="euclidean",
        window_type="knn",
        window_size=1,
        p=1,
        a=2,
        b=2,
        n_jobs=-1,
    ):
        self.weights = weights
        self.metric = metric
        self.window_type = window_type

        self.metric_p = {"euclidean": 2, "manhattan": 1, "minkowski": p}

        if metric != "cosine":
            self.p = self.metric_p[metric]

        self.n_neighbors = n_neighbors
        self.window_size = window_size
        self.n_jobs = n_jobs
        self.a = a
        self.b = b

        self.y_train = None
        self.x_train = None

    def fit(self, x, y, priori_weights=None):
        self.x_train = x
        self.y_train = y
        self.priori_weights = priori_weights

    def predict(self, x, use_weights=False):
        dists = self.distances(x)

        neighbors = self.choose_neighbors(dists)
        kernel_weights = self.posteriori_weights(dists, use_weights)

        class_votes = self.vote(x, neighbors, kernel_weights)

        predictions = np.argmax(class_votes, axis=1)

        return predictions

    def posteriori_weights(self, dists, use_weights=False):
        kernel_weights = self.apply_kernel(dists)

        if use_weights and self.priori_weights is not None:
            assert kernel_weights.shape[1] == len(self.priori_weights)
            kernel_weights *= self.priori_weights

        return kernel_weights

    def normalize(self, dists):
        if self.window_type == "knn":
            k_plus_one_distances = np.sort(dists, axis=1)[:, self.n_neighbors]
            return dists / k_plus_one_distances[:, None]
        elif self.window_type == "fixed":
            return dists / self.window_size
        else:
            return dists

    def vote(self, x, neighbors, kernel_weights):
        class_votes = np.zeros((x.shape[0], np.unique(self.y_train).shape[0]))

        for i in range(x.shape[0]):
            for j, neighbor in enumerate(neighbors[i]):
                if neighbor:
                    class_index = self.y_train[j]
                    class_votes[i, class_index] += kernel_weights[i, j]

        return class_votes

    def apply_kernel(self, dists, weights=None):
        if weights is None:
            weights = self.weights

        if weights == "uniform":
            return np.ones(dists.shape)
        elif weights == "gaussian":
            return (1 / np.sqrt(2 * np.pi)) * np.exp(-0.5 * (dists**2))
        elif weights == "common":
            return np.abs(1 - np.abs(dists) ** self.a) ** self.b
        else:
            raise ValueError("Incorrect kernel chosen!")

    def distances(self, x):
        if self.metric == "cosine":
            dists = cosine_distances(x, self.x_train)
        else:
            dists = pairwise_distances(
                x, self.x_train, "minkowski", n_jobs=self.n_jobs, p=self.p
            )

        return self.normalize(dists)

    def choose_neighbors(self, dists):
        if self.window_type == "knn":
            neighbors_indices = np.argsort(dists, axis=1)[:, : self.n_neighbors]
            neighbors = np.zeros(dists.shape, dtype=bool)
            for i, indices in enumerate(neighbors_indices):
                neighbors[i, indices] = True
        else:
            neighbors = dists <= self.window_size

        return neighbors
