import numpy as np
import numpy.linalg as ln
from baseline import Baseline


class DogLeg(Baseline):
    def __init__(
        self, init_r=1.0, max_trust_radius=100.0, eta=0.15, acc=1e-4
    ) -> None:
        super().__init__()
        self.init_r = init_r
        self.max_r = max_trust_radius
        self.eta = eta
        self.acc = acc

    def dogleg_method(self, inv_matrix, jacobian, hessian, trust_radius):
        pB = -np.dot(inv_matrix, jacobian) # opt point
        norm_pB = np.sqrt(np.dot(pB, pB))

        if np.all(norm_pB <= trust_radius):
            return pB

        pU = (
            -(np.dot(jacobian, jacobian) / np.dot(jacobian, np.dot(hessian, jacobian))) # steepest
            * jacobian
        )
        dot_pU = np.dot(pU, pU)
        norm_pU = np.sqrt(dot_pU)

        if norm_pU >= trust_radius:
            return trust_radius * pU / norm_pU

        pB_pU = pB - pU  # intersection cauchy - newton and quadratic
        dot_pB_pU = np.dot(pB_pU, pB_pU)
        dot_pU_pB_pU = np.dot(pU, pB_pU)
        fact = dot_pU_pB_pU**2 - dot_pB_pU * (dot_pU - trust_radius**2)
        tau = (-dot_pU_pB_pU + np.sqrt(fact)) / dot_pB_pU

        return pU + tau * pB_pU

    def fit(self, x: np.ndarray, y: np.ndarray, init_guess: np.ndarray = None):
        self.x = x
        self.y = y
        self.weights = init_guess

        trust_radius = self.init_r
        k = 0

        while True:
            self.fits.append(self.predict(self.x))
            self.residuals.append(self.residual(self.x, self.weights))

            jacobian = self.jacobian(self.weights)
            hessian = self.hessian(self.weights)

            inv_matrix = np.linalg.inv(hessian)

            pk = self.dogleg_method(inv_matrix, jacobian, hessian, trust_radius)

            act_red = self.loss_fn(self.weights) - self.loss_fn(self.weights + pk)

            pred_red = -(np.dot(jacobian, pk) + 0.5 * np.dot(pk, np.dot(hessian, pk)))

            if pred_red == 0.0:
                rhok = 1e99
            else:
                rhok = act_red / pred_red

            norm_pk = np.sqrt(np.dot(pk, pk)) # norm

            if rhok < 0.25:
                trust_radius = 0.25 * norm_pk
            else:
                if rhok > 0.75 and norm_pk == trust_radius: # enlarge region
                    trust_radius = min(2.0 * trust_radius, self.max_r)
                else:
                    trust_radius = trust_radius

            if rhok > self.eta:
                self.weights = self.weights + pk
            else:
                self.weights = self.weights

            if ln.norm(jacobian) < self.acc:
                break

            if k >= self.max_iter:
                break

            k = k + 1

        return self.weights, self.fits, self.residuals

    def hessian(self, x):
        N = x.shape[0]
        hessian = np.zeros((N, N))
        gd_old = self.jacobian(x)
        eps = np.linalg.norm(gd_old) * np.finfo(np.float32).eps
        for i in range(N):
            x_copy = 1.0 * x[i]
            x[i] = x_copy + eps
            gd_new = self.jacobian(x)
            hessian[:, i] = ((gd_new - gd_old) / eps).reshape(x.shape[0])
            x[i] = x_copy
        return hessian
