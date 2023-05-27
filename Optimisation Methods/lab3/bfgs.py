import numpy as np
from baseline import Baseline


class BFGS(Baseline):
    def __init__(self, c1=1e-4, c2=0.9):
        super().__init__()
        self.c1 = c1
        self.c2 = c2

    def line_search(self, x, p, grad):
        a = 1

        fx = self.loss_fn(x)
        x_new = x + a * p

        grad_new = self.jacobian(x_new)
        while (
            self.loss_fn(x_new) >= fx + (self.c1 * a * grad.T @ p)
            or grad_new.T @ p <= self.c2 * grad.T @ p
        ):
            a *= 0.5
            x_new = x + a * p
            grad_new = self.jacobian(x_new)
            
        return a

    def fit(
        self, x: np.ndarray, y: np.ndarray, init_guess: np.ndarray = None
    ) -> np.ndarray:
        self.x = x
        self.y = y
        self.weights = init_guess

        d = len(init_guess)
        grad = self.jacobian(init_guess)  # init grad
        h = np.eye(d)  # init hessian
        it = 2

        while np.linalg.norm(grad) > 1e-5:
            self.fits.append(self.predict(self.x))
            self.residuals.append(self.residual(self.x, self.weights))

            if it > self.max_iter:
                break
            it += 1
            # print(it)
            p = -h @ grad  # newton
            a = self.line_search(self.weights, p, grad)
            s = a * p
            x_new = self.weights + a * p
            grad_new = self.jacobian(x_new)
            y = grad_new - grad
            y = np.array([y])
            s = np.array([s])
            y = np.reshape(y, (d, 1))
            s = np.reshape(s, (d, 1))
            r = 1 / (y.T @ s)
            li = np.eye(d) - (r * (s @ y.T))
            ri = np.eye(d) - (r * (y @ s.T))
            hessian_upd = li @ h @ ri
            h = hessian_upd + (r * (s @ s.T))  # main Update
            grad = grad_new[:]
            self.weights = x_new[:]

        return self.weights, self.fits, self.residuals
