import numpy as np
from tqdm import trange

class MyRNN:
    def __init__(self, input_dim, hidden_dim, output_dim, bptt_truncate, learning_rate, max_clip_value, min_clip_value):
        self.input_dim = input_dim
        self.hidden_dim = hidden_dim
        self.output_dim = output_dim
        self.bptt_truncate = bptt_truncate
        self.learning_rate = learning_rate
        self.max_clip_value = max_clip_value
        self.min_clip_value = min_clip_value
        self.U = np.random.uniform(-np.sqrt(1. / input_dim), np.sqrt(1. / input_dim), (hidden_dim, input_dim))
        self.V = np.random.uniform(-np.sqrt(1. / hidden_dim), np.sqrt(1. / hidden_dim), (output_dim, hidden_dim))
        self.W = np.random.uniform(-np.sqrt(1. / hidden_dim), np.sqrt(1. / hidden_dim), (hidden_dim, hidden_dim))

    def sigmoid(self, x):
        return 1 / (1 + np.exp(-x))

    def forward_propagation(self, x):
        T = len(x)
        layers = []
        prev_s = np.zeros((self.hidden_dim, 1))
        for t in range(T):
            new_input = np.zeros(x.shape)
            new_input[t] = x[t]
            mulu = np.dot(self.U, new_input)
            mulw = np.dot(self.W, prev_s)
            add = mulw + mulu
            s = self.sigmoid(add)
            mulv = np.dot(self.V, s)
            layers.append({'s': s, 'prev_s': prev_s})
            prev_s = s
        return layers

    def calculate_loss(self, X, Y):
        loss = 0.0
    
        for i in range(Y.shape[0]):
            x, y = X[i], Y[i]                    
            prev_s = np.zeros((self.hidden_dim, 1))   
            for t in range(self.input_dim):
                new_input = np.zeros(x.shape)    
                new_input[t] = x[t]              
                mulu = np.dot(self.U, new_input)
                mulw = np.dot(self.W, prev_s)
                add = mulw + mulu
                s = self.sigmoid(add)
                mulv = np.dot(self.V, s)
                prev_s = s

            loss_per_record = (y - mulv)**2 / 2
            loss += loss_per_record

        return loss / float(y.shape[0])

    def backward_propagation(self, X, Y):
        for i in range(Y.shape[0]):
            x, y = X[i], Y[i]

            layers = []
            prev_s = np.zeros((self.hidden_dim, 1))
            dU = np.zeros(self.U.shape)
            dV = np.zeros(self.V.shape)
            dW = np.zeros(self.W.shape)

            dU_t = np.zeros(self.U.shape)
            dV_t = np.zeros(self.V.shape)
            dW_t = np.zeros(self.W.shape)

            dU_i = np.zeros(self.U.shape)
            dW_i = np.zeros(self.W.shape)


            for t in range(self.input_dim):
                new_input = np.zeros(x.shape)
                new_input[t] = x[t]
                mulu = np.dot(self.U, new_input)
                mulw = np.dot(self.W, prev_s)
                add = mulw + mulu
                s = self.sigmoid(add)
                mulv = np.dot(self.V, s)
                layers.append({'s':s, 'prev_s':prev_s})
                prev_s = s

            dmulv = (mulv - y)

            for t in range(self.input_dim):
                dV_t = np.dot(dmulv, np.transpose(layers[t]['s']))
                dsv = np.dot(np.transpose(self.V), dmulv)

                ds = dsv
                dadd = add * (1 - add) * ds

                dmulw = dadd * np.ones_like(mulw)

                dprev_s = np.dot(np.transpose(self.W), dmulw)


                for i in range(t-1, max(-1, t-self.bptt_truncate-1), -1):
                    ds = dsv + dprev_s
                    dadd = add * (1 - add) * ds

                    dmulw = dadd * np.ones_like(mulw)
                    dmulu = dadd * np.ones_like(mulu)

                    dW_i = np.dot(self.W, layers[t]['prev_s'])
                    dprev_s = np.dot(np.transpose(self.W), dmulw)

                    new_input = np.zeros(x.shape)
                    new_input[t] = x[t]
                    dU_i = np.dot(self.U, new_input)

                    dU_t += dU_i
                    dW_t += dW_i

                dV += dV_t
                dU += dU_t
                dW += dW_t

                self.clip(dU, dV, dW)

        return dU, dV, dW
    
    def clip(self, dU, dV, dW):
        if dU.max() > self.max_clip_value:
            dU[dU > self.max_clip_value] = self.max_clip_value
        if dV.max() > self.max_clip_value:
            dV[dV > self.max_clip_value] = self.max_clip_value
        if dW.max() > self.max_clip_value:
            dW[dW > self.max_clip_value] = self.max_clip_value

        if dU.min() < self.min_clip_value:
            dU[dU < self.min_clip_value] = self.min_clip_value
        if dV.min() < self.min_clip_value:
            dV[dV < self.min_clip_value] = self.min_clip_value
        if dW.min() < self.min_clip_value:
            dW[dW < self.min_clip_value] = self.min_clip_value

    def train(self, X, Y, nepoch):
        for epoch in range(nepoch):
            loss = self.calculate_loss(X, Y)
            print('Epoch: ', epoch + 1, ', Loss: ', loss.item)
            dU, dV, dW = self.backward_propagation(X, Y)
            self.U -= self.learning_rate * dU
            self.V -= self.learning_rate * dV
            self.W -= self.learning_rate * dW
            
    def predict(self, init, n_points):
        preds = []
        window = init  # Initialize the window with the first data point

        for i in range(n_points):
            x = window
            prev_s = np.zeros((self.hidden_dim, 1))

            for t in range(self.hidden_dim):
                mulu = np.dot(self.U, x)
                mulw = np.dot(self.W, prev_s)
                add = mulw + mulu
                s = self.sigmoid(add)
                mulv = np.dot(self.V, s)
                prev_s = s

            window = window.tolist()
            new_tick = mulv[0, 0]
            window.append([new_tick])
            window = np.array(window[1:])

            preds.append(mulv)

        preds = np.array(preds)
        return preds
