class Neuron:
    def __init__(self, bin_repr):
        self.weights = [-1.0 if digit == '0' else 1.0 for digit in bin_repr]  # DNF
        self.ones = self.weights.count(1)
        self.bias = 0.5 - self.ones

    def __str__(self):
        weights_str = " ".join(map(str, self.weights))
        return f"{weights_str} {self.bias}"


class MLP:
    def __init__(self, layers, non_zero):
        self.neurons = [Neuron(binary_argument) for binary_argument in layers]
        self.last_layer = [1.0 for _ in range(non_zero)] + [-0.5]

    def __str__(self):
        header = f"{len(self.last_layer) - 1} 1"
        neurons = "\n".join(map(str, self.neurons))
        last = " ".join(map(str, self.last_layer))
        return "\n".join(["2", header, neurons, last])


def main():
    m = int(input())

    layers = []
    non_zero = 0

    for i in range(2 ** m):
        func_eval = int(input())
        if func_eval:
            non_zero += 1
            layers.append(format(i, 'b').zfill(m)[::-1])

    if non_zero == 0:  # False Boolean
        print(1)
        print(1)
        print(*[0] * m, -1)
    else:
        mlp = MLP(layers, non_zero)
        print(mlp)


if __name__ == "__main__":
    main()
