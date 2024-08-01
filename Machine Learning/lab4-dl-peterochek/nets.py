import torch
import torch.nn as nn
import torch.nn.functional as F


def init_weights(m, init_type="xavier"):
    if type(m) == nn.Linear:
        if init_type == "xavier":
            nn.init.xavier_uniform_(m.weight)
        elif init_type == "he":
            nn.init.kaiming_uniform_(m.weight, nonlinearity="relu")
        if m.bias is not None:
            m.bias.data.fill_(0.01)


class MLP(nn.Module):
    def __init__(self, input_size, hidden_size, output_size, num_layers, init_type):
        super(MLP, self).__init__()
        layers = [nn.Linear(input_size, hidden_size), nn.ReLU()]
        for _ in range(num_layers - 2):
            layers += [nn.Linear(hidden_size, hidden_size), nn.ReLU()]
        layers.append(nn.Linear(hidden_size, output_size))
        self.layers = nn.Sequential(*layers)
        self.layers.apply(lambda m: init_weights(m, init_type))

    def forward(self, x):
        return self.layers(x)


class ResNetBlock(nn.Module):
    def __init__(self, input_size, init_type):
        super(ResNetBlock, self).__init__()
        self.linear1 = nn.Linear(input_size, input_size)
        self.relu = nn.ReLU()
        self.linear2 = nn.Linear(input_size, input_size)
        init_weights(self.linear1, init_type)
        init_weights(self.linear2, init_type)

    def forward(self, x):
        identity = x
        out = self.linear1(x)
        out = self.relu(out)
        out = self.linear2(out)
        out += identity
        return out


class ResNet(nn.Module):
    def __init__(self, input_size, hidden_size, output_size, num_blocks, init_type):
        super(ResNet, self).__init__()
        self.initial = nn.Linear(input_size, hidden_size)
        self.blocks = nn.Sequential(
            *(ResNetBlock(hidden_size, init_type) for _ in range(num_blocks))
        )
        self.final = nn.Linear(hidden_size, output_size)
        self.activations = []

    def forward(self, x):
        x = self.initial(x)
        self.activations = []
        for block in self.blocks:
            x = block(x)
            self.activations.append(x.detach().cpu().mean().item())
        return self.final(x)

    def prune_blocks(self, n):
        activation_indices = torch.tensor(self.activations).argsort()
        remove_indices = set(activation_indices[:n].tolist())
        self.blocks = nn.ModuleList(
            [block for i, block in enumerate(self.blocks) if i not in remove_indices]
        )


class MLP_BN(nn.Module):
    def __init__(self, input_size, hidden_size, output_size, num_layers, init_type):
        super(MLP_BN, self).__init__()
        self.layers = nn.ModuleList([nn.Linear(input_size, hidden_size)])
        self.bns = nn.ModuleList([nn.BatchNorm1d(hidden_size)])
        for _ in range(num_layers - 2):
            self.layers.append(nn.Linear(hidden_size, hidden_size))
            self.bns.append(nn.BatchNorm1d(hidden_size))
        self.layers.append(nn.Linear(hidden_size, output_size))
        self.layers.apply(lambda m: init_weights(m, init_type))

    def forward(self, x):
        for i, layer in enumerate(self.layers[:-1]):
            x = F.relu(self.bns[i](layer(x)))
        x = self.layers[-1](x)
        return x


class ResNetBlock_BN(nn.Module):
    def __init__(self, hidden_size, init_type="xavier"):
        super(ResNetBlock_BN, self).__init__()
        self.linear1 = nn.Linear(hidden_size, hidden_size)
        self.bn1 = nn.BatchNorm1d(hidden_size)
        self.linear2 = nn.Linear(hidden_size, hidden_size)
        self.bn2 = nn.BatchNorm1d(hidden_size)
        init_weights(self.linear1, init_type)
        init_weights(self.linear2, init_type)

    def forward(self, x):
        identity = x
        out = F.relu(self.bn1(self.linear1(x)))
        out = self.bn2(self.linear2(out))
        out += identity
        return out


class ResNet_BN(nn.Module):
    def __init__(self, input_size, hidden_size, output_size, num_blocks, init_type):
        super(ResNet_BN, self).__init__()
        self.initial = nn.Linear(input_size, hidden_size)
        self.blocks = nn.Sequential(
            *(ResNetBlock_BN(hidden_size, init_type) for _ in range(num_blocks))
        )
        self.final = nn.Linear(hidden_size, output_size)

    def forward(self, x):
        x = self.initial(x)
        x = self.blocks(x)
        return self.final(x)
