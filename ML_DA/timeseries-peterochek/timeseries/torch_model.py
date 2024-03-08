import torch
import torch.nn as nn
import torch.optim as optim
import numpy as np
from tqdm import trange


class TorchRNN(nn.Module):
    def __init__(self, input_size, hidden_size, num_layers, output_size):
        super(TorchRNN, self).__init__()
        self.rnn = nn.RNN(input_size, hidden_size, num_layers, batch_first=True)
        self.fc = nn.Linear(hidden_size, output_size)

    def forward(self, x):
        out, _ = self.rnn(x)
        out = self.fc(out[:, -1, :])
        return out