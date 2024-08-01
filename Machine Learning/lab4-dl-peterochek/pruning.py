import torch
import torch.nn as nn
import torch.nn.functional as F
import torch.optim as optim
from torch.utils.data import DataLoader
from torchvision import datasets, transforms

from nets import ResNet
from pipelines import HIDEN_SIZE, INPUT_SIZE, N_CLASSES, device, train_model


def losses_and_accs(num_layers, lr=0.001, epochs=10, init_type="xavier"):
    net = ResNet(INPUT_SIZE, HIDEN_SIZE, N_CLASSES, num_layers, init_type).to(device)
    criterion = nn.CrossEntropyLoss()
    optimizer = optim.Adam(net.parameters(), lr=lr)
    losses, accs = train_model(
        net,
        criterion=criterion,
        optimizer=optimizer,
        num_epochs=epochs,
    )

    return net, losses, accs
