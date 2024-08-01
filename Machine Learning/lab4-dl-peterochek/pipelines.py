import torch
import torch.nn as nn
import torch.optim as optim
from tqdm import trange

from data import get_test_batches, get_train_batches


def train_model(model, criterion, optimizer, num_epochs=10):
    losses = []
    accs = []
    for _ in range(num_epochs):
        model.train()
        cur_losses = []
        for batch_images, batch_labels in get_train_batches():
            outputs = model(batch_images)
            loss = criterion(outputs, batch_labels)

            cur_losses.append(loss.item())

            optimizer.zero_grad()
            loss.backward()
            optimizer.step()

        losses.append(sum(cur_losses))
        accs.append(evaluate_model(model))

    return losses, accs


def evaluate_model(model):
    correct = 0
    total = 0
    model.eval()
    with torch.no_grad():
        for batch_images, batch_labels in get_test_batches():
            outputs = model(batch_images)
            _, predicted = torch.max(outputs.data, 1)
            correct += (predicted == batch_labels).sum().item()
            total += len(batch_labels)
    return correct / total


INPUT_SIZE = 28 * 28
HIDEN_SIZE = 256
N_CLASSES = 10


cuda_available = torch.cuda.is_available()
device = torch.device("cuda" if cuda_available else "cpu")


def losses_and_accs(model_cls, num_layers=2, lr=0.001, epochs=10, init_type="xavier"):
    net = model_cls(INPUT_SIZE, HIDEN_SIZE, N_CLASSES, num_layers, init_type).to(device)
    criterion = nn.CrossEntropyLoss()
    optimizer = optim.Adam(net.parameters(), lr=lr)
    losses, accs = train_model(
        net,
        criterion=criterion,
        optimizer=optimizer,
        num_epochs=epochs,
    )

    return losses, accs
