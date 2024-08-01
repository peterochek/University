import torch
from torchvision import datasets, transforms

cuda_available = torch.cuda.is_available()
device = torch.device("cuda" if cuda_available else "cpu")

transform = transforms.Compose(
    [transforms.ToTensor(), transforms.Normalize((0.5,), (0.5,))]
)

train_dataset = datasets.MNIST(
    root="./data", train=True, download=True, transform=transform
)
train_images = (
    torch.stack([image[0] for image in train_dataset]).view(-1, 28 * 28).to(device)
)
train_labels = torch.tensor([label[1] for label in train_dataset], dtype=torch.long).to(
    device
)

test_dataset = datasets.MNIST(
    root="./data", train=False, download=True, transform=transform
)
test_images = (
    torch.stack([image[0] for image in test_dataset]).view(-1, 28 * 28).to(device)
)
test_labels = torch.tensor([label[1] for label in test_dataset], dtype=torch.long).to(
    device
)


def get_train_batches(batch_size=1024):
    total_size = len(train_images)
    for i in range(0, total_size, batch_size):
        yield train_images[i : i + batch_size], train_labels[i : i + batch_size]


def get_test_batches(batch_size=1024):
    total_size = len(test_images)
    for i in range(0, total_size, batch_size):
        yield test_images[i : i + batch_size], test_labels[i : i + batch_size]
