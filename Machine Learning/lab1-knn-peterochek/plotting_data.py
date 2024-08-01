from collections import defaultdict

from sklearn.metrics import accuracy_score
from tqdm import trange


def plot_accuracies(models, X_train, y_train, X_val, y_val, X_test, y_test):
    train_accuracies = defaultdict(list)
    val_accuracies = defaultdict(list)
    test_accuracies = defaultdict(list)

    for model_name, model_class in models.items():
        for k in trange(1, 21):
            model_instance = model_class(
                n_neighbors=k, weights="uniform", metric="euclidean"
            )

            model_instance.fit(X_train, y_train)

            for acc_dict, (X, y) in zip(
                [train_accuracies, val_accuracies, test_accuracies],
                [(X_train, y_train), (X_val, y_val), (X_test, y_test)],
            ):
                preds = model_instance.predict(X)
                accuracy = accuracy_score(y, preds)
                acc_dict[model_name].append(accuracy)

    return train_accuracies, val_accuracies, test_accuracies
