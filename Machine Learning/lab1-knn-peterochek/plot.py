import pandas as pd
from matplotlib import pyplot as plt

from constants import MY_MODEL, SCIKIT_LEARN_MODEL, colors, map_index
from plotting_data import plot_accuracies


def prepare_data(data):
    df = pd.DataFrame(data)

    df_melted = df.melt(var_name="Model", value_name="Values")
    df_melted["Index"] = df_melted.groupby("Model").cumcount()

    df_exploded = df_melted.explode("Values")
    df_exploded["Values"] = pd.to_numeric(df_exploded["Values"])

    df_exploded["Index"] = df_exploded["Index"].apply(lambda x: map_index[x])

    df_exploded["X"] = [i % 20 + 1 for i in range(20 * 6)]

    return df_exploded


def plot(models, X_train, y_train, X_val, y_val, X_test, y_test):
    data = plot_accuracies(models, X_train, y_train, X_val, y_val, X_test, y_test)

    df_exploded = prepare_data(data)

    fig, ax = plt.subplots()

    for key, group in df_exploded.groupby(["Model", "Index"]):
        model, index = key
        params = {
            "ax": ax,
            "kind": "line",
            "x": "X",
            "y": "Values",
            "label": f"{model} - {index}",
            "color": colors[(model, index)],
        }

        if model == SCIKIT_LEARN_MODEL:
            group.plot(**params)
        else:
            group.plot(**params, linewidth=5)

    plt.xlabel("# neighbors")
    plt.ylabel("Values")
    plt.title("Accuracy")
    plt.legend(title="Model - data")
    plt.grid(True)
    plt.show()
