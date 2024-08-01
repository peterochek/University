import numpy as np


def f_measure(precision, recall):
    return 2 * (precision * recall) / (precision + recall) if precision + recall > 0 else 0


def weighted_f_measure(conf_matrix):
    num_classes = len(conf_matrix)
    f_measures = []
    class_counts = np.sum(conf_matrix, axis=1)
    tp = np.diag(conf_matrix)
    fp = np.sum(conf_matrix, axis=0) - tp
    fn = class_counts - tp

    micro_precision = np.sum(tp) / np.sum(tp + fp)
    micro_recall = np.sum(tp) / np.sum(tp + fn)
    micro_f_measure = f_measure(micro_precision, micro_recall)

    macro_precision = np.mean(tp / (tp + fp))
    macro_recall = np.mean(tp / (tp + fn))
    macro_f_measure = f_measure(macro_precision, macro_recall)

    for i in range(num_classes):
        precision = tp[i] / (tp[i] + fp[i])
        recall = tp[i] / (tp[i] + fn[i])
        f_measures.append(f_measure(precision, recall))

    return micro_f_measure, macro_f_measure, np.mean(f_measures)


K = int(input())
conf_matrix = []
for _ in range(K):
    row = list(map(int, input().split()))
    conf_matrix.append(row)

conf_matrix = np.array(conf_matrix)

print(*weighted_f_measure(conf_matrix))
