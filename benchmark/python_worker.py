"""Run one direct-Python BERTopic benchmark process."""
import argparse
import csv
import os
import random
import time
from pathlib import Path


def write_metrics(path, values):
    with open(path, "w", newline="", encoding="utf-8") as stream:
        writer = csv.writer(stream)
        writer.writerow(["metric", "value"])
        writer.writerows(values.items())


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--documents", required=True)
    parser.add_argument("--embeddings", required=True)
    parser.add_argument("--output", required=True)
    parser.add_argument("--max-docs", type=int, required=True)
    parser.add_argument("--seed", type=int, required=True)
    parser.add_argument("--min-cluster-size", type=int, default=10)
    args = parser.parse_args()

    os.environ["PYTHONHASHSEED"] = str(args.seed)
    for name in ("OMP_NUM_THREADS", "MKL_NUM_THREADS", "OPENBLAS_NUM_THREADS", "NUMEXPR_NUM_THREADS"):
        os.environ[name] = "1"

    from importlib.metadata import version as distribution_version
    import numpy as np
    import pandas as pd
    import psutil
    import sklearn
    import torch
    import transformers
    import umap
    import hdbscan
    from bertopic import BERTopic
    from bertopic.dimensionality import BaseDimensionalityReduction
    from sklearn.feature_extraction.text import CountVectorizer

    random.seed(args.seed)
    np.random.seed(args.seed)
    documents = pd.read_csv(args.documents, encoding="utf-8")["text"].astype(str).tolist()[: args.max_docs]
    embeddings = np.asfortranarray(
        np.asarray(np.load(args.embeddings, allow_pickle=False), dtype=np.float64)[: len(documents)]
    )
    if embeddings.shape[0] != len(documents):
        raise ValueError("The frozen embedding rows do not match the selected documents")

    dimensionality_model = BaseDimensionalityReduction()
    cluster_model = hdbscan.HDBSCAN(
        min_cluster_size=args.min_cluster_size,
        metric="euclidean",
        cluster_selection_method="eom",
        prediction_data=True,
    )
    vectorizer_model = CountVectorizer(stop_words="english")

    started = time.perf_counter()
    model = BERTopic(
        umap_model=dimensionality_model,
        hdbscan_model=cluster_model,
        vectorizer_model=vectorizer_model,
        calculate_probabilities=True,
    )
    topics, probabilities = model.fit_transform(documents, embeddings=embeddings)
    fit_seconds = time.perf_counter() - started

    output = Path(args.output)
    output.mkdir(parents=True, exist_ok=True)
    np.savetxt(output / "topics.csv", np.asarray(topics, dtype=np.int64), fmt="%d")
    if probabilities is None:
        (output / "probabilities.csv").write_text("", encoding="utf-8")
    else:
        np.savetxt(output / "probabilities.csv", np.asarray(probabilities), delimiter=",", fmt="%.17g")

    info = model.get_topic_info().loc[:, ["Topic", "Count", "Name"]]
    info.to_csv(output / "topic_info.csv", index=False, encoding="utf-8")
    with open(output / "topic_terms.csv", "w", newline="", encoding="utf-8") as stream:
        writer = csv.writer(stream)
        writer.writerow(["Topic", "Rank", "Term", "Weight"])
        for topic in info["Topic"].tolist():
            for rank, (term, weight) in enumerate(model.get_topic(int(topic))[:10], start=1):
                writer.writerow([int(topic), rank, term, format(float(weight), ".17g")])

    versions = {
        "fit_seconds": format(fit_seconds, ".17g"),
        "peak_rss_bytes": str(psutil.Process().memory_info().peak_wset),
        "python_version": os.sys.version.replace("\n", " "),
        "bertopic_version": __import__("bertopic").__version__,
        "numpy_version": np.__version__,
        "sklearn_version": sklearn.__version__,
        "torch_version": torch.__version__,
        "transformers_version": transformers.__version__,
        "umap_version": umap.__version__,
        "hdbscan_version": distribution_version("hdbscan"),
        "documents": str(len(documents)),
        "topics": str(len(info)),
        "outliers": str(sum(int(topic) == -1 for topic in topics)),
    }
    write_metrics(output / "metrics.csv", versions)


if __name__ == "__main__":
    main()