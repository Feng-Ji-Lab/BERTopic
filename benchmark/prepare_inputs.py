"""Prepare one frozen reduced-embedding array for the paired benchmark."""
import argparse
import hashlib
import json
import os
from pathlib import Path


def sha256_file(path):
    digest = hashlib.sha256()
    with open(path, "rb") as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def selected_documents_hash(documents):
    digest = hashlib.sha256()
    for document in documents:
        encoded = document.encode("utf-8")
        digest.update(len(encoded).to_bytes(8, "big"))
        digest.update(encoded)
    return digest.hexdigest()


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--documents", default="data/sms_spam.csv")
    parser.add_argument("--output", default="benchmark/inputs/reduced_embeddings.npy")
    parser.add_argument("--max-docs", type=int, default=2247)
    parser.add_argument("--seed", type=int, default=42)
    parser.add_argument("--embedding-model", default="all-MiniLM-L6-v2")
    parser.add_argument("--model-revision", default="")
    parser.add_argument("--reduced-dim", type=int, default=5)
    args = parser.parse_args()
    if args.max_docs < 1 or args.reduced_dim < 1:
        parser.error("--max-docs and --reduced-dim must be positive")

    os.environ["PYTHONHASHSEED"] = str(args.seed)
    for name in ("OMP_NUM_THREADS", "MKL_NUM_THREADS", "OPENBLAS_NUM_THREADS", "NUMEXPR_NUM_THREADS"):
        os.environ[name] = "1"

    from importlib.metadata import version as distribution_version
    import numpy as np
    import pandas as pd

    documents_path = Path(args.documents).resolve()
    frame = pd.read_csv(documents_path, encoding="utf-8")
    if "text" not in frame.columns:
        raise ValueError(f"{documents_path} does not contain a 'text' column")
    documents = frame["text"].astype(str).tolist()[: args.max_docs]
    if not documents:
        raise ValueError("No documents selected")

    if args.embedding_model == "synthetic":
        reduced = np.random.RandomState(args.seed).normal(
            size=(len(documents), args.reduced_dim)
        ).astype(np.float32)
        source_dimensions = args.reduced_dim
    else:
        if not args.model_revision:
            parser.error("--model-revision is required unless --embedding-model synthetic is used")
        from sentence_transformers import SentenceTransformer
        import umap

        encoder = SentenceTransformer(args.embedding_model, revision=args.model_revision)
        encoded = encoder.encode(documents, show_progress_bar=True)
        source_dimensions = int(encoded.shape[1])
        reduced = umap.UMAP(
            n_neighbors=15,
            n_components=args.reduced_dim,
            min_dist=0,
            metric="cosine",
            random_state=args.seed,
        ).fit_transform(encoded)

    # Match the float type and column-major layout used by R/reticulate.
    # HDBSCAN soft-membership calculations can be sensitive to array strides.
    reduced = np.asfortranarray(reduced, dtype=np.float64)

    output_path = Path(args.output).resolve()
    output_path.parent.mkdir(parents=True, exist_ok=True)
    np.save(output_path, reduced, allow_pickle=False)
    manifest = {
        "documents_file": str(documents_path),
        "documents_file_sha256": sha256_file(documents_path),
        "selected_documents": len(documents),
        "selected_documents_sha256": selected_documents_hash(documents),
        "selected_documents_hash_format": "sha256(length:uint64be || utf8(document))",
        "embedding_model": args.embedding_model,
        "model_revision": args.model_revision or None,
        "source_dimensions": source_dimensions,
        "reduced_dimensions": int(reduced.shape[1]),
        "embeddings_dtype": str(reduced.dtype),
        "embeddings_memory_order": "F" if reduced.flags.f_contiguous else "C",
        "seed": args.seed,
        "embeddings_file": str(output_path),
        "embeddings_sha256": sha256_file(output_path),
        "python_version": os.sys.version.replace("\n", " "),
        "numpy_version": np.__version__,
        "sentence_transformers_version": distribution_version("sentence-transformers"),
        "umap_learn_version": distribution_version("umap-learn"),
        "torch_version": distribution_version("torch"),
        "thread_settings": {
            name: os.environ[name]
            for name in ("PYTHONHASHSEED", "OMP_NUM_THREADS", "MKL_NUM_THREADS", "OPENBLAS_NUM_THREADS", "NUMEXPR_NUM_THREADS")
        },
        "umap": {"n_neighbors": 15, "n_components": args.reduced_dim, "min_dist": 0, "metric": "cosine", "random_state": args.seed},
    }
    manifest_path = output_path.with_suffix(".manifest.json")
    manifest_path.write_text(json.dumps(manifest, indent=2, ensure_ascii=False) + "\n", encoding="utf-8")
    print(f"Wrote {output_path}")
    print(f"Wrote {manifest_path}")


if __name__ == "__main__":
    main()