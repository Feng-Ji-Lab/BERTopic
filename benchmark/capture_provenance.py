"""Validate frozen inputs and archive Windows benchmark provenance."""
import argparse
import hashlib
import json
import os
import platform
import subprocess
from importlib.metadata import distributions
from pathlib import Path


def sha256_file(path):
    digest = hashlib.sha256()
    with open(path, "rb") as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--repo", required=True)
    parser.add_argument("--documents", required=True)
    parser.add_argument("--embeddings", required=True)
    parser.add_argument("--output", required=True)
    parser.add_argument("--package-archive", default="")
    parser.add_argument("--release-tag", default="v0.1.1")
    args = parser.parse_args()
    import numpy as np
    import psutil

    repo = Path(args.repo).resolve()
    embeddings = Path(args.embeddings).resolve()
    manifest = json.loads(embeddings.with_suffix(".manifest.json").read_text(encoding="utf-8"))
    if sha256_file(embeddings) != manifest["embeddings_sha256"]:
        raise ValueError("Frozen embedding hash differs from its manifest")
    if sha256_file(args.documents) != manifest["documents_file_sha256"]:
        raise ValueError("Document file hash differs from the frozen input manifest")
    array = np.load(embeddings, allow_pickle=False)
    if array.ndim != 2 or array.dtype != np.float64 or not array.flags.f_contiguous:
        raise ValueError("Frozen embeddings must be a float64 column-major matrix; rerun prepare_inputs.py")
    if manifest.get("embeddings_dtype") != "float64" or manifest.get("embeddings_memory_order") != "F":
        raise ValueError("Input manifest must record the canonical dtype and memory order")

    def git(*arguments):
        return subprocess.check_output(
            ["git", "-c", f"safe.directory={repo.as_posix()}", "-C", str(repo), *arguments],
            text=True, encoding="utf-8",
        ).strip()

    archive = Path(args.package_archive).resolve() if args.package_archive else None
    files = [repo / "benchmark" / name for name in (
        "prepare_inputs.py", "python_worker.py", "r_worker.R", "run_benchmark.R", "capture_provenance.py",
    )]
    record = {
        "git_commit": git("rev-parse", "HEAD"),
        "release_tag": args.release_tag,
        "release_commit": git("rev-parse", f"{args.release_tag}^{{commit}}"),
        "package_archive": str(archive) if archive else None,
        "package_archive_sha256": sha256_file(archive) if archive else None,
        "script_sha256": {str(path.relative_to(repo)): sha256_file(path) for path in files},
        "input_manifest": manifest,
        "embeddings_shape": list(array.shape),
        "embeddings_dtype": str(array.dtype),
        "embeddings_memory_order": "F",
        "python_executable": os.sys.executable,
        "python_version": os.sys.version,
        "python_distributions": dict(sorted(
            (dist.metadata["Name"], dist.version) for dist in distributions() if dist.metadata["Name"]
        )),
        "windows_platform": platform.platform(),
        "windows_version": list(platform.win32_ver()),
        "machine": platform.machine(),
        "processor": platform.processor(),
        "processor_identifier": os.environ.get("PROCESSOR_IDENTIFIER"),
        "physical_cpu_count": psutil.cpu_count(logical=False),
        "logical_cpu_count": psutil.cpu_count(logical=True),
        "total_memory_bytes": psutil.virtual_memory().total,
        "process_settings": {
            name: os.environ.get(name) for name in (
                "PYTHONHASHSEED", "OMP_NUM_THREADS", "MKL_NUM_THREADS", "OPENBLAS_NUM_THREADS", "NUMEXPR_NUM_THREADS",
            )
        },
    }
    Path(args.output).write_text(json.dumps(record, indent=2) + "\n", encoding="utf-8")


if __name__ == "__main__":
    main()
