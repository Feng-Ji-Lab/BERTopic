"""Validate the retained Windows analysis archive without importing ML packages."""
import argparse
import hashlib
import json
from pathlib import Path

def digest(path):
    with path.open("rb") as stream:
        checksum = hashlib.sha256()
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            checksum.update(chunk)
        return checksum.hexdigest()

def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--write", action="store_true", help="Record hashes after validating execution provenance")
    args = parser.parse_args()
    root = Path(__file__).resolve().parents[1]
    manifest = root / "benchmark/archive_sha256.json"
    example = root / "benchmark/example-results"
    data = json.loads((example / "manifest.json").read_text(encoding="utf-8"))
    missing = []
    for name, expected in data["artifact_sha256"].items():
        path = example / name
        if name == "model.pkl" and not path.exists():
            missing.append(name)
            continue
        if digest(path) != expected:
            raise ValueError(f"Worked-example hash mismatch: {name}")
    for name, key in [("run_example.R", "script_sha256"), ("example_io.R", "helper_sha256")]:
        if digest(root / "benchmark" / name) != data[key]:
            raise ValueError(f"Executed script mismatch: {name}")
    prov = json.loads((root / "benchmark/results/provenance.json").read_text(encoding="utf-8"))
    for name, expected in prov["script_sha256"].items():
        if digest(root / name.replace(chr(92), "/")) != expected:
            raise ValueError(f"Worker script mismatch: {name}")
    archive = root / "provenance/releases/BERTopic_0.1.2.tar.gz"
    if digest(archive) != prov["package_archive_sha256"] or digest(archive) != data["package_archive_sha256"]:
        raise ValueError("Release archive mismatch")
    inp = prov["input_manifest"]
    if digest(root / "data/sms_spam.csv") != inp["documents_file_sha256"]:
        raise ValueError("Documents mismatch")
    if digest(root / "benchmark/inputs/reduced_embeddings.npy") != inp["embeddings_sha256"]:
        raise ValueError("Frozen embeddings mismatch")
    if args.write:
        files = [p for folder in ["benchmark", "provenance"] for p in (root / folder).rglob("*")
                 if p.is_file() and p != manifest and p.name != "model.pkl"
                 and "__pycache__" not in p.parts and p.suffix != ".pyc"]
        hashes = {p.relative_to(root).as_posix(): digest(p) for p in sorted(files)}
        manifest.write_text(json.dumps(hashes, indent=2) + "\n", encoding="utf-8")
    hashes = json.loads(manifest.read_text(encoding="utf-8"))
    for name, expected in hashes.items():
        if digest(root / name) != expected:
            raise ValueError(f"Archive mismatch: {name}")
    print(f"PASS: {len(hashes)} retained files; release, scripts, input and example hashes verified.")
    if missing:
        print("Large model.pkl omitted from Git; regenerate with run_example.R (recorded hash retained).")

if __name__ == "__main__":
    main()
