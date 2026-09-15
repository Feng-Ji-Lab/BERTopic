"""Build and verify the committed Windows analysis upload with offline Git provenance."""
import argparse
import datetime
import hashlib
import json
import subprocess
import sys
import zipfile
from pathlib import Path

def git(root, *args):
    return subprocess.check_output(["git", "-C", str(root), *args]).decode("utf-8").strip()

def checksum(path):
    value = hashlib.sha256()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            value.update(chunk)
    return value.hexdigest()

def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", default="../bertopic-publication-0.1.2")
    args = parser.parse_args()
    root = Path(__file__).resolve().parents[2]
    if git(root, "status", "--porcelain"):
        raise ValueError("Commit all source/metadata changes before packaging")
    commit = git(root, "rev-parse", "HEAD")
    release = git(root, "rev-parse", "v0.1.2^{commit}")
    if release != "faee106360389a8e75199893f6e8c68c28876c8d":
        raise ValueError("Exact release tag changed")
    subprocess.run([sys.executable, str(root / "benchmark/verify_archive.py")], check=True)
    output = Path(args.output).resolve()
    if output == root or root in output.parents:
        raise ValueError("Choose an output directory outside the source repository")
    output.mkdir(parents=True, exist_ok=True)
    archive = output / "BERTopic-analysis-0.1.2.zip"
    bundle = output / "SOURCE.bundle"
    sha_file = output / "SHA256SUMS.txt"
    inventory_file = output / "upload_inventory.json"
    if any(p.exists() for p in [archive, bundle, sha_file, inventory_file]):
        raise FileExistsError("Publication output files already exist; choose a new directory")
    subprocess.run(["git", "-C", str(root), "bundle", "create", str(bundle),
                    "HEAD", "refs/tags/v0.1.2"], check=True)
    subprocess.run(["git", "-C", str(root), "bundle", "verify", str(bundle)],
                   check=True, stdout=subprocess.DEVNULL)
    prefix = "BERTopic-analysis-0.1.2/"
    subprocess.run(["git", "-c", "core.autocrlf=false", "-C", str(root), "archive", "--format=zip",
                    "--prefix=" + prefix, "--output=" + str(archive), "HEAD"], check=True)
    stamp = datetime.datetime.fromtimestamp(int(git(root, "show", "-s", "--format=%ct", "HEAD")),
                                            datetime.timezone.utc).timetuple()[:6]
    with zipfile.ZipFile(archive, "a", compression=zipfile.ZIP_DEFLATED) as packed:
        item = zipfile.ZipInfo("SOURCE.bundle", stamp)
        item.compress_type = zipfile.ZIP_DEFLATED
        packed.writestr(item, bundle.read_bytes())
    expected = json.loads((root / "benchmark/archive_sha256.json").read_text(encoding="utf-8"))
    provenance = json.loads((root / "benchmark/results/provenance.json").read_text(encoding="utf-8"))
    with zipfile.ZipFile(archive) as packed:
        if packed.testzip() is not None:
            raise ValueError("ZIP CRC verification failed")
        documents_hash = hashlib.sha256(packed.read(prefix + "data/sms_spam.csv")).hexdigest()
        if documents_hash != provenance["input_manifest"]["documents_file_sha256"]:
            raise ValueError("Packaged CSV differs from the frozen benchmark input")
        for name, digest in expected.items():
            if hashlib.sha256(packed.read(prefix + name)).hexdigest() != digest:
                raise ValueError("Packaged file mismatch: " + name)
        if hashlib.sha256(packed.read("SOURCE.bundle")).hexdigest() != checksum(bundle):
            raise ValueError("Bundled Git source mismatch")
        entries = [{"path": info.filename, "bytes": info.file_size}
                   for info in packed.infolist() if not info.is_dir()]
    record = {"analysis_commit": commit, "package_release_commit": release,
              "package_release_tag": "v0.1.2", "archive": archive.name,
              "archive_sha256": checksum(archive), "bytes": archive.stat().st_size,
              "verified_manifest_files": len(expected), "contents": entries,
              "pending": ["Published Zenodo/OSF DOI", "Confirmed archive creators/license",
                          "Author affiliations/declarations/CRediT", "E deferred by user"]}
    inventory_file.write_text(json.dumps(record, indent=2) + "\n", encoding="utf-8")
    sha_file.write_text("".join(checksum(p) + "  " + p.name + "\n"
                               for p in [archive, bundle, inventory_file]), encoding="utf-8")
    print(f"PASS: {len(expected)} archived hashes and {len(entries)} ZIP files verified.")
    print(f"Upload: {archive}")
    print(f"SHA-256: {record['archive_sha256']}")

if __name__ == "__main__":
    main()
