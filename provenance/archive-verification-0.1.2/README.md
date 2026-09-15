# Offline archive reconstruction verification

The publication ZIP and SOURCE.bundle were built from committed analysis source 2aef241. A fresh detached checkout was cloned from that bundle, all 260 original retained hashes passed, and the exact R 0.1.2 source archive was installed into a separate R library.

The Windows reproduction entrypoint completed with exit code 0: five independent R/Python pairs passed every comparison, and all six full-example restoration checks passed. Eleven worked-example numerical CSV files matched the formal retained outputs byte for byte (see verification.json).

The Python environment and other R dependencies were the existing validated Windows stack; this is not a claim of a second clean Conda installation. Standard upstream embedding/pickle notices and the existing reticulate R-build-version warning were emitted. Confirmation timings are retained here but do not replace the manuscript's formal benchmark results.

The final upload adds this verification record and documentation fixes. Executed benchmark, example and reproduction scripts remain the same as the validated source. Public DOI and author facts are pending; E stays deferred.
