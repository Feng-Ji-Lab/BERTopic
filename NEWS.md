# BERTopic 0.1.2

- Fixed default safetensors/PyTorch saving on the supported Windows BERTopic 0.16.0 backend without adding an unwanted embedding-model reference.
- Mapped R's pt serialization to Python's pytorch format and forwarded explicit Hugging Face model references unchanged.
- Preserved original save failures instead of retrying pickle, and protected existing destinations unless overwrite is explicit.
- Added regression tests for lightweight weight files, metadata restoration, reference handling, and original-error propagation.
# BERTopic 0.1.1

## Correctness

- Fixed representative-document conversion and topic-ID handling.
- Validated `nr_topics` and synchronized returned R-side topics and probabilities after topic reduction.
- Converted custom-label keys to Python integers.
- Mapped document-topic matrix columns to the fitted model's actual topic IDs, including sparse output and nonconsecutive IDs.
- Restored topics and probabilities when loading models and verified post-load transformation.
- Made `bertopic_self_check()` perform and compare a deterministic fit/transform/save/load round trip.
- Restored embedding models through BERTopic's backend selector.
- Handled degenerate hierarchical-document visualizations and propagated visualization failures to tests.
- Registered `fortify.bertopic_r` as a delayed ggplot2 S3 method.

## Installation and reproducibility

- Declared Python BERTopic 0.16.0 as the supported backend.
- Made Conda and virtualenv installers consume one exact dependency specification and run the same import validation.
- Added exact module versions to `bertopic_session_info()`.
- Added Windows Python 3.10.21 / BERTopic 0.16.0 environment and session provenance.
- Added the standard testthat entry point so `R CMD check` runs the regression suite.
- Added a Windows fresh-process benchmark harness with immutable input hashes, separate R and Python workers, version gates, raw outputs, fit/cold timing, and peak-memory recording.
