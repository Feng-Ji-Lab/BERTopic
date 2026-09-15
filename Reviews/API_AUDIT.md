# Package/API audit for manuscript revision

Scope: package source at `4bdb931` and the validated Windows environment in `provenance/windows-bertopic-0.16.0.yml`. macOS and Linux claims are outside this audit. Numerical benchmark claims remain provisional until the final 0.1.1 release is installed and the five-pair run is complete.

| Manuscript claim | Code-level finding | Evidence | Status / manuscript action |
| --- | --- | --- | --- |
| `bertopic_fit(text, embeddings = NULL, ...)` validates text, checks Python, forwards `...`, and calls `fit_transform()` | Matches `R/fit.R`; supplied embeddings are converted from a numeric R matrix. | Roundtrip and exporter/predict tests. | Accurate. |
| A `bertopic_r` object retains `.py`, `topics`, and optional `probs` | Constructor and loader both return these fields. | `test-bertopic-roundtrip.R`, `test-save-load.R`. | Accurate. |
| `bertopic_topics()` and `bertopic_topic_terms()` return R-friendly topic metadata and terms/weights | Both convert upstream results to tibbles. | Full Windows suite and fresh-process smoke comparison. | Accurate. |
| `bertopic_find_topics()` returns IDs, scores, and labels | Wrapper augments upstream IDs/scores with `Name` from topic info. | `R/info.R`; exercised by Demo. | Accurate for models with an embedding backend. |
| `bertopic_get_document_info()` returns document metadata | Wrapper converts `get_document_info()` to a tibble and exposes failures. | `test-info.R`. | Accurate. |
| `bertopic_get_representative_docs()` returns ranked documents | Conversion handles character and structured backend results and validates topic IDs. | `test-representative-docs-regression.R`, `test-info.R`. | Add this principal extractor to the API table if space permits. |
| `predict()` and `bertopic_transform()` return assignments and optional strengths | `predict(type = "class"/"prob"/"both")` delegates to transform. | `test-exporters-predict.R`, roundtrip tests. | Accurate. |
| Document-topic export maps columns to actual fitted topic IDs | Dense and sparse paths derive labels from non-outlier topic IDs rather than positional assumptions. | `test-document-topic-id-regression.R`, visualization regression fixture. | Accurate after 0.1.1; cite the corrected release. |
| Topic modification is supported | `bertopic_update_topics()` mutates the retained Python model. `bertopic_reduce_topics()` returns an updated R wrapper with synchronized cached fields, so callers must assign its return value. | Reduction argument/state regression tests; corrected `demo.R`. | State the reassignment requirement if reduction is shown. |
| Custom labels use Python-compatible topic keys | Named vectors/data frames are converted to a Python dict with integer keys. | `test-topics-ops.R`. | Accurate after 0.1.1. |
| Visualization helpers return interactive HTML or write HTML files | Plotly figures are converted with `to_html`; no-file calls require `htmltools`. Degenerate hierarchical-document input has an explicit fallback. | `test-visualize.R`, `test-visualize-regression.R`. | Replace broad “all visualization” wording with the tested helper list if a completeness claim is intended. |
| Save/load restores a reusable R wrapper | Load restores `.py`, cached topics, and probabilities; file and directory paths can transform after loading in the tested environment. | `test-save-load.R`. | Accurate for matching trusted environments; retain the serialization/version warning. |
| The documented self-check performs a round trip | It now fits deterministic synthetic embeddings, transforms, saves, loads, and compares cached topics/probabilities, topic metadata, and post-load transform results. | `test-self-check.R`. | Accurate after 0.1.1. |
| S3 methods provide print, summary, predict, coef, data-frame, and fortify workflows | All six methods are registered; `fortify` uses delayed ggplot2 registration. | `test-s3.R`, `NAMESPACE`. | Accurate. |
| Conda and virtualenv setup are consistent | Both consume `inst/python/requirements.txt` and validate the same 11 import modules. | `test-install-spec.R`; clean `r-bertopic-016` installation. | Replace the version-0.1.0 installation text with 0.1.1 behavior. |
| Session diagnostics are availability-only | `bertopic_session_info()` now reports exact versions for 11 key distributions. | `test-session-info.R`, Windows session provenance. | Current manuscript statement is stale and must be rewritten. |
| Supported Python backend is BERTopic 0.16.0 | Both installers pin 0.16.0; the full testthat suite and `R CMD check` ran in a fresh Windows Python 3.10.21 environment with no failures or skips. | `provenance/windows-session-0.16.0.txt`. | Claim Windows support only from current evidence. Do not claim macOS/Linux validation. |
| Five fresh R/Python benchmark processes produced the published numbers | The new harness now creates separate workers, but the checked-in numeric results are the prior 0.17.4 single-process prototype. | `benchmark/README.md`, `results-legacy-0.17.4/`. | Replace every benchmark number only after the installed final release five-pair run. |

## Required manuscript edits after release and rerun

1. Change package-version references from 0.1.0 to the exact released 0.1.1 tag/commit and source hash.
2. Rewrite the installation paragraph around the shared exact requirements and the archived full Conda lock.
3. Rewrite the session-info paragraph because exact dependency versions are now reported.
4. Report the completed Windows validation only; remove or defer untested macOS/Linux claims.
5. Replace the old benchmark hashes, hardware/OS, counts, timings, memory values, tables, and figures from the final five-pair output.
6. Keep the benchmark claim bounded to the compared assignments, metadata, ordered terms/weights, and probability matrices.
