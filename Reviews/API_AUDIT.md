# Package/API audit for manuscript revision

Scope: package 0.1.2, commit faee106360389a8e75199893f6e8c68c28876c8d, local tag v0.1.2, installed from the retained source archive. Validation covers Windows, Python 3.10.21, BERTopic 0.16.0. Five paired runs and the complete worked example have finished; macOS/Linux are outside the requested scope.

| Manuscript claim | Code-level finding | Evidence | Status / manuscript action |
| --- | --- | --- | --- |
| `bertopic_fit(text, embeddings = NULL, ...)` validates text, checks Python, forwards `...`, and calls `fit_transform()` | Matches `R/fit.R`; supplied embeddings are converted from a numeric R matrix. | Roundtrip and exporter/predict tests. | Accurate. |
| A `bertopic_r` object retains `.py`, `topics`, and optional `probs` | Constructor and loader both return these fields. | `test-bertopic-roundtrip.R`, `test-save-load.R`. | Accurate. |
| `bertopic_topics()` and `bertopic_topic_terms()` return R-friendly topic metadata and terms/weights | Both convert upstream results to tibbles. | Full Windows suite and fresh-process smoke comparison. | Accurate. |
| `bertopic_find_topics()` returns IDs, scores, and labels | Wrapper augments upstream IDs/scores with `Name` from topic info. | `R/info.R`; exercised by Demo. | Accurate for models with an embedding backend. |
| `bertopic_get_document_info()` returns document metadata | Wrapper converts `get_document_info()` to a tibble and exposes failures. | `test-info.R`. | Accurate. |
| `bertopic_get_representative_docs()` returns ranked documents | Conversion handles character and structured backend results and validates topic IDs. | `test-representative-docs-regression.R`, `test-info.R`. | Covered by retained worked-example outputs. |
| `predict()` and `bertopic_transform()` return assignments and optional strengths | `predict(type = "class"/"prob"/"both")` delegates to transform. | `test-exporters-predict.R`, roundtrip tests. | Accurate. |
| Document-topic export maps columns to actual fitted topic IDs | Dense and sparse paths derive labels from non-outlier topic IDs rather than positional assumptions. | `test-document-topic-id-regression.R`, visualization regression fixture. | Accurate after 0.1.1; cite the corrected release. |
| Topic modification is supported | `bertopic_update_topics()` mutates the retained Python model. `bertopic_reduce_topics()` returns an updated R wrapper with synchronized cached fields, so callers must assign its return value. | Reduction argument/state regression tests; corrected `demo.R`. | State the reassignment requirement if reduction is shown. |
| Custom labels use Python-compatible topic keys | Named vectors/data frames are converted to a Python dict with integer keys. | `test-topics-ops.R`. | Accurate after 0.1.1. |
| Visualization helpers return interactive HTML or write HTML files | Plotly figures are converted with `to_html`; no-file calls require `htmltools`. Degenerate hierarchical-document input has an explicit fallback. | `test-visualize.R`, `test-visualize-regression.R`. | Replace broad “all visualization” wording with the tested helper list if a completeness claim is intended. |
| Save/load restores a reusable R wrapper | Pickle retains the full backend, cached fields and inference. Lightweight formats retain metadata but omit the reducer/clusterer. Version 0.1.2 fixes pt mapping and the upstream no-reference warning path; save failures propagate without a pickle fallback. | test-save-load.R, test-save-formats-regression.R, example-results/restoration_checks.csv | Six worked-example restoration checks passed; manuscript distinguishes metadata from full inference restoration. |
| The documented self-check performs a round trip | It now fits deterministic synthetic embeddings, transforms, saves, loads, and compares cached topics/probabilities, topic metadata, and post-load transform results. | `test-self-check.R`. | Accurate after 0.1.1. |
| S3 methods provide print, summary, predict, coef, data-frame, and fortify workflows | All six methods are registered; `fortify` uses delayed ggplot2 registration. | `test-s3.R`, `NAMESPACE`. | Accurate. |
| Conda and virtualenv setup are consistent | Both consume `inst/python/requirements.txt` and validate the same 11 import modules. | `test-install-spec.R`; clean `r-bertopic-016` installation. | Manuscript updated to shared requirements and 0.1.2 behavior. |
| Session diagnostics are availability-only | `bertopic_session_info()` now reports exact versions for 11 key distributions. | `test-session-info.R`, Windows session provenance. | Manuscript now reports exact versions and the retained resolved environment. |
| Supported Python backend is BERTopic 0.16.0 | Both installers pin 0.16.0; the full testthat suite and `R CMD check` ran in a fresh Windows Python 3.10.21 environment with no failures or skips. | `provenance/windows-session-0.16.0.txt`. | Claim Windows support only from current evidence. Do not claim macOS/Linux validation. |
| Five fresh R/Python pairs produced manuscript numbers | All five pairs used the exact installed 0.1.2 release and canonical float64 column-major input. Assignments, metadata and ordered term keys matched exactly; weights/probabilities agreed within absolute tolerance 1e-12. | benchmark/results/, benchmark/results/artifacts/ | Manuscript numbers, tables, hardware/software and hashes are synchronized from this run. |

## Completed Windows evidence (2026-09-15)

- R CMD check of the exact 0.1.2 archive: 0 errors, 0 warnings, 1 existing UTF-8 SMS data NOTE. Actual testthat entrypoint: 159 passes, 0 failures, 0 warnings, 0 skips.
- All five independent R/Python pairs passed. Maximum absolute c-TF-IDF difference: 4.9960036108132e-16; probability difference: 4.85722573273506e-16. These are tolerance comparisons, not bitwise equality.
- Paired R-minus-Python median differences: fitting 4.183 seconds, cold-process time 11.400 seconds, peak memory 200.0 MiB. Embedding generation and UMAP fitting are excluded.
- Full worked example, dense/sparse export comparison, interactive/static figures and six restoration checks completed.
- Manuscript synchronized from recorded outputs; repeated synchronization is idempotent. Full LaTeX/Biber build succeeded (26 pages), with no final undefined references or overfull boxes. Numerical table pages and runtime plot were visually checked.
- Source archive, resolved Windows environment, hashes, seeds, raw worker output, figures/tables and the exact manuscript snapshot are retained locally.

## Remaining items

D1 has a validated local tag/archive; public release publication remains pending. D3 local materials are retained, but the public archive/DOI is F1. E is deferred at the user's request; historical SMS selection is not established or changed. F2/F3 require author-provided affiliations, declarations and contribution facts. This audit does not claim those items complete.
