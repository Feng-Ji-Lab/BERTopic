# Windows fresh-process R-versus-Python benchmark

The benchmark uses one frozen float64, column-major reduced-embedding array for both interfaces. Matching the memory layout matters because HDBSCAN soft-membership calculations can differ when numerically identical arrays have different strides. Each repetition starts one new R process that calls the package and one new Python process that calls BERTopic directly. Odd repetitions run R first and even repetitions run Python first to balance persistent cache effects.

The timed model uses `BaseDimensionalityReduction()`, HDBSCAN with `min_cluster_size = 10` and `prediction_data = TRUE`, an English-stop-word `CountVectorizer`, and `calculate_probabilities = TRUE`. Component construction occurs before the fit timer. Cold-process time includes interpreter startup, imports, input loading, component construction, fitting, and result writing. Windows peak resident memory is read from `psutil`'s `peak_wset` for the complete worker process.

## 1. Prepare the frozen inputs once

Run this from the repository root with the validated Python environment:

```powershell
$python = "C:\Users\zby15\miniconda\envs\r-bertopic-016\python.exe"

& $python benchmark/prepare_inputs.py `
  --documents data/sms_spam.csv `
  --output benchmark/inputs/reduced_embeddings.npy `
  --max-docs 2247 `
  --seed 42 `
  --embedding-model all-MiniLM-L6-v2 `
  --model-revision 1110a243fdf4706b3f48f1d95db1a4f5529b4d41 `
  --reduced-dim 5
```

The preparer requires an immutable Hugging Face model revision, encodes the documents once, reduces the 384-dimensional embeddings once with UMAP, and writes the frozen `.npy` array in float64 column-major form. Its adjacent `.manifest.json` records the dtype, memory order, and full SHA-256 hashes of the source document file, the selected UTF-8 documents, and the array, together with its shape and preparation parameters. `--embedding-model synthetic` is available only for a quick harness smoke test; do not use it for manuscript results.

## 2. Run the exact R release

Build and install from a clean checkout of the release tag that will be cited, then return to the benchmark harness checkout. Retain the actual source archive; the runner records its SHA-256:

```powershell
R.exe CMD build .
R.exe CMD INSTALL BERTopic_0.1.2.tar.gz
```

Then run five paired fresh processes:

```powershell
Rscript benchmark/run_benchmark.R `
  --python $python `
  --documents data/sms_spam.csv `
  --embeddings benchmark/inputs/reduced_embeddings.npy `
  --output benchmark/results `
  --repetitions 5 `
  --max-docs 2247 `
  --seed 42 `
  --min-cluster-size 10 `
  --package-mode installed `
  --package-archive BERTopic_0.1.2.tar.gz `
  --release-tag v0.1.2 `
  --expected-package-version 0.1.2 `
  --expected-bertopic-version 0.16.0
```

Use `--package-mode source` only while developing the runner. The default `installed` mode and the two expected-version gates prevent a final run from silently using the wrong R package or Python backend. An existing non-empty output directory is rejected; pass `--overwrite true` only when replacement is intentional.

## Parameters

| Parameter | Default | Meaning |
| --- | ---: | --- |
| `--python` | required | Exact Python executable used by all workers. |
| `--documents` | `data/sms_spam.csv` | UTF-8 CSV containing the `text` column. |
| `--embeddings` | `benchmark/inputs/reduced_embeddings.npy` | Frozen reduced-embedding array. |
| `--output` | `benchmark/results` | Output directory. |
| `--repetitions` | `5` | Number of fresh R/Python process pairs. |
| `--max-docs` | `2247` | Number of documents and embedding rows used. |
| `--seed` | `42` | R, Python, NumPy, and process hash seed. |
| `--min-cluster-size` | `10` | HDBSCAN minimum cluster size. |
| `--package-mode` | `installed` | Load the installed release or the source tree. |
| `--expected-package-version` | `0.1.2` | Required R package version. |
| `--expected-bertopic-version` | `0.16.0` | Required Python backend version. |
| `--package-archive` | `BERTopic_0.1.2.tar.gz` in installed mode | Exact installed release source archive; SHA-256 is recorded. |
| `--release-tag` | `v0.1.2` | Immutable package release tag whose commit is recorded. |
| `--overwrite` | `false` | Replace an existing output directory. |

## Recorded outputs

- `runs.csv`: fit time, cold-process time, and peak RSS for every worker.
- `equivalence.csv`: per-pair comparisons of assignments, `Topic`/`Count`/`Name`, ordered term keys, c-TF-IDF weights at `1e-12`, probability dimensions, and probability values at `1e-12`.
- `summary.csv`: medians and interquartile ranges for fit, cold-process, and peak-RSS measurements and their paired differences.
- `manifest.txt`: source commit, expected versions, process settings, input manifest, and overall equivalence status.
- `provenance.json`: release/source and worker-script SHA-256 hashes, validated input manifest, full Python distribution versions, Windows/CPU/memory details, and inherited process settings.
- `runs/`: raw assignments, topic metadata, top terms and weights, probability matrices, exact environment metrics, and R session information from every worker.
- `logs/`: separate stdout and stderr logs from every fresh process.

The files under `results-legacy-0.17.4/` came from the earlier single-process prototype. They are retained as compatibility history and must not be used as the final D2 results.

## 3. Export tables, figures, and the worked example

After a successful paired run, regenerate the numerical report, LaTeX tables, and PNG/PDF plots from the raw outputs:

```powershell
Rscript benchmark/export_artifacts.R --results benchmark/results
```

The exporter refuses results whose equivalence checks failed. It writes its artifacts under `benchmark/results/artifacts`.

Run the complete manuscript worked example separately, after the timed benchmark has finished, with the same installed 0.1.2 package and Python 0.16.0 environment:

```powershell
Rscript benchmark/run_example.R `
  --python $python `
  --package-archive BERTopic_0.1.2.tar.gz `
  --output benchmark/example-results `
  --seed 42 `
  --max-docs 2247 `
  --model-revision 1110a243fdf4706b3f48f1d95db1a4f5529b4d41
```

This script requires the R packages `Matrix` and `jsonlite`. It uses the manuscript's complete embedding/reduction pipeline, while the controlled benchmark uses the frozen array and an English-stop-word vectorizer. Their topic counts need not match. It saves the full printed output, descriptive statistics, metadata, terms, query/prediction results, matrix exports, API-produced HTML, self-contained HTML, Plotly JSON, and static PNG/PDF figures from its own model.

The lightweight safetensors restoration check verifies topic metadata. The additional full pickle round trip verifies cached assignments/strengths and post-load predictions at `1e-12`. The script stops on failed operations or restoration checks. A manifest records the archive/script and generated artifact hashes. The large generated `model.pkl` is excluded from Git and can be recreated with this script.

The retained exact source archive is also available at provenance/releases/BERTopic_0.1.2.tar.gz; pass that path explicitly with --package-archive when the root archive is absent. Full topic metadata contain list columns; the example CSV serializer preserves these as JSON-valued cells. Its regression case can be run with:

~~~powershell
Rscript benchmark/test_example_io.R
~~~

The validated five-pair Windows 0.1.1 outputs are retained under results-windows-0.1.1/. Its full worked example exposed the lightweight-save bug fixed in 0.1.2; the final manuscript should use the new 0.1.2 rerun.

The final 0.1.2 run is retained in results/ and example-results/. Its synchronized manuscript source, bibliography, PDF and successful build logs are in manuscript-0.1.2/. The package check reports 159 passes, no failures/warnings/skips, and one existing UTF-8 data NOTE. Public release/DOI and E remain pending.

Validate the retained archive using Python 3.10+ without loading machine-learning packages: python benchmark/verify_archive.py. Hashes cover retained inputs, scripts, raw results, figures, manuscript and release provenance. Regenerating artifacts requires refreshing archive_sha256.json with --write after verifying their execution manifests.
