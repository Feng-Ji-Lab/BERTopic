# R-vs-Python benchmark

`run_benchmark.R` runs the package fitting path and the equivalent direct Python BERTopic path with the same documents, generated embeddings, UMAP configuration, and random seed. It compares document topic assignments, topic IDs, and probability matrices, and records fit times.

## Requirements

- Run from the BERTopic repository root.
- Install the R development dependencies, including `pkgload` and `reticulate`.
- Use a Python environment containing BERTopic and its dependencies.
- Pass the Python executable explicitly on Windows so reticulate cannot select a different environment.

## Command

```powershell
Rscript benchmark/run_benchmark.R `
  --python "C:\Users\zby15\miniconda\envs\r-bertopic\python.exe" `
  --output benchmark/results `
  --seed 42 `
  --embedding-dim 16 `
  --max-docs 2247
```

## Parameters

| Parameter | Default | Meaning |
| --- | ---: | --- |
| `--python` | `RETICULATE_PYTHON` or reticulate auto-selection | Python executable to use. An explicit path is recommended. |
| `--output` | `benchmark/results` | Directory for generated results. |
| `--seed` | `42` | R, Python, NumPy, and UMAP random seed. |
| `--embedding-dim` | `16` | Number of columns in the generated fixed embedding matrix. |
| `--max-docs` | `2247` | Maximum number of packaged SMS documents to use. |

## Outputs

- `summary.csv`: equality checks, matrix dimensions, and elapsed fit times.
- `environment.txt`: R, Python, NumPy, BERTopic, and module availability details.
- `raw.rds`: topic assignments, topic metadata, and probability matrices from both paths.

The checked-in Windows result was generated with R package 0.1.1, Python 3.10, and BERTopic 0.17.4. The package currently targets `bertopic==0.16.0`, so this run validates the benchmark script and this tested 0.17.4 path; the final D2 manuscript benchmark must be rerun in the frozen supported release environment.
