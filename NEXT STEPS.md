# BERTopic R Package: Development Progress

## Project Goal
Provide a stable R interface to BERTopic (originally Python)  
Final targets:
- Submit the package to CRAN.
- Publish an accompanying paper describing the implementation and applications.


## Current status
Feature coverage: approximately 85–90% of common BERTopic workflows are implemented with tests.  
CRAN posture: local checks are clean; optional dependencies are declared; tests avoid network and heavy workloads.

---

## Completed work

### Package and infrastructure
- Package skeleton with roxygen2, testthat (3e), pkgdown config
- DESCRIPTION/NAMESPACE aligned; optional deps in Suggests (Matrix, htmltools, testthat)

### Python environment integration
- Installers and binders (virtualenv and conda):
  - install_py_deps()
  - use_bertopic_virtualenv(), use_bertopic_condaenv(), get_py_env()
- Diagnostics and reporting:
  - bertopic_available(), bertopic_session_info(), bertopic_self_check()
- Reproducible environment pinning for torch, sentence-transformers, umap-learn, hdbscan, scikit-learn

### Core modeling API
- bertopic_fit(text, embeddings = NULL, ...)
- bertopic_transform(model, new_text, embeddings = NULL)
- predict.bertopic_r(type = c("both", "class", "prob"))

### Model introspection and explainability
- bertopic_topics()
- bertopic_topic_terms(model, topic_id, top_n)
- bertopic_get_document_info(model, docs)  (explicit docs for robustness)
- bertopic_find_topics(model, query_text, top_n)
- bertopic_get_representative_docs(model, topic_id, top_n)  (multi-signature fallback)

### Topic operations
- bertopic_update_topics(model, text)
- bertopic_reduce_topics(model, nr_topics = "auto", docs = NULL, ...)  (signature fallbacks)
- bertopic_set_topic_labels(model, labels)  (tries vectors, Python dict with integer keys, and named list; verifies application via Name)

### Time dimension
- bertopic_topics_over_time(model, docs, timestamps, nr_bins = NULL, datetime_format = NULL)
- bertopic_visualize_topics_over_time(model, topics_over_time, top_n, file = NULL)  (supports top_n and top_n_topics variants)

### Visualization (Plotly in Python → HTML)
- bertopic_visualize_topics()
- bertopic_visualize_barchart(topic_id = NULL)
- bertopic_visualize_hierarchy()
- bertopic_visualize_heatmap()
- bertopic_visualize_term_rank()
- bertopic_visualize_documents(docs = NULL)
- Shared helper for safe figure → HTML conversion with file or htmltools::HTML; gracefully skipped if Python plotly is missing

### Persistence
- bertopic_save(model, path, serialization = c("pickle", "safetensors", "pt"), save_embedding_model = FALSE, overwrite = FALSE)
  - only ensures parent directory; does not pre-create the target path
  - Windows-safe; named/positional fallbacks
- bertopic_load(path)

### Embedding management
- bertopic_has_embedding_model(model)  (no reliance on non-exported reticulate helpers)
- bertopic_set_embedding_model(model, embedding_model)  (object or model id; signature fallbacks)

### Exporters and R ecosystem
- bertopic_as_document_topic_matrix(model, sparse = TRUE, prefix = TRUE)
- summary.bertopic_r(), coef.bertopic_r(top_n), as.data.frame.bertopic_r(), fortify.bertopic_r()

### Data and demo
- Packaged dataset: sms_spam (UCI SMS Spam subset)
- Demo: end-to-end train → topics/terms → find/transform → save/load → soft predict

### Testing and QA
- Broad testthat suite using sms_spam
- skip_on_cran() and backend-aware skips for heavy or optional features
- Coverage includes fit/transform, save/load (file and dir), topic terms, find_topics, document info, topic ops (update/reduce/labels), topics over time and visualizations, exporters and S3 methods, embedding presence/reset
- Tests avoid network and keep runtime bounded


---

## Remaining for full feature parity and CRAN

### Features
1. Per-class analysis and c-TF-IDF
   - bertopic_topics_per_class(model, docs, classes, ...)
   - bertopic_c_tf_idf(model, docs, topics, ...)
   - tests: non-empty outputs; loose monotonicity checks; mark heavy paths as skippable

2. Outlier reduction
   - bertopic_reduce_outliers(model, docs = NULL, strategy = c("c-tf-idf", "distributions"), ...)
   - tests: call succeeds; noise proportion does not increase (soft assertion)

3. Manual merge of topics
   - bertopic_merge_topics(model, topics_to_merge, new_label = NULL, ...)
   - tests: topic count decreases; label applied if provided

4. Direct representations editing
   - bertopic_set_topic_representations(model, topic_id, terms, weights = NULL)
   - tests: get_topic_info() or get_topic() reflects changes

5. Field normalization
   - normalize bertopic_topics() output to ensure a Name column across backend variants

6. Optional visualizations
   - wrappers for additional plots if available in backend (for example, probability distribution)

### Documentation and examples
- Vignettes
  - Getting started: install → fit → topics/terms → predict → save/load → visualize
  - Temporal and per-class analyses: topics over time, topics per class, c-TF-IDF
  - Troubleshooting: Python env, embedding downloads, Windows paths, transform-after-load, plotly missing
- README with quickstart and environment setup (conda/virtualenv)
- NEWS.md, cran-comments.md, inst/CITATION

