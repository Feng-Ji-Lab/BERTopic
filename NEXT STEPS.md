# BERTopic R Package: Development Progress

## Project Goal
Provide a stable R interface to BERTopic (originally Python)  
Final targets:
- Submit the package to CRAN.
- Publish an accompanying paper describing the implementation and applications.

---

## Completed Steps

### 1. Package Setup
- Created `BERTopic` R package structure.
- Integrated reticulate for Python interoperability.
- Added `DESCRIPTION`, `NAMESPACE`, and minimal R wrappers.

### 2. Core Functionality
- Implemented R functions wrapping main BERTopic API:
  - `bertopic_fit()`
  - `bertopic_topics()`
  - `bertopic_topic_terms()`
  - `bertopic_transform()`
  - `bertopic_save()`, `bertopic_load()`
- Added `bertopic_available()` and `bertopic_self_check()` for environment validation.

### 3. Python Environment Integration
- Isolated environment (`r-bertopic` conda env).
- Verified installation of torch, sentence-transformers, bertopic.
- Documented reproducible setup instructions.

### 4. Packaged EXAMPLE Dataset
- Added `sms_spam` dataset (subset of UCI SMS Spam Collection).
- Documented via `R/sms_spam.R`.
- Provides consistent example corpus for demos, tests, and vignettes.

### 5. Demonstration Scripts
- Implemented end-to-end demo:
  - Train model on `sms_spam`.
  - Show topic overview.
  - Extract top terms for a selected topic.
  - Predict topics for new texts with soft predictions (via `find_topics`).
- Created working testthat integration test.

---

## Pending Steps:

### Package Refinement for CRAN
- Dependencies: minimize Python dependency burden, provide clear fallback/error messages.
- Testing: 
  - Skip heavy tests on CRAN (`skip_on_cran()`).
  - Keep lightweight checks using small subsets of `sms_spam`.
- Documentation:
  - Full man pages for all exported functions.
  - Vignettes with use cases and explanations.
- CRAN Policies: 
  - Ensure no network calls during tests/examples.
  - Confirm package size < 5 MB (use subset dataset, skip large models).

### Advanced Features
- Add wrapper for `find_topics()` to `bertopic_predict()`.
- Improve save/load to handle both `pickle` and `safetensors` robustly (Currently not available).
- Consider visualization support (via reticulate → plotly/pyLDAvis).


---

## Next Steps
1. Finalize robust error handling and skip logic for CRAN.
2. Write user vignette showcasing end-to-end analysis.
3. CRAN submission checks.
4. Submit to CRAN and journal.

---
