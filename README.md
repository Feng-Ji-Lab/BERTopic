# BERTopic (R interface)

R wrapper around the excellent Python library **BERTopic** for transformer-based topic modeling without leaving R.  
This package manages a separate Python environment, calls the Python API through ``reticulate``, and gives you R-friendly helpers for fitting, transforming, exporting, visualizing, and saving/loading models.  

Includes a tiny **SMS spam** demo dataset to get you started quickly.

---

## Table of contents

- [Installation](#installation)
  - [1) Install the R package](#1-install-the-r-package)
  - [2) Install Python deps (auto-selects Conda or virtualenv)](#2-install-python-deps-auto-selects-conda-or-virtualenv)
  - [3) Bind the session (auto-selects the right route)](#3-bind-the-session-auto-selects-the-right-route)
  - [Self check & session info](#self-check--session-info)
- [Quick start](#quick-start)
  - [Fit a model (demo data)](#fit-a-model-demo-data)
  - [Use your own data](#use-your-own-data)
- [Core workflow](#core-workflow)
  - [Get topic info & top terms](#get-topic-info--top-terms)
  - [Transform new documents](#transform-new-documents)
  - [Export document–topic matrix](#export-documenttopic-matrix)
  - [Label, update, reduce topics](#label-update-reduce-topics)
  - [Representative docs & nearest topics](#representative-docs--nearest-topics)
  - [Topics over time](#topics-over-time)
- [Visualization](#visualization)
- [Model I/O](#model-io)
- [Reproducibility](#reproducibility)
- [Troubleshooting](#troubleshooting)
- [API reference (R)](#api-reference-r)
- [Citation & data](#citation--data)


---

## Installation

### 1) Install the R package

If you’re installing from source (e.g., GitHub):

```r
# install.packages("remotes")
remotes::install_github("Feng-Ji-Lab/BERTopic")
library(BERTopic)
```

Our package relies on **reticulate**. You do **not** need a system-wide Python; we can set up an isolated one.

### 2) Install Python deps (auto-selects Conda or virtualenv)
``` r'
library(reticulate)
library(BERTopic)
install_py_deps(envname = "r-bertopic", python_version = "3.10")
```

### 3) Bind the session (auto-selects the right route)
``` r
use_bertopic("r-bertopic")
```

#### Self check & session info

```r
bertopic_available()          # TRUE if Python + bertopic importable
bertopic_session_info()       # versions & path
bertopic_self_check()  # runs a tiny round-trip fit/transform/save/load
```

---

## Quick start

### Fit a model (demo data)

We ship a small, cleaned subset of the UCI **SMS Spam** dataset:

```r
library(BERTopic)

data(sms_spam)
head(sms_spam)

m <- bertopic_fit(sms_spam$text, calculate_probabilities = TRUE)
print(m)
summary(m)

topics_tbl <- bertopic_topics(m)
topics_tbl
```

Result:

```
> topics_tbl
# A tibble: 48 × 5
   Topic Count Name                          Representation Representative_Docs
   <dbl> <dbl> <chr>                         <list>         <list>
 1    -1   784 -1_to_you_the_in              <chr [10]>     <chr [3]>
 2     0   121 0_love_my_life_you            <chr [10]>     <chr [3]>
 3     1   101 1_chat_text_stop_sexy         <chr [10]>     <chr [3]>
 4     2    84 2_tone_ringtone_tones_week    <chr [10]>     <chr [3]>
 5     3    64 3_bus_pub_car_at              <chr [10]>     <chr [3]>
 6     4    62 4_missing_message_please_call <chr [10]>     <chr [3]>
 7     5    52 5_class_exam_school_lesson    <chr [10]>     <chr [3]>
 8     6    45 6_he_gal_ask_dude             <chr [10]>     <chr [3]>
 9     7    45 7_won_claim_draw_prize        <chr [10]>     <chr [3]>
10     8    43 8_later_sorry_call_ill        <chr [10]>     <chr [3]>
```

### Use your own data

```r
df <- read.csv("data/sms_spam.csv", stringsAsFactors = FALSE, encoding = "UTF-8")
docs <- as.character(df$text)

m2 <- bertopic_fit(docs, calculate_probabilities = TRUE)
pred <- predict(m2, c("I like BERTopic"), type = "both")
pred$topics
pred$probs
```

With **precomputed embeddings** (rows = docs), pass a numeric matrix:

```r
emb <- matrix(rnorm(length(docs) * 384), length(docs), 384)
m_emb <- bertopic_fit(docs, embeddings = emb)
```

---

## Core workflow

### Get topic info & top terms

```r
info <- bertopic_topics(m)
head(info)

# Top terms per topic (weights)
coef_df <- coef(m, top_n = 8)
subset(coef_df, topic %in% head(info$Topic, 3))
```

### Transform new documents

```r
new_docs <- c("please confirm the meeting", "win a free trip now!!!")
tr <- bertopic_transform(m, new_docs)
tr$topics
tr$probs
```

### Export document–topic matrix

```r
M <- bertopic_as_document_topic_matrix(m, sparse = TRUE, prefix = TRUE)
M[1:5, 1:10]
```

> Requires `calculate_probabilities = TRUE` during fitting; otherwise returns `NULL` with a warning.

### Label, update, reduce topics

```r
# Update topic representations (after changing docs/preproc)
m <- bertopic_update_topics(m, sms_spam$text)

# Reduce/merge topics (target number or "auto")
m <- bertopic_reduce_topics(m, nr_topics = "auto", docs = sms_spam$text)

# Set custom labels (named character vector or data.frame)
labs <- c(`0` = "General ham", `1` = "Promotions/spam")
m <- bertopic_set_topic_labels(m, labs)
```

### Topics over time

```r
set.seed(1)
ts <- as.Date("2024-01-01") + seq_len(nrow(sms_spam)) %% 60
tot <- bertopic_topics_over_time(m, docs = sms_spam$text, timestamps = ts, nr_bins = 10)
head(tot)
```

---

## Visualization

These return an **`htmltools::HTML`** object by default, or write a self-contained HTML file if you pass `file =`.

```r
# Map of topics
bertopic_visualize_topics(m, file = "topics.html")

# Barchart (specific topic or default)
bertopic_visualize_barchart(m, topic_id = 0, file = "barchart_t0.html")

# Hierarchy / Heatmap / Term rank
bertopic_visualize_hierarchy(m, file = "hierarchy.html")
bertopic_visualize_heatmap(m,    file = "heatmap.html")
bertopic_visualize_term_rank(m,  file = "term_rank.html")

# Embedded documents (optionally pass a subset of docs)
bertopic_visualize_documents(m, docs = sms_spam$text[1:300], file = "docs.html")

# Topics over time
tot_py <- bertopic_topics_over_time(m, sms_spam$text, ts)
bertopic_visualize_topics_over_time(m, tot_py, top_n = 10, file = "topics_over_time.html")
```

---

## Model I/O

```r
# Save - choose one of: "pickle" (default), "safetensors", or "pt"
bertopic_save(
  m,
  path = "models/bert_model.pkl",
  serialization = "pickle",
  save_embedding_model = FALSE,
  overwrite = TRUE
)

# Load
m2 <- bertopic_load("models/bert_model.pkl")

# If you didn't save the embedding model, set one to enable `transform()`
bertopic_set_embedding_model(m2, "all-MiniLM-L6-v2")
```

---

## Reproducibility

```r
set_bertopic_seed(2025)   # seeds R, NumPy, and Python's random; also sets PYTHONHASHSEED
```

---

## Troubleshooting

- **`Python is not available`**  
  Run the installer first, then bind the session:
  ```r
  install_py_deps(); use_bertopic_virtualenv()
  ```
  Or, if you used conda:
  ```r
  use_bertopic_condaenv("r-bertopic")
  ```

- **`Python module 'bertopic' not found`**  
  You’re likely not pointing to the right env in this session. Call `use_bertopic_virtualenv()` or `use_bertopic_condaenv()` again (these are **per-session**).

- **Offline / first-run model download fails**  
  `sentence-transformers` may download model weights on first use. Connect to the internet once, or pre-download by running:
  ```r
  m <- bertopic_fit(c("a","b","c"))
  ```

- **Apple Silicon (M-series)**  
  Prefer the conda installer path; it auto-sets `CONDA_SUBDIR=osx-arm64`.

- **Headless/CI environments**  
  Visualizations return HTML strings; writing to file via `file =` is safest in headless/CI contexts.

- **Check everything**  
  ```r
  bertopic_available()
  bertopic_session_info()
  bertopic_self_check()
  ```

---

## API reference (R)

**Environment / diagnostics**
- `use_bertopic_virtualenv(env)`
- `use_bertopic_condaenv(envname, required = TRUE)`
- `bertopic_available()`
- `bertopic_session_info()`
- `bertopic_self_check(verbose = TRUE)`
- `set_bertopic_seed(seed)`

**Fit / transform / export**
- `bertopic_fit(text, embeddings = NULL, ...)`
- `predict.bertopic_r(object, newdata, type = c("both","class","prob"), embeddings = NULL, ...)`
- `bertopic_transform(model, new_text, embeddings = NULL)`
- `bertopic_as_document_topic_matrix(model, sparse = TRUE, prefix = TRUE)`

**Info / terms / search**
- `bertopic_topics(model)`
- `coef.bertopic_r(object, top_n = 10)`
- `bertopic_topic_terms(model, topic_id, top_n = 10)`
- `bertopic_get_document_info(model, docs)`
- `bertopic_find_topics(model, query_text, top_n = 5)`
- `bertopic_get_representative_docs(model, topic_id, top_n = 5)`

**Topic operations**
- `bertopic_update_topics(model, text)`
- `bertopic_reduce_topics(model, nr_topics = "auto", representation_model = NULL, docs = NULL)`
- `bertopic_set_topic_labels(model, labels)`

**Time & viz**
- `bertopic_topics_over_time(model, docs, timestamps, nr_bins = NULL, datetime_format = NULL)`
- `bertopic_visualize_topics(model, file = NULL)`
- `bertopic_visualize_barchart(model, topic_id = NULL, file = NULL)`
- `bertopic_visualize_hierarchy(model, file = NULL)`
- `bertopic_visualize_heatmap(model, file = NULL)`
- `bertopic_visualize_term_rank(model, file = NULL)`
- `bertopic_visualize_documents(model, docs = NULL, file = NULL)`
- `bertopic_visualize_topics_over_time(model, topics_over_time, top_n = 10, file = NULL)`

---

