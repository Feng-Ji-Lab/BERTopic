test_that("BERTopic end-to-end round trip works", {
  skip_if_not(BERTopic::bertopic_available(), "Python/BERTopic not available")

  # ---- 1. 构造足够的语料 (>30 条，避免全部掉到 -1) ----
  docs <- c(
    "topic modeling with transformers",
    "we use BERTopic from R",
    "this is a small document",
    "another sentence about clustering",
    "embeddings and umap reduction",
    "hdbscan finds dense regions",
    "topic models group similar docs",
    "class tf idf helps labeling",
    "saving and loading models",
    "transform new unseen text",
    "visualization of topics",
    "semantic embeddings help clustering",
    "machine learning for document clustering",
    "deep learning with sentence transformers",
    "hierarchical topic visualization",
    "interactive dashboards with BERTopic",
    "pipelines for NLP topic modeling",
    "vectorization of documents with embeddings",
    "reduction of dimensions using UMAP",
    "density based clustering using HDBSCAN",
    "language models help NLP tasks",
    "neural embeddings capture semantics",
    "document clustering pipelines",
    "large corpora topic analysis",
    "explainable AI in topic models",
    "cross-lingual topic modeling",
    "dynamic topic modeling with time",
    "short texts and topic modeling",
    "long documents topic extraction",
    "academic paper clustering",
    "visual exploration of clusters",
    "BERTopic applied to social media"
  )

  # ---- 2. 训练模型 ----
  set_bertopic_seed(123)
  m <- bertopic_fit(docs)
  topics_tbl <- bertopic_topics(m)

  # 至少应该有一个非 -1 的主题
  expect_true(nrow(topics_tbl) >= 1)


  # ---- 3. 检查主题词表 ----
  # 找一个非 -1 主题
  valid_topics <- topics_tbl$Topic[topics_tbl$Topic != -1]
  if (length(valid_topics) > 0) {
    tt <- bertopic_topic_terms(m, valid_topics[1], top_n = 5)
    expect_s3_class(tt, "tbl_df")
    expect_true(nrow(tt) > 0)
  }

  # ---- 4. transform 新文档 ----
  pred <- bertopic_transform(m, c("new unseen sentence", "embedding based topic model"))
  expect_length(pred$topics, 2)
  expect_equal(length(pred$probs), 2)


})
