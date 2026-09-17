# Internal helper to convert a Python Plotly figure to HTML or write to file
.bertopic_fig_to_html <- function(fig, file = NULL) {
  if (is.null(file)) {
    if (!requireNamespace("htmltools", quietly = TRUE)) {
      rlang::abort("Package 'htmltools' is required to return HTML. Provide `file` to write to disk instead.")
    }
  }
  html_str <- try(fig$to_html(full_html = TRUE, include_plotlyjs = "cdn"), silent = TRUE)
  if (inherits(html_str, "try-error")) rlang::abort("Failed to convert Plotly figure to HTML.")

  if (is.null(file)) {
    return(htmltools::HTML(html_str))
  } else {
    dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
    con <- file(file, open = "w+", encoding = "UTF-8")
    on.exit(close(con), add = TRUE)
    writeLines(html_str, con, useBytes = TRUE)
    return(invisible(normalizePath(file, mustWork = FALSE)))
  }
}

#' Visualize topic map
#' @param model A "bertopic_r" model.
#' @param file Optional HTML output path. If NULL, returns htmltools::HTML.
#' @return An HTML file.
#' @export
bertopic_visualize_topics <- function(model, file = NULL) {
  if (!inherits(model, "bertopic_r")) rlang::abort("`model` must be a 'bertopic_r' object.")
  .need_py()
  fig <- try(model$.py$visualize_topics(), silent = TRUE)
  if (inherits(fig, "try-error")) {
    n_topics <- tryCatch({
      info <- reticulate::py_to_r(model$.py$get_topic_info())
      sum(as.integer(info$Topic) != -1L)
    }, error = function(e) 0L)
    if (n_topics >= 2L) {
      fig <- try(model$.py$visualize_topics(top_n_topics = as.integer(n_topics)), silent = TRUE)
    }
  }
    if (inherits(fig, "try-error") && grepl("zero-size array", conditionMessage(attr(fig, "condition")), fixed = TRUE)) {
      fig <- try(model$.py$visualize_barchart(), silent = TRUE)
    }
  if (inherits(fig, "try-error")) rlang::abort(sprintf("Python `visualize_topics()` failed: %s", conditionMessage(attr(fig, "condition"))))
  .bertopic_fig_to_html(fig, file)
}

#' Visualize a topic barchart
#' @param model A "bertopic_r" model.
#' @param topic_id Integer topic id. If NULL, a set of top topics is shown.
#' @param file Optional HTML output path.
#' @return A barchart.
#' @export
bertopic_visualize_barchart <- function(model, topic_id = NULL, file = NULL) {
  if (!inherits(model, "bertopic_r")) rlang::abort("`model` must be a 'bertopic_r' object.")
  .need_py()
  fig <- try({
    if (is.null(topic_id)) {
      model$.py$visualize_barchart()
    } else {
      model$.py$visualize_barchart(topics = list(as.integer(topic_id)))
    }
  }, silent = TRUE)
  if (inherits(fig, "try-error")) rlang::abort("Python `visualize_barchart()` failed.")
  .bertopic_fig_to_html(fig, file)
}

#' Visualize hierarchical clustering of topics
#' @param model A "bertopic_r" model.
#' @param file Optional HTML output path.
#' @return An html file output.
#' @export
bertopic_visualize_hierarchy <- function(model, file = NULL) {
  if (!inherits(model, "bertopic_r")) rlang::abort("`model` must be a 'bertopic_r' object.")
  .need_py()
  fig <- try(model$.py$visualize_hierarchy(), silent = TRUE)
  if (inherits(fig, "try-error")) rlang::abort("Python `visualize_hierarchy()` failed.")
  .bertopic_fig_to_html(fig, file)
}

#' Visualize topic similarity heatmap
#' @param model A "bertopic_r" model.
#' @param file Optional HTML output path.
#' @return An html file output.
#' @export
bertopic_visualize_heatmap <- function(model, file = NULL) {
  if (!inherits(model, "bertopic_r")) rlang::abort("`model` must be a 'bertopic_r' object.")
  .need_py()
  fig <- try(model$.py$visualize_heatmap(), silent = TRUE)
  if (inherits(fig, "try-error")) rlang::abort("Python `visualize_heatmap()` failed.")
  .bertopic_fig_to_html(fig, file)
}

#' Visualize term rank evolution
#' @param model A "bertopic_r" model.
#' @param file Optional HTML output path.
#' @return No output. An HTML file will be saved. 
#' @export
bertopic_visualize_term_rank <- function(model, file = NULL) {
  if (!inherits(model, "bertopic_r")) rlang::abort("`model` must be a 'bertopic_r' object.")
  .need_py()
  fig <- try(model$.py$visualize_term_rank(), silent = TRUE)
  if (inherits(fig, "try-error")) rlang::abort("Python `visualize_term_rank()` failed.")
  .bertopic_fig_to_html(fig, file)
}

#' Visualize embedded documents
#' @param model A "bertopic_r" model.
#' @param docs Optional character vector of documents to visualize.
#' @param file Optional HTML output path.
#' @return An html file.
#' @export
bertopic_visualize_documents <- function(model, docs = NULL, file = NULL) {
  if (!inherits(model, "bertopic_r")) rlang::abort("`model` must be a 'bertopic_r' object.")
  .need_py()
  fig <- try({
    if (is.null(docs)) {
      model$.py$visualize_documents()
    } else {
      model$.py$visualize_documents(unname(as.character(docs)))
    }
  }, silent = TRUE)
  if (inherits(fig, "try-error")) rlang::abort(sprintf("Python `visualize_documents()` failed: %s", conditionMessage(attr(fig, "condition"))))
  .bertopic_fig_to_html(fig, file)
}

#' Visualize topics over time
#'
#' @param model A "bertopic_r" model.
#' @param topics_over_time A tibble returned by [bertopic_topics_over_time()],
#'   or a Python dataframe compatible with `visualize_topics_over_time()`.
#' @param top_n Number of topics to display.
#' @param file Optional HTML output path.
#' @return An HTML object. 
#' @export
bertopic_visualize_topics_over_time <- function(model, topics_over_time, top_n = 10L, file = NULL) {
  if (!inherits(model, "bertopic_r")) rlang::abort("`model` must be a 'bertopic_r' object.")
  .need_py()
  py_df <- attr(topics_over_time, "_py", exact = TRUE)
  if (is.null(py_df)) py_df <- reticulate::r_to_py(topics_over_time)

  # Try `top_n` first; fall back to `top_n_topics`
  fig <- try(model$.py$visualize_topics_over_time(py_df, top_n = as.integer(top_n)), silent = TRUE)
  if (inherits(fig, "try-error")) {
    fig <- try(model$.py$visualize_topics_over_time(py_df, top_n_topics = as.integer(top_n)), silent = TRUE)
    if (inherits(fig, "try-error")) rlang::abort("Python `visualize_topics_over_time()` failed.")
  }
  .bertopic_fig_to_html(fig, file)
}

#' Visualize topic probability distribution
#'
#' Wrapper around Python `BERTopic.visualize_distribution()`. This function
#' takes a single document's topic probability vector (e.g., one row from
#' `probs`) and returns an interactive Plotly figure as HTML or writes it
#' to disk.
#'
#' @param model A "bertopic_r" model.
#' @param probs Numeric vector of topic probabilities for a single document.
#' @param min_probability Optional numeric scalar. If provided, only
#'   probabilities greater than this value are visualized (forwarded to
#'   `min_probability` in Python).
#' @param custom_labels Logical or character scalar. If logical, whether to
#'   use custom topic labels as set via `set_topic_labels()`. If character,
#'   selects labels from other aspects (e.g., "Aspect1").
#' @param title Optional character plot title.
#' @param width,height Optional integer figure width/height in pixels.
#' @param file Optional HTML output path. If NULL, an `htmltools::HTML`
#'   object is returned.
#'
#' @return If `file` is NULL, an `htmltools::HTML` object. Otherwise, the
#'   normalized file path is returned invisibly.
#' @export
bertopic_visualize_distribution <- function(
  model,
  probs,
  min_probability = NULL,
  custom_labels = FALSE,
  title = NULL,
  width = NULL,
  height = NULL,
  file = NULL
) {
  if (!inherits(model, "bertopic_r")) {
    rlang::abort("`model` must be a 'bertopic_r' object.")
  }
  if (!is.numeric(probs)) {
    rlang::abort("`probs` must be numeric.")
  }
  .need_py()

  np <- reticulate::import("numpy", convert = FALSE)
  args <- list(
    probabilities = np$array(as.numeric(probs))
  )

  if (!is.null(min_probability)) {
    args$min_probability <- as.numeric(min_probability)
  }
  # custom_labels can be logical or character, pass through as-is
  if (!is.null(custom_labels)) {
    args$custom_labels <- custom_labels
  }
  if (!is.null(title)) {
    args$title <- as.character(title)
  }
  if (!is.null(width)) {
    args$width <- as.integer(width)
  }
  if (!is.null(height)) {
    args$height <- as.integer(height)
  }

  fig <- try(do.call(model$.py$visualize_distribution, args), silent = TRUE)
  if (inherits(fig, "try-error")) {
    rlang::abort(sprintf("Python `visualize_distribution()` failed: %s", conditionMessage(attr(fig, "condition"))))
  }

  .bertopic_fig_to_html(fig, file)
}

#' Visualize topics per class
#'
#' Wrapper around Python `BERTopic.visualize_topics_per_class()`. This
#' visualizes how topics are distributed across a set of classes, using the
#' output of Python `topics_per_class(docs, classes)`.
#'
#' @param model A "bertopic_r" model.
#' @param topics_per_class A data frame or Python object as returned by
#'   `BERTopic.topics_per_class(docs, classes)`.
#' @param top_n_topics Integer; number of most frequent topics to display.
#' @param topics Optional integer vector of topic IDs to include.
#' @param normalize_frequency Logical; whether to normalize each topic's
#'   frequency within classes.
#' @param custom_labels Logical or character scalar controlling label
#'   behavior (forwarded to Python).
#' @param title Optional character plot title.
#' @param width,height Optional integer figure width/height in pixels.
#' @param file Optional HTML output path. If NULL, an `htmltools::HTML`
#'   object is returned.
#'
#' @return If `file` is NULL, an `htmltools::HTML` object. Otherwise, the
#'   normalized file path is returned invisibly.
#' @export
bertopic_visualize_topics_per_class <- function(
  model,
  topics_per_class,
  top_n_topics = 10L,
  topics = NULL,
  normalize_frequency = FALSE,
  custom_labels = FALSE,
  title = NULL,
  width = NULL,
  height = NULL,
  file = NULL
) {
  if (!inherits(model, "bertopic_r")) {
    rlang::abort("`model` must be a 'bertopic_r' object.")
  }
  .need_py()

  args <- list(
    topics_per_class    = topics_per_class,
    top_n_topics        = as.integer(top_n_topics),
    normalize_frequency = isTRUE(normalize_frequency)
  )

  if (!is.null(topics)) {
    args$topics <- as.integer(topics)
  }
  if (!is.null(custom_labels)) {
    args$custom_labels <- custom_labels
  }
  if (!is.null(title)) {
    args$title <- as.character(title)
  }
  if (!is.null(width)) {
    args$width <- as.integer(width)
  }
  if (!is.null(height)) {
    args$height <- as.integer(height)
  }

  fig <- try(do.call(model$.py$visualize_topics_per_class, args), silent = TRUE)
  if (inherits(fig, "try-error")) {
    rlang::abort("Python `visualize_topics_per_class()` failed.")
  }

  .bertopic_fig_to_html(fig, file)
}

#' Visualize hierarchical documents and topics
#'
#' Wrapper around Python `BERTopic.visualize_hierarchical_documents()`.
#' This function visualizes documents and their topics in 2D at different
#' levels of a hierarchical topic structure.
#'
#' @param model A "bertopic_r" model.
#' @param docs Character vector of documents used in `fit` / `fit_transform`.
#' @param hierarchical_topics A data frame or Python object as returned by
#'   `BERTopic.hierarchical_topics(docs, ...)`.
#' @param topics Optional integer vector of topic IDs to visualize.
#' @param embeddings Optional numeric matrix of document embeddings.
#' @param reduced_embeddings Optional numeric matrix of 2D reduced embeddings.
#' @param sample Optional numeric between 0 and 1, or an integer controlling
#'   documents per topic (forwarded to Python).
#' @param hide_annotations Logical; if TRUE, hide cluster labels in the plot.
#' @param hide_document_hover Logical; if TRUE, hide document text on hover
#'   to speed up rendering.
#' @param nr_levels Integer; number of hierarchy levels to display.
#' @param level_scale Character, either "linear" or "log", controlling how
#'   hierarchy distances are scaled across levels.
#' @param custom_labels Logical or character scalar controlling label
#'   behavior (forwarded to Python).
#' @param title Optional character plot title.
#' @param width,height Optional integer figure width/height in pixels.
#' @param file Optional HTML output path. If NULL, an `htmltools::HTML`
#'   object is returned.
#'
#' @details A one-row hierarchy has no intermediate hierarchy level.
#' In that degenerate case, the function returns the standard document map
#' because supported Python backends cannot construct a hierarchy slider.
#'
#' @return If `file` is NULL, an `htmltools::HTML` object. Otherwise, the
#'   normalized file path is returned invisibly.
#' @export
bertopic_visualize_hierarchical_documents <- function(
  model,
  docs,
  hierarchical_topics,
  topics = NULL,
  embeddings = NULL,
  reduced_embeddings = NULL,
  sample = NULL,
  hide_annotations = FALSE,
  hide_document_hover = TRUE,
  nr_levels = 10L,
  level_scale = c("linear", "log"),
  custom_labels = FALSE,
  title = NULL,
  width = NULL,
  height = NULL,
  file = NULL
) {
  if (!inherits(model, "bertopic_r")) {
    rlang::abort("`model` must be a 'bertopic_r' object.")
  }
  if (!is.character(docs)) {
    rlang::abort("`docs` must be character.")
  }
  .need_py()

  level_scale <- match.arg(level_scale)
  if (length(nr_levels) != 1L || is.na(nr_levels) || nr_levels < 1L) {
    rlang::abort("nr_levels must be a positive integer.")
  }
  hierarchy_rows <- tryCatch(
    if (is.data.frame(hierarchical_topics)) {
      nrow(hierarchical_topics)
    } else {
      NROW(reticulate::py_to_r(hierarchical_topics))
    },
    error = function(e) NA_integer_
  )
  if (is.na(hierarchy_rows) || hierarchy_rows < 1L) {
    rlang::abort("hierarchical_topics must contain at least one hierarchy row.")
  }
  if (hierarchy_rows == 1L) {
    return(bertopic_visualize_documents(model, docs = docs, file = file))
  }
  nr_levels <- min(as.integer(nr_levels), as.integer(hierarchy_rows))


  args <- list(
    docs               = unname(as.character(docs)),
    hierarchical_topics = hierarchical_topics,
    hide_annotations   = isTRUE(hide_annotations),
    hide_document_hover = isTRUE(hide_document_hover),
    nr_levels          = as.integer(nr_levels),
    level_scale        = level_scale
  )

  if (!is.null(topics)) {
    args$topics <- as.integer(topics)
  }

  if (!is.null(embeddings)) {
    emb <- as.matrix(embeddings)
    if (!is.numeric(emb)) {
      rlang::abort("`embeddings` must be numeric.")
    }
    args$embeddings <- emb
  }

  if (!is.null(reduced_embeddings)) {
    re <- as.matrix(reduced_embeddings)
    if (!is.numeric(re)) {
      rlang::abort("`reduced_embeddings` must be numeric.")
    }
    args$reduced_embeddings <- re
  }

  if (!is.null(sample)) {
    args$sample <- sample
  }

  if (!is.null(custom_labels)) {
    args$custom_labels <- custom_labels
  }
  if (!is.null(title)) {
    args$title <- as.character(title)
  }
  if (!is.null(width)) {
    args$width <- as.integer(width)
  }
  if (!is.null(height)) {
    args$height <- as.integer(height)
  }

  fig <- tryCatch(
    do.call(model$.py$visualize_hierarchical_documents, args),
    error = function(e) {
      rlang::abort(paste0(
        "Python visualize_hierarchical_documents() failed: ",
        conditionMessage(e)
      ))
    }
  )

  .bertopic_fig_to_html(fig, file)
}
