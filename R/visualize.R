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
#' @export
bertopic_visualize_topics <- function(model, file = NULL) {
  if (!inherits(model, "bertopic_r")) rlang::abort("`model` must be a 'bertopic_r' object.")
  .need_py()
  fig <- try(model$.py$visualize_topics(), silent = TRUE)
  if (inherits(fig, "try-error")) rlang::abort("Python `visualize_topics()` failed.")
  .bertopic_fig_to_html(fig, file)
}

#' Visualize a topic barchart
#' @param model A "bertopic_r" model.
#' @param topic_id Integer topic id. If NULL, a set of top topics is shown.
#' @param file Optional HTML output path.
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
  if (inherits(fig, "try-error")) rlang::abort("Python `visualize_documents()` failed.")
  .bertopic_fig_to_html(fig, file)
}

#' Visualize topics over time
#'
#' @param model A "bertopic_r" model.
#' @param topics_over_time A tibble returned by [bertopic_topics_over_time()],
#'   or a Python dataframe compatible with `visualize_topics_over_time()`.
#' @param top_n Number of topics to display.
#' @param file Optional HTML output path.
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
