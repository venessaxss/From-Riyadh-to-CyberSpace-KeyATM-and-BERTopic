#!/usr/bin/env Rscript

# ================================================================
# Guided keyATM of Saudi Diplomatic Corpus - Only Exact Seed Words, with Visuals and Output Tables
#
# Usage:
#   Input: all.csv; output_dir: 
#
# Required column:
#   content
#
# Optional columns:
#   title, date, address, Categories
# ================================================================

args <- commandArgs(trailingOnly = TRUE)
DATA_PATH <- if (length(args) >= 1) args[1] else "saudi_diplomatic_corpus.csv"
OUTPUT_DIR <- if (length(args) >= 2) args[2] else "keyatm_output"

SEED <- 1234L
KEYATM_NO_KEYWORD_TOPICS <- 3L
KEYATM_ITERATIONS <- 1000L
KEYATM_MIN_TERMFREQ <- 3L
KEYATM_MIN_DOCFREQ <- 2L
KEYATM_TOP_WORDS_N <- 15L
KEYATM_ALPHA_PLOT_START <- 0L
KEYATM_MODEL_FIT_START <- 1L
RUN_PER_CATEGORY <- TRUE
MIN_DOCS_PER_CATEGORY_KEYATM <- 20L
CATEGORY_COL <- "Categories"
INSTALL_MISSING_R_PACKAGES <- TRUE

required_r_packages <- c(
  "readr", "dplyr", "stringr", "purrr", "tibble", "tidyr",
  "quanteda", "keyATM", "stopwords", "ggplot2"
)

missing_r <- required_r_packages[
  !vapply(required_r_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_r) > 0) {
  if (!INSTALL_MISSING_R_PACKAGES) {
    stop(
      "Missing R packages: ", paste(missing_r, collapse = ", "),
      "\nSet INSTALL_MISSING_R_PACKAGES <- TRUE or install them manually."
    )
  }
  install.packages(missing_r, repos = "https://cloud.r-project.org")
}

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(purrr)
  library(tibble)
  library(tidyr)
  library(quanteda)
  library(keyATM)
  library(ggplot2)
})

seed_topics <- list(
  "Economic Cooperation" = c(
    "trade", "investment", "economic", "partnership", "oil", "infrastructure"
  ),
  "Belt and Road Initiative (BRI)" = c(
    "BRI", "infrastructure", "connectivity", "China", "Saudi Arabia", "Silk Road"
  ),
  "Diplomatic Relations" = c(
    "oil", "energy", "Saudi Arabia", "China", "production", "supply"
  ),
  "Technology and Innovation" = c(
    "technology", "innovation", "collaboration", "digital", "AI", "China"
  ),
  "Cultural Exchange" = c(
    "culture", "exchange", "tourism", "education", "Saudi Arabia", "China"
  ),
  "Geopolitical Strategy" = c(
    "geopolitics", "strategy", "Middle East", "alliance", "security", "influence"
  ),
  "Trade Agreements" = c(
    "agreements", "trade", "partnerships", "deals", "exports", "imports"
  ),
  "Infrastructure Development" = c(
    "infrastructure", "construction", "projects", "cities", "development", "partnership"
  )
)

seed_topics_keyatm <- lapply(seed_topics, function(words) {
  words |>
    tolower() |>
    stringr::str_squish() |>
    stringr::str_replace_all("\\s+", "_")
})

normalize_spaces <- function(x) {
  x <- as.character(x)
  x <- stringr::str_replace_all(x, "[\r\n\t]+", " ")
  stringr::str_squish(x)
}

clean_html_text <- function(x) {
  x <- as.character(x)
  x <- stringr::str_replace_all(x, "<[^>]+>", " ")
  x <- stringr::str_replace_all(x, "&[A-Za-z0-9#]+;", " ")
  normalize_spaces(x)
}

safe_make_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

safe_slug <- function(x) {
  x <- ifelse(is.na(x) | x == "", "missing", x)
  x |>
    stringr::str_replace_all("[^A-Za-z0-9]+", "_") |>
    stringr::str_replace_all("_+", "_") |>
    stringr::str_replace_all("^_|_$", "") |>
    tolower()
}

safe_write_csv <- function(x, path) {
  tryCatch(readr::write_csv(x, path), error = function(e) NULL)
}

safe_save_keyatm_fig <- function(fig, path_png, width = 11, height = 7, dpi = 300) {
  tryCatch({
    keyATM::save_fig(fig, filename = path_png, width = width, height = height, dpi = dpi)
    TRUE
  }, error = function(e) FALSE)
}

safe_values_fig <- function(fig) {
  tryCatch(keyATM::values_fig(fig), error = function(e) NULL)
}

first_existing_col <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) return(NA_character_)
  hit[[1]]
}

extract_last_series <- function(df, group_col = "Topic") {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)

  iter_col <- first_existing_col(df, c("Iteration", "iteration", "Iter", "iter", "Slice", "slice"))
  if (is.na(iter_col)) {
    df$.__rowid__ <- seq_len(nrow(df))
    iter_col <- ".__rowid__"
  }

  numeric_cols <- names(df)[vapply(df, is.numeric, logical(1))]
  numeric_cols <- setdiff(numeric_cols, iter_col)
  if (length(numeric_cols) == 0) return(NULL)

  value_col <- numeric_cols[[1]]
  if (group_col %in% names(df)) {
    df %>%
      group_by(.data[[group_col]]) %>%
      arrange(.data[[iter_col]], .by_group = TRUE) %>%
      slice_tail(n = 1) %>%
      ungroup() %>%
      transmute(
        Topic = .data[[group_col]],
        last_iteration = .data[[iter_col]],
        estimate = .data[[value_col]]
      )
  } else {
    df %>%
      arrange(.data[[iter_col]]) %>%
      slice_tail(n = 1) %>%
      transmute(
        Topic = "all",
        last_iteration = .data[[iter_col]],
        estimate = .data[[value_col]]
      )
  }
}

get_phi_matrix <- function(fit, topic_names) {
  phi <- fit$phi
  if (is.null(phi)) return(NULL)

  if (is.matrix(phi)) {
    rownames(phi) <- rownames(phi) %||% topic_names[seq_len(nrow(phi))]
    colnames(phi) <- colnames(phi) %||% fit$vocab
    return(phi)
  }

  if (is.list(phi)) {
    if (is.null(names(phi)) && length(phi) == length(topic_names)) names(phi) <- topic_names
    mat <- do.call(rbind, lapply(phi, function(x) as.numeric(unlist(x))))
    rownames(mat) <- names(phi)
    colnames(mat) <- fit$vocab
    return(mat)
  }

  NULL
}

`%||%` <- function(x, y) if (is.null(x)) y else x

extract_doc_topic_table <- function(fit, topic_names, model_df) {
  theta <- fit$theta
  if (is.null(theta)) return(NULL)

  theta_mat <- as.matrix(theta)
  colnames(theta_mat) <- colnames(theta_mat) %||% topic_names[seq_len(ncol(theta_mat))]

  theta_tbl <- as_tibble(theta_mat)
  names(theta_tbl) <- paste0("topic_", make.names(names(theta_tbl), unique = TRUE))

  bind_cols(
    model_df %>% select(doc_id, title, date, address, all_of(CATEGORY_COL), content),
    theta_tbl
  )
}

extract_seed_word_probabilities <- function(fit, seed_topics_keyatm, topic_names) {
  phi_mat <- get_phi_matrix(fit, topic_names)
  if (is.null(phi_mat)) return(NULL)

  vocab_terms <- colnames(phi_mat)
  if (is.null(vocab_terms) || length(vocab_terms) == 0) return(NULL)

  rows <- list()
  for (topic in names(seed_topics_keyatm)) {
    if (!topic %in% rownames(phi_mat)) next

    probs <- as.numeric(phi_mat[topic, ])
    names(probs) <- vocab_terms

    ord <- order(probs, decreasing = TRUE, na.last = TRUE)
    ranks <- integer(length(probs))
    ranks[ord] <- seq_along(ord)
    names(ranks) <- vocab_terms

    for (seed_word in seed_topics_keyatm[[topic]]) {
      idx <- match(seed_word, vocab_terms)
      present <- !is.na(idx)

      rows[[length(rows) + 1]] <- tibble(
        Topic = topic,
        seed_word = stringr::str_replace_all(seed_word, "_", " "),
        vocab_term = seed_word,
        present_in_vocab = present,
        probability = if (present) unname(probs[idx]) else NA_real_,
        rank_in_topic = if (present) unname(ranks[idx]) else NA_integer_
      )
    }
  }

  bind_rows(rows) %>% arrange(Topic, desc(probability), rank_in_topic)
}

extract_keyword_prominence_table <- function(fit, seed_topics_keyatm, topic_names) {
  phi_mat <- get_phi_matrix(fit, topic_names)
  if (is.null(phi_mat)) return(NULL)

  rows <- list()
  for (topic in names(seed_topics_keyatm)) {
    if (!topic %in% rownames(phi_mat)) next
    probs <- as.numeric(phi_mat[topic, ])
    names(probs) <- colnames(phi_mat)

    for (seed_word in seed_topics_keyatm[[topic]]) {
      idx <- match(seed_word, names(probs))
      if (is.na(idx)) next
      rows[[length(rows) + 1]] <- tibble(
        Topic = topic,
        keyword = stringr::str_replace_all(seed_word, "_", " "),
        term_in_vocab = seed_word,
        topic_probability = unname(probs[idx]),
        relative_prominence = unname(probs[idx] / sum(probs, na.rm = TRUE))
      )
    }
  }

  bind_rows(rows) %>% arrange(Topic, desc(topic_probability))
}

save_keyatm_plot_bundle <- function(fig, out_dir, stem, summary_name = NULL) {
  png_path <- file.path(out_dir, paste0(stem, ".png"))
  safe_save_keyatm_fig(fig, png_path)

  vals <- safe_values_fig(fig)
  if (!is.null(vals)) {
    safe_write_csv(as_tibble(vals), file.path(out_dir, paste0(stem, "_values.csv")))
    if (!is.null(summary_name)) {
      last_vals <- extract_last_series(as_tibble(vals))
      if (!is.null(last_vals)) {
        safe_write_csv(last_vals, file.path(out_dir, paste0(summary_name, ".csv")))
      }
    }
  }
}

prepare_keyatm_input <- function(df_subset) {
  df_pre <- df_subset %>%
    mutate(
      text_for_model = clean_html_text(content),
      title = normalize_spaces(title),
      date = as.character(date),
      address = as.character(address),
      Categories = as.character(.data[[CATEGORY_COL]])
    ) %>%
    filter(!is.na(text_for_model), text_for_model != "") %>%
    mutate(doc_id = row_number())

  toks <- quanteda::tokens(
    df_pre$text_for_model,
    remove_punct = TRUE,
    remove_symbols = TRUE,
    remove_numbers = TRUE,
    remove_url = TRUE
  ) %>%
    quanteda::tokens_tolower() %>%
    quanteda::tokens_remove(pattern = stopwords::stopwords("en")) %>%
    quanteda::tokens_wordstem(language = "english")

  toks <- quanteda::tokens_compound(
    toks,
    pattern = phrase(list(
      c("saudi", "arabia"),
      c("middle", "east"),
      c("silk", "road"),
      c("belt", "road")
    )),
    concatenator = "_"
  )

  dfm_mat <- quanteda::dfm(toks) %>%
    quanteda::dfm_trim(
      min_termfreq = KEYATM_MIN_TERMFREQ,
      min_docfreq = KEYATM_MIN_DOCFREQ,
      docfreq_type = "count"
    )

  keep_docs <- quanteda::ntoken(dfm_mat) > 0
  dfm_mat <- dfm_mat[keep_docs, ]
  df_pre <- df_pre[keep_docs, , drop = FALSE]

  keyatm_docs <- keyATM::keyATM_read(texts = dfm_mat)

  list(
    model_df = df_pre,
    dfm = dfm_mat,
    keyatm_docs = keyatm_docs
  )
}

run_keyatm_pipeline <- function(df_subset, subset_label, out_dir) {
  message("Running keyATM for subset: ", subset_label)
  safe_make_dir(out_dir)

  prepared <- prepare_keyatm_input(df_subset)
  model_df <- prepared$model_df
  dfm_mat <- prepared$dfm
  keyatm_docs <- prepared$keyatm_docs

  if (nrow(model_df) == 0 || ndoc(dfm_mat) == 0 || nfeat(dfm_mat) == 0) {
    warning("No usable documents left for keyATM in subset: ", subset_label)
    return(NULL)
  }

  set.seed(SEED)
  fit <- keyATM::keyATM(
    docs = keyatm_docs,
    no_keyword_topics = KEYATM_NO_KEYWORD_TOPICS,
    keywords = seed_topics_keyatm,
    model = "base",
    options = list(
      seed = SEED,
      iterations = KEYATM_ITERATIONS,
      verbose = TRUE
    )
  )

  other_topic_names <- if (KEYATM_NO_KEYWORD_TOPICS > 0) {
    paste0("Other_", seq_len(KEYATM_NO_KEYWORD_TOPICS))
  } else character(0)
  topic_names <- c(names(seed_topics_keyatm), other_topic_names)

  top_words_tbl <- keyATM::top_words(fit, n = KEYATM_TOP_WORDS_N)
  if (is.matrix(top_words_tbl)) top_words_tbl <- as.data.frame(top_words_tbl)
  safe_write_csv(as_tibble(top_words_tbl, rownames = "Topic"), file.path(out_dir, "top_words.csv"))

  doc_topic_tbl <- extract_doc_topic_table(fit, topic_names, model_df)
  if (!is.null(doc_topic_tbl)) {
    safe_write_csv(doc_topic_tbl, file.path(out_dir, "document_topic_distribution.csv"))

    if (CATEGORY_COL %in% names(doc_topic_tbl)) {
      topic_cols <- grep("^topic_", names(doc_topic_tbl), value = TRUE)
      if (length(topic_cols) > 0) {
        category_topic_avg <- doc_topic_tbl %>%
          filter(!is.na(.data[[CATEGORY_COL]]), .data[[CATEGORY_COL]] != "") %>%
          group_by(.data[[CATEGORY_COL]]) %>%
          summarise(across(all_of(topic_cols), mean, na.rm = TRUE), .groups = "drop")
        safe_write_csv(category_topic_avg, file.path(out_dir, "category_topic_average.csv"))
      }
    }
  }

  seed_prob_tbl <- extract_seed_word_probabilities(fit, seed_topics_keyatm, topic_names)
  if (!is.null(seed_prob_tbl)) {
    safe_write_csv(seed_prob_tbl, file.path(out_dir, "seed_word_probabilities.csv"))
  }

  keyword_prom_tbl <- extract_keyword_prominence_table(fit, seed_topics_keyatm, topic_names)
  if (!is.null(keyword_prom_tbl)) {
    safe_write_csv(keyword_prom_tbl, file.path(out_dir, "keyword_prominence.csv"))
  }

  fig_topicprop <- tryCatch(
    keyATM::plot_topicprop(
      fit,
      n = 3,
      show_topwords = TRUE,
      order = "proportion"
    ),
    error = function(e) NULL
  )
  if (!is.null(fig_topicprop)) {
    save_keyatm_plot_bundle(fig_topicprop, out_dir, "keyatm_topic_proportions")
  }

  fig_keyword <- tryCatch(
    keyATM::visualize_keywords(fit),
    error = function(e) NULL
  )
  if (!is.null(fig_keyword)) {
    save_keyatm_plot_bundle(fig_keyword, out_dir, "keyatm_keyword_prominence")
  }

  fig_modelfit <- tryCatch(
    keyATM::plot_modelfit(fit, start = KEYATM_MODEL_FIT_START),
    error = function(e) NULL
  )
  if (!is.null(fig_modelfit)) {
    save_keyatm_plot_bundle(fig_modelfit, out_dir, "keyatm_loglikelihood_perplexity")
  }

  fig_alpha <- tryCatch(
    keyATM::plot_alpha(fit, start = KEYATM_ALPHA_PLOT_START),
    error = function(e) NULL
  )
  if (!is.null(fig_alpha)) {
    save_keyatm_plot_bundle(fig_alpha, out_dir, "keyatm_alpha", summary_name = "estimated_alpha")
  }

  fig_pi <- tryCatch(
    keyATM::plot_pi(fit, start = KEYATM_ALPHA_PLOT_START),
    error = function(e) NULL
  )
  if (!is.null(fig_pi)) {
    save_keyatm_plot_bundle(fig_pi, out_dir, "keyatm_pi", summary_name = "estimated_pi")
  }

  metadata_tbl <- tibble(
    subset = subset_label,
    n_input_docs = nrow(df_subset),
    n_model_docs = nrow(model_df),
    n_vocab = length(fit$vocab),
    n_topics_total = length(topic_names),
    n_guided_topics = length(seed_topics_keyatm),
    n_other_topics = KEYATM_NO_KEYWORD_TOPICS
  )
  safe_write_csv(metadata_tbl, file.path(out_dir, "subset_summary.csv"))

  list(fit = fit, model_df = model_df, topic_names = topic_names, dfm = dfm_mat, keyatm_docs = keyatm_docs)
}

if (!file.exists(DATA_PATH)) {
  stop(
    "Data file not found: ", DATA_PATH,
    "\nPut your CSV in the working directory or pass the full path as the first argument."
  )
}

safe_make_dir(OUTPUT_DIR)
raw_df <- readr::read_csv(DATA_PATH, show_col_types = FALSE)

if (!"content" %in% names(raw_df)) {
  stop("Your CSV must contain a 'content' column.")
}
if (!"title" %in% names(raw_df)) raw_df$title <- ""
if (!"date" %in% names(raw_df)) raw_df$date <- NA_character_
if (!"address" %in% names(raw_df)) raw_df$address <- NA_character_
if (!CATEGORY_COL %in% names(raw_df)) raw_df[[CATEGORY_COL]] <- NA_character_

raw_df <- raw_df %>%
  mutate(
    title = as.character(title),
    date = as.character(date),
    address = as.character(address),
    content = as.character(content),
    Categories = as.character(.data[[CATEGORY_COL]])
  )

safe_write_csv(raw_df, file.path(OUTPUT_DIR, "input_corpus_snapshot.csv"))

whole_dir <- file.path(OUTPUT_DIR, "whole_corpus")
safe_make_dir(whole_dir)
safe_write_csv(raw_df, file.path(whole_dir, "input_documents_used.csv"))
run_keyatm_pipeline(raw_df, "whole corpus", whole_dir)

if (RUN_PER_CATEGORY && CATEGORY_COL %in% names(raw_df)) {
  category_values <- raw_df %>%
    filter(!is.na(.data[[CATEGORY_COL]]), .data[[CATEGORY_COL]] != "") %>%
    distinct(.data[[CATEGORY_COL]]) %>%
    pull(.data[[CATEGORY_COL]])

  if (length(category_values) > 0) {
    categories_root <- file.path(OUTPUT_DIR, "each_category")
    safe_make_dir(categories_root)

    for (cat_value in category_values) {
      cat_df <- raw_df %>% filter(.data[[CATEGORY_COL]] == cat_value)
      if (nrow(cat_df) < MIN_DOCS_PER_CATEGORY_KEYATM) next

      cat_dir <- file.path(categories_root, safe_slug(cat_value))
      safe_make_dir(cat_dir)
      safe_write_csv(cat_df, file.path(cat_dir, "input_documents_used.csv"))
      try(run_keyatm_pipeline(cat_df, paste0("category: ", cat_value), cat_dir), silent = FALSE)
    }
  }
}

message("Done. keyATM outputs saved to: ", normalizePath(OUTPUT_DIR, winslash = "/", mustWork = FALSE))
