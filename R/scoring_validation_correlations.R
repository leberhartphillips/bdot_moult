suppressPackageStartupMessages(library(readxl))

find_workbook <- function(filename = "Scoring - test.xlsx") {
  candidates <- c(
    file.path("data", filename),
    file.path("..", "data", filename)
  )

  existing <- candidates[file.exists(candidates)]

  if (length(existing) == 0) {
    stop(
      "Could not find the workbook. Checked: ",
      paste(shQuote(candidates), collapse = ", "),
      call. = FALSE
    )
  }

  normalizePath(existing[[1]], winslash = "/")
}

parse_observer_scores <- function(x) {
  x <- trimws(as.character(x))

  parsed <- vapply(
    x,
    function(value) {
      if (is.na(value) || value == "") {
        return(NA_real_)
      }

      if (!grepl("-", value, fixed = TRUE)) {
        return(as.numeric(value))
      }

      bounds <- as.numeric(strsplit(value, "-", fixed = TRUE)[[1]])
      mean(bounds)
    },
    numeric(1)
  )

  unparsable <- unique(x[is.na(parsed) & !is.na(x) & nzchar(x)])

  if (length(unparsable) > 0) {
    stop(
      "Unparsable score values found: ",
      paste(shQuote(unparsable), collapse = ", "),
      call. = FALSE
    )
  }

  parsed
}

run_correlation <- function(data, observer_1, observer_2, method) {
  complete_rows <- complete.cases(data[[observer_1]], data[[observer_2]])
  x <- data[[observer_1]][complete_rows]
  y <- data[[observer_2]][complete_rows]

  test <- if (identical(method, "spearman")) {
    cor.test(x, y, method = method, exact = FALSE)
  } else {
    cor.test(x, y, method = method)
  }

  data.frame(
    observer_1 = observer_1,
    observer_2 = observer_2,
    method = method,
    coefficient = if (identical(method, "spearman")) "rho" else "r",
    n = length(x),
    estimate = unname(test$estimate),
    p_value = test$p.value,
    stringsAsFactors = FALSE
  )
}

workbook_path <- find_workbook()
scoring_raw <- read_excel(workbook_path, sheet = 1)

scoring <- data.frame(
  picture_no = scoring_raw[[1]],
  BJ = as.numeric(scoring_raw[[4]]),
  LE_H = as.numeric(scoring_raw[[5]]),
  TH = parse_observer_scores(scoring_raw[[6]]),
  stringsAsFactors = FALSE
)

results <- do.call(
  rbind,
  lapply(
    c("spearman", "pearson"),
    function(method) {
      rbind(
        run_correlation(scoring, "BJ", "LE_H", method),
        run_correlation(scoring, "BJ", "TH", method)
      )
    }
  )
)

results$estimate <- round(results$estimate, 3)
results$p_value <- signif(results$p_value, 3)

cat("Scoring validation correlations\n")
cat("Workbook:", workbook_path, "\n")
cat("Tony's ranged scores were converted to midpoints (e.g. 4-5 -> 4.5).\n\n")
print(results, row.names = FALSE)

