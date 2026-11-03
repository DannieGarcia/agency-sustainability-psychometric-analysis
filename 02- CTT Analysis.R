# Classical Test Analysis

# This script examines item-level characteristics across the four
# test cases (A–D), including missing data, item difficulty,
# corrected item-total correlations, and response-category proportions.

# The dataset is not included in this repository because permission
# to publicly share the research data has not been obtained.


# Load the dataset
Scores <- read.csv("Scores (1).csv")


# Missing Data Analysis

# Identify test cases with at least one observed response
# for each participant.

case_items <- list(
  A = paste0("A", 1:26),
  B = paste0("B", 1:26),
  C = paste0("C", 1:26),
  D = paste0("D", 1:26)
)

Scores$answered_cases <- apply(Scores, 1, function(row) {
  answered <- names(case_items)[sapply(case_items, function(items) {
    any(!is.na(as.numeric(row[items])))
  })]
  
  paste(answered, collapse = "")
})
# Create a table of missing data by item
miss_tbl <- data.frame(
  item = grep("^[A-D]\\d+$", names(Scores), value = TRUE),
  n_missing = NA_integer_,
  pct_missing = NA_real_
)

for (i in seq_len(nrow(miss_tbl))) {
  item <- miss_tbl$item[i]
  
  # Keep only participants who provided at least one response
  # in the corresponding test case
  assigned_rows <- Scores[
    grepl(substr(item, 1, 1), Scores$answered_cases),
  ]
  
  n_resp <- nrow(assigned_rows)
  
  miss_tbl$n_missing[i] <- sum(is.na(assigned_rows[[item]]))
  miss_tbl$pct_missing[i] <- if (n_resp > 0) {
    miss_tbl$n_missing[i] / n_resp * 100
  } else {
    NA_real_
  }
}

# Display the missing data summary
print(miss_tbl)


# Item Difficulty

# Define item columns for each case

items <- Scores[, -(1:4), drop = FALSE]
case_columns <- list(
  A = 1:26,
  B = 27:52,
  C = 53:78,
  D = 79:104
)

difficulty_stats <- do.call(rbind, lapply(names(case_columns), function(case) {
  case_items <- items[, case_columns[[case]], drop = FALSE]
  item_means <- sapply(case_items, mean, na.rm = TRUE)
  item_maxima <- sapply(case_items, max, na.rm = TRUE)
  item_difficulty <- item_means / item_maxima
  item_difficulty <- item_difficulty[is.finite(item_difficulty)]
  data.frame(
    Case = case,
    Items = length(item_difficulty),
    Mean = mean(item_difficulty),
    SD = sd(item_difficulty),
    Minimum = min(item_difficulty),
    Maximum = max(item_difficulty),
    Range = diff(range(item_difficulty))
  )
}))

difficulty_stats

# Item-Rest Correlations (Corrected Item-Total Correlations)
# Calculate the correlation between each item and the sum of
# the remaining items within the same case.

item_cor <- do.call(rbind, lapply(names(case_cols), function(case) {
  x <- items[, case_cols[[case]], drop = FALSE]
  
  cors <- sapply(seq_along(x), function(i) {
    item <- x[[i]]
    rest <- x[, -i, drop = FALSE]
    
    # Require an observed item response and at least one observed
    # response among the remaining items for each participant
    valid <- !is.na(item) & rowSums(!is.na(rest)) > 0
    
    if (sum(valid) < 2) {
      return(NA_real_)
    }
    
    item_scores <- item[valid]
    rest_scores <- rowSums(rest[valid, , drop = FALSE], na.rm = TRUE)
    
    # Check for zero variance before calculating the correlation
    if (sd(item_scores) == 0 || sd(rest_scores) == 0) {
      return(NA_real_)
    }
    
    cor(item_scores, rest_scores)
  })
  
  data.frame(
    Case = case,
    Item = names(x),
    Correlation = as.numeric(cors),
    row.names = NULL
  )
}))

print(item_cor)


# Descriptive Statistics of Item-Rest Correlations
# Summarize item-rest correlations separately for each case.

cor_stats <- do.call(rbind, lapply(split(
  item_cor$Correlation,
  item_cor$Case
), function(x) {
  x <- x[is.finite(x)]
  
  data.frame(
    n_items = length(x),
    Mean = if (length(x) > 0) mean(x) else NA_real_,
    SD = if (length(x) > 1) sd(x) else NA_real_,
    Minimum = if (length(x) > 0) min(x) else NA_real_,
    Maximum = if (length(x) > 0) max(x) else NA_real_,
    Range = if (length(x) > 0) diff(range(x)) else NA_real_
  )
}))

cor_stats$Case <- rownames(cor_stats)
rownames(cor_stats) <- NULL
cor_stats <- cor_stats[, c(
  "Case", "n_items", "Mean", "SD", "Minimum", "Maximum", "Range"
)]

print(cor_stats)


# Response-Category Proportions, Item Difficulty, and Item-Rest Correlations
# Calculate the proportion of responses in each score category (0–3),
# normalized item difficulty, and item-rest correlations.

item_stats <- do.call(rbind, lapply(names(case_cols), function(case) {
  x <- items[, case_cols[[case]], drop = FALSE]
  
  do.call(rbind, lapply(names(x), function(item_name) {
    obs <- x[[item_name]][!is.na(x[[item_name]])]
    n_obs <- length(obs)
    
    # Calculate the proportion of responses in each category (0–3)
    cat_prop <- if (n_obs > 0) {
      tabulate(as.integer(obs) + 1L, nbins = 4) / n_obs
    } else {
      rep(NA_real_, 4)
    }
    
    data.frame(
      Case = case,
      Item = item_name,
      n = n_obs,
      p_0 = cat_prop[1],
      p_1 = cat_prop[2],
      p_2 = cat_prop[3],
      p_3 = cat_prop[4],
      Difficulty = if (n_obs > 0) mean(obs) / 3 else NA_real_,
      stringsAsFactors = FALSE
    )
  }))
}))

# Match item-rest correlations by both case and item
item_stats$item_rest_cor <- item_cor$Correlation[
  match(
    paste(item_stats$Case, item_stats$Item),
    paste(item_cor$Case, item_cor$Item)
  )
]

# Display item-level statistics
print(item_stats)

