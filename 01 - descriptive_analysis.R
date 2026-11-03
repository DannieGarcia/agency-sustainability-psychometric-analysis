# Descriptive Statistics and Frequency Distribution Plots

# Data availability and confidentiality
# The original data set is not included in this repository because it contains
# individual responses from student participants. The first four columns
# contain participant identifiers and test administration information and
# are excluded from the analyses in this script.
# The analyses require authorized access to the original data set.

#Packages
library(dplyr)

#Data frame
Scores <- read.csv('Scores (1).csv')
items <- Scores[, -(1:4), drop = FALSE]

# Absolute frequencies and percentages of participants by number of test cases completed

# Function to count non-missing responses for a given case
case_responses <- function(row, case) {
  case_columns <- grep(paste0("^", case), names(row), value = TRUE)
  return(sum(!is.na(row[case_columns])))
}

# Function to identify the test version and count the cases answered
version <- function(row) {
  cases <- c("A", "B", "C", "D")
  assigned_cases <- toupper(c(row["Caso_1"], row["Caso_2"], row["Caso_3"]))
  
  cases_answered <- sapply(assigned_cases, function(case) {
    if (case_responses(row, case) > 0) {
      return(case)
    } else {
      return(NA)
    }
  })

  
  cases_answered <- cases_answered[!is.na(cases_answered)]
  
  return(list(
    version = paste(sort(assigned_cases), collapse = ""),
    cases_answered = length(cases_answered)
  ))
}

# Apply the function to each row of the dataframe
results <- apply(Scores, 1, version)
Scores$version <- sapply(results, function(x) x$version)
Scores$cases_answered <- sapply(results, function(x) x$cases_answered)

# Create the summary table
results_table <- Scores %>%
  group_by(version) %>%
  summarize(
    resp_3_freq = sum(cases_answered == 3),
    resp_3_perc = resp_3_freq / n() * 100,
    resp_2_freq = sum(cases_answered == 2),
    resp_2_perc = resp_2_freq / n() * 100,
    part_per_version = n()
  )

# Display the summary table
print(results_table)

####################################################################
# Descriptive Statistics and Frequency Distribution of Total Scores

#Case A
CaseA<- items[, 1:26]
non_empty_rows <- apply(CaseA, 1, function(row) !all(is.na(row)))
clean_A <- CaseA[non_empty_rows, ]
totalscoresA <- rowSums(clean_A, na.rm = TRUE)

# Descriptive statistics
mean(totalscoresA) 
sd(totalscoresA)   

# Calculate the minimum, maximum, and range
min_max <- range(totalscoresA) 
score_rng <- diff(min_max) 

#Case B
CaseB<- items[, 27:52]
non_empty_rows <- apply(CaseB, 1, function(row) !all(is.na(row)))
clean_B <- CaseB[non_empty_rows, ]
totalscoresB <- rowSums(clean_B, na.rm = TRUE)

# Descriptive statistics
mean(totalscoresB) 
sd(totalscoresB) 

# Calculate the minimum, maximum, and range
min_max_B <- range(totalscoresB) 
score_rng_B <- diff(min_max_B) 

#Case C
CaseC<- items[, 53:78]
non_empty_rows <- apply(CaseC, 1, function(row) !all(is.na(row)))
clean_C <- CaseC[non_empty_rows, ]
totalscoresC <- rowSums(clean_C, na.rm = TRUE)

# Descriptive statistics
mean(totalscoresC) 
sd(totalscoresC) 

# Calculate the minimum, maximum, and range
min_max_C <- range(totalscoresC) 
score_rng_C <- diff(min_max_C) 

#Case D
CaseD<- items[, 79:104]
non_empty_rows <- apply(CaseD, 1, function(row) !all(is.na(row)))
clean_D <- CaseD[non_empty_rows, ]
totalscoresD <- rowSums(clean_D, na.rm = TRUE)

# Descriptive statistics
mean(totalscoresD) 
sd(totalscoresD) 

# Calculate the minimum, maximum, and range
min_max_D <- range(totalscoresD) 
score_rng_D <- diff(min_max_D)

# Create Plots for all cases 

par(mfrow = c(2, 2))

# Bar plots of scores
barplot_A <- barplot(table(totalscoresA), main = "Frequency Distribution of Total Scores: Case A", xlab = "Total Scores", ylab = "Frequency", col = "grey", ylim = c(0,40))

barplot_B <- barplot(table(totalscoresB), main = "Frequency Distribution of Total Scores: Case B", xlab = "Total Scores", ylab = "Frequency", col = "grey", ylim = c(0,40))

barplot_C <- barplot(table(totalscoresC), main = "Frequency Distribution of Total Scores: Case C", xlab = "Total Scores", ylab = "Frequency", col = "grey", ylim = c(0,40))

barplot_D <- barplot(table(totalscoresD), main = "Frequency Distribution of Total Scores: Case D", xlab = "Total Scores", ylab = "Frequency", col = "grey", ylim = c(0,40))
