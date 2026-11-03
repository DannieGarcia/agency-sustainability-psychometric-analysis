# Unidimensional Analysis


# This script fits unidimensional graded response models to the items
# of each of the four test cases (A–D) using the mirt package.
# Item parameter estimates and model fit statistics are extracted
# for each case.

# The data set is not included in this repository because it contains
# participant data collected for academic research and is not publicly shared.



# Load the mirt package for item response theory (IRT) analysis
library(mirt)

# Load the dataset and select the item response columns
Scores <- read.csv("Scores (1).csv")
items <- Scores[, -(1:4), drop = FALSE]

# Fit unidimensional graded response models for each case

# Case A
Model_A <- mirt(data = items[, 1:26],
                model = 1, itemtype = "graded")

# Extract item parameter estimates
coef_A <- mirt::coef(Model_A, simplify = TRUE, IRTpars = TRUE)$items

# Calculate model fit statistics
mod_fit_A <- mirt::M2(Model_A, na.rm = TRUE)

# Case B
Model_B <- mirt(data = items[, 27:52],
                model = 1, itemtype = "graded")

# Extract item parameter estimates
coef_B <- mirt::coef(Model_B, simplify = TRUE, IRTpars = TRUE)$items

# Calculate model fit statistics
mod_fit_B <- mirt::M2(Model_B, na.rm = TRUE)

# Case C
Model_C <- mirt(data = items[, 53:78],
                model = 1, itemtype = "graded")

# Extract item parameter estimates
coef_C <- mirt::coef(Model_C, simplify = TRUE, IRTpars = TRUE)$items

# Calculate model fit statistics
mod_fit_C <- mirt::M2(Model_C, na.rm = TRUE)

# Case D
Model_D <- mirt(data = items[, 79:104],
                model = 1, itemtype = "graded")

# Extract item parameter estimates
coef_D <- mirt::coef(Model_D, simplify = TRUE, IRTpars = TRUE)$items

# Calculate model fit statistics
mod_fit_D <- mirt::M2(Model_D, na.rm = TRUE)

