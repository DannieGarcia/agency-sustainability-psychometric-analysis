# Exploratory Multidimensional Agency Models: Case A

# This script explores alternative multidimensional item response
# theory (IRT) models for the 26 items in Case A.

# Three model structures are examined:
# 1. Correlated seven-factor model.
# 2. Bifactor model.
# 3. Second-order factor model.

# All models are estimated using the generalized partial credit
# model (GPCM) with the QMCEM estimation method.



# Exploratory Multidimensional Agency Models – Case A

library(mirt)

# Load the dataset
Scores <- read.csv("Scores (1).csv")

# Select item response columns, excluding the first four metadata columns
items <- Scores[, -(1:4)]


# Case A

# Select the 26 items corresponding to Case A
CaseA <- items[, 1:26]

# Retain participants with at least one observed response in Case A
non_empty_rows <- apply(CaseA, 1, function(row) !all(is.na(row)))
clean_A <- CaseA[non_empty_rows, ]


# Correlated Seven-Factor Model
#
# This was the first model structure explored.
# It specifies seven agency subconstructs as correlated factors.

correlated_structure <- mirt::mirt.model("Theta_Coagency = A1-A3
                                    Theta_Identity_Belonging = A4-A6  
                                    Theta_Motiv_Selfdet_Autonomy_Cont = A7-A11
                                    Theta_Hope = A12-A14
                                    Theta_SelfEff_SelfReac_Forethought_SelfRefl_Cont = A15-A20 
                                    Theta_GrowthMindset = A21-A23
                                    Theta_Purpose = A24-A26
                                    COV = Theta_Coagency * Theta_Identity_Belonging * Theta_Motiv_Selfdet_Autonomy_Cont * Theta_Hope * Theta_SelfEff_SelfReac_Forethought_SelfRefl_Cont * Theta_GrowthMindset * Theta_Purpose",
                                   itemnames = clean_A)
correlated_model <- mirt::mirt(data = clean_A, model = correlated_structure, itemtype = "gpcm", method = 'QMCEM', technical = list(NCYCLES = 10000))

# Fit the correlated seven-factor model
mirt::coef(correlated_model, simplify = TRUE)
mirt::M2(correlated_model, na.rm = TRUE, QMC = TRUE)


# Bifactor Model

# This specification includes seven specific factors and one
# general factor (Theta_CaseA) loading on all 26 items.


bifactor_structure <- mirt::mirt.model("Theta_Coagency = A1-A3
                                    Theta_Identity_Belonging = A4-A6  
                                    Theta_Motiv_Selfdet_Autonomy_Cont = A7-A11
                                    Theta_Hope = A12-A14
                                    Theta_SelfEff_SelfReac_Forethought_SelfRefl_Cont = A15-A20 
                                    Theta_GrowthMindset = A21-A23
                                    Theta_Purpose = A24-A26
                                    Theta_CaseA = A1-A26",
                                    itemnames = clean_A)


# Fit the bifactor model                                                                  
bifactor_model <- mirt::mirt(data = clean_A, model = bifactor_structure, itemtype = "gpcm", method = 'QMCEM', technical = list(NCYCLES = 10000), missing = 'Complete')

mirt::coef(bifactor_model, simplify = TRUE)
mirt::M2(bifactor_model, na.rm = TRUE, QMC = TRUE)


# Second-Order Factor Model

# This specification includes seven first-order factors representing
# agency subconstructs and a second-order factor (Theta_Agency)
# defined in terms of these seven dimensions.


second_order_structure <- mirt::mirt.model("Theta_Coagency = A1-A3
                                    Theta_Identity_Belonging = A4-A6  
                                    Theta_Motiv_Selfdet_Autonomy_Cont = A7-A11
                                    Theta_Hope = A12-A14
                                    Theta_SelfEff_SelfReac_Forethought_SelfRefl_Cont = A15-A20 
                                    Theta_GrowthMindset = A21-A23
                                    Theta_Purpose = A24-A26
                                    Theta_Agency = Theta_Coagency, Theta_Identity_Belonging, Theta_Motiv_Selfdet_Autonomy_Cont,
                                    Theta_Hope, Theta_SelfEff_SelfReac_Forethought_SelfRefl_Cont, Theta_GrowthMindset, Theta_Purpose",
                                        itemnames = clean_A)
second_order_model <- mirt::mirt(data = clean_A, model = second_order_structure, itemtype = "gpcm", method = 'QMCEM', technical = list(NCYCLES = 10000), missing = 'Complete')

# Fit the second-order factor model
mirt::coef(second_order_model, simplify = TRUE)
mirt::M2(second_order_model, na.rm = TRUE, QMC = TRUE)
