# Exploratory Multidimensional Agency Models

#This script explores alternative multidimensional item response theory (IRT) 
#models to examine the structure of agency as a construct of sustainable 
#development competence.

# Note: The dataset used in this analysis is not included in this
# repository because it contains data collected from human participants.
# Individual-level response data are withheld to protect participants'
# privacy and confidentiality.


# Load required package
library(mirt)


# Load datasets
Scores <- read.csv("Scores.csv")
Scores_ <- read.csv("Scores_.csv")

# Data preparation
# Remove participants with no valid responses across all items.
# Participants with partially completed responses are retained.
Scores_model <- Scores_[rowSums(!is.na(Scores_)) > 0, , drop = FALSE]

# Initial Multidimensional Model

# This model specifies seven agency subconstructs across the four
# test cases (A–D). Items are grouped according to their theoretical
# subconstructs rather than their original test case.

# Four additional factors represent the test cases.
# Their correlations are explicitly specified in the model.

structure <- mirt::mirt.model("
  Theta_Coagency = A1-A3, B4, B10, B25, C5-C6, C22, D8, D10-D11
  Theta_Identity_Belonging = A4-A6, B1, B5, B7, C3-C4, C12, D6-D7, D9
  Theta_Motiv_Selfdet_Autonomy_Cont = A7-A11, B2-B3, B6, B8-B9, C1-C2, C7-C8, C24, D1-D5
  Theta_Hope = A12-A14, B11-B13, C9-C11, D21-D23
  Theta_SelfEff_SelfReac_Forethought_SelfRefl_Cont = A15-A20, B14-B19, C13-C18, D12-D17
  Theta_GrowthMindset = A21-A23, B20-B22, C19-C21, D18-D20
  Theta_Purpose = A24-A26, B23-B24, B26, C23, C25,C26, D24-D26

  Theta_CaseA = A1-A26
  Theta_CaseB = B1-B26
  Theta_CaseC = C1-C26
  Theta_CaseD = D1-D26

  COV = Theta_CaseA*Theta_CaseB*Theta_CaseC*Theta_CaseD
", itemnames = Scores_model)


# Fit using quasi-Monte Carlo EM instead of the default quadrature EM
model <- mirt::mirt(
  data = Scores_model,
  model = structure,
  itemtype = "gpcm",
  method = "QMCEM"
)

# Extract model parameter estimates and calculate the M2 fit statistic.
mirt::coef(model, simplify = TRUE)
mirt::M2(model, na.rm = TRUE, QMC = TRUE)

# Correlated Four-Factor Model by Test Case

# This model specifies one factor for each test case (A–D),
# allowing the four factors to correlate.

structure_1 <- mirt::mirt.model( "Theta_CaseA = A1-A26
                                   Theta_CaseB = B1-B26
                                   Theta_CaseC = C1-C26
                                   Theta_CaseD = D1-D26
                                  COV = Theta_CaseA * Theta_CaseB * Theta_CaseC * Theta_CaseD", itemnames = Scores_model)

model_1 <- mirt::mirt(data = Scores_model, model = structure_1, itemtype = "gpcm")
mirt::coef(model_1, simplify = TRUE)
mirt::M2(model_1, na.rm = TRUE, QMC = TRUE)

# Second-Order Agency Model

# This model specifies seven first-order agency subconstructs
# and a second-order factor representing overall agency.

structure_2 <- mirt::mirt.model( "Theta_Coagency= A1-A3,B4,B10, B25, C5-C6, C22, D8, D10-D11
                                   Theta_Identity_Belonging = A4-A6, B1, B5, B7, C3-C4,C12, D6-D7, D9  
                                   Theta_Motiv_Selfdet_Autonomy_Cont= A7-A11,B2-B3,B6, B8-B9,C1-C2,C7-C8, C24, D1-D5
                                   Theta_Hope= A12-A14, B11-B13, C9-C11, D21-D23
                                   Theta_SelfEff_SelfReac_Forethought_SelfRefl_Cont= A15-A20, B14-B19, C13-C18, D12-D17 
                                   Theta_GrowthMindset= A21-A23, B20-B22, C19-C21, D18-D20
                                   Theta_Purpose= A24-A26, B23-B24, B26, C23, C25-C26, D24-D26
                                   Theta_Agency = Theta_Coagency, Theta_Identity_Belonging, Theta_Motiv_Selfdet_Autonomy_Cont,
                                    Theta_Hope, Theta_SelfEff_SelfReac_Forethought_SelfRefl_Cont, Theta_GrowthMindset, Theta_Purpose", itemnames = Scores_model)

model_2 <- mirt::mirt(data = Scores_model, model = structure_2, itemtype = "gpcm")
mirt::coef(model_2, simplify = TRUE)
mirt::M2(model_2, na.rm = TRUE, QMC = TRUE)

# Combined Subconstruct and Test Case Model

# This model includes seven agency subconstructs, four test-case
# factors, and a second-order agency factor.

structure_3 <- mirt::mirt.model("Theta_Coagency = A1-A3,B4,B10, B25, C5-C6, C22, D8, D10-D11
                                Theta_Identity_Belonging = A4-A6, B1, B5, B7, C3-C4,C12, D6-D7, D9  
                                Theta_Motiv_Selfdet_Autonomy_Cont = A7-A11,B2-B3,B6, B8-B9,C1-C2,C7-C8, C24, D1-D5
                                Theta_Hope = A12-A14, B11-B13, C9-C11, D21-D23
                                Theta_SelfEff_SelfReac_Forethought_SelfRefl_Cont = A15-A20, B14-B19, C13-C18, D12-D17 
                                Theta_GrowthMindset = A21-A23, B20-B22, C19-C21, D18-D20
                                Theta_Purpose= A24-A26, B23-B24, B26, C23, C25-C26, D24-D26
                                Theta_CaseA = A1-A26
                                Theta_CaseB = B1-B26
                                Theta_CaseC = C1-C26
                                Theta_CaseD = D1-D26
                                Theta_Agency = Theta_Coagency, Theta_Identity_Belonging, Theta_Motiv_Selfdet_Autonomy_Cont,
                                    Theta_Hope, Theta_SelfEff_SelfReac_Forethought_SelfRefl_Cont, Theta_GrowthMindset, Theta_Purpose
                                
                                
                                COV = Theta_CaseA * Theta_CaseB * Theta_CaseC * Theta_CaseD", itemnames = Scores_model)


model_3 <- mirt::mirt(data = Scores_model, model = structure_3, itemtype = "gpcm")
mirt::coef(model_cov, simplify = TRUE)
mirt::M2(model_cov, na.rm = TRUE, QMC = TRUE)

# Item Parameters and Latent Trait Scores

# Extract item parameter estimates
betasEst <- -mirt::coef(model, simplify = TRUE)$items

# Estimate latent trait scores using the MAP method
ThetasEst <- mirt::fscores(model, method = "MAP")

# Assign descriptive names to the estimated latent trait scores

ThetasEst <- ThetasEst[, 1:7, drop = FALSE]

colnames(ThetasEst) <- c(
  "EstTheta_CoAgency",
  "EstTheta_Identity",
  "EstTheta_Motivation",
  "EstTheta_Hope",
  "EstTheta_SelfEfficacy",
  "EstTheta_GrowthMindset",
  "EstTheta_Purpose"
)
theta_names <- c(
  "EstTheta_CoAgency",
  "EstTheta_Identity",
  "EstTheta_Motivation",
  "EstTheta_Hope",
  "EstTheta_SelfEfficacy",
  "EstTheta_GrowthMindset",
  "EstTheta_Purpose"
)

if (ncol(ThetasEst) != length(theta_names)) {
  stop("ThetasEst must have exactly 7 columns before renaming.")
}

colnames(ThetasEst) <- theta_names

#Revised Models


# Revised Correlated Model

# This specification includes seven agency subconstructs and
# four test-case factors. Correlations are specified separately
# among the test-case factors and among the agency subconstructs.

structure_cov <- mirt::mirt.model("Theta_Coagency = A1-A3,B4,B10, B25, C5-C6, C22, D8, D10-D11
                                Theta_Identity_Belonging = A4-A6, B1, B5, B7, C3-C4,C12, D6-D7, D9  
                                Theta_Motiv_Selfdet_Autonomy_Cont = A7-A11,B2-B3,B6, B8-B9,C1-C2,C7-C8, C24, D1-D5
                                Theta_Hope = A12-A14, B11-B13, C9-C11, D21-D23
                                Theta_SelfEff_SelfReac_Forethought_SelfRefl_Cont = A15-A20, B14-B19, C13-C18, D12-D17 
                                Theta_GrowthMindset = A21-A23, B20-B22, C19-C21, D18-D20
                                Theta_Purpose= A24-A26, B23-B24, B26, C23, C25-C26, D24-D26
                                Theta_CaseA = A1-A26
                                Theta_CaseB = B1-B26
                                Theta_CaseC = C1-C26
                                Theta_CaseD = D1-D26
                        
                                COV = Theta_CaseA * Theta_CaseB * Theta_CaseC * Theta_CaseD,
                                  Theta_Coagency*Theta_Identity_Belonging*Theta_Motiv_Selfdet_Autonomy_Cont*Theta_Hope*Theta_SelfEff_SelfReac_Forethought_SelfRefl_Cont*Theta_GrowthMindset*Theta_Purpose", itemnames = Scores_model)

# Fit the revised correlated model using QMCEM estimation
model_cov <- mirt::mirt(data = Scores_model, model = structure_cov, itemtype = "gpcm", method = "QMCEM")
mirt::coef(model_cov, simplify = TRUE)
mirt::M2(model_cov, na.rm = TRUE, QMC = TRUE)

# General and Specific Factor Model

# This specification includes a general agency factor loading
# on all items, seven specific agency subconstructs, and
# four test-case factors.

structure_Scov <- mirt::mirt.model("Theta_Agency = A1-A26, B1-B26, C1-C26, D1-D26
                                     Theta_Coagency = A1-A3,B4,B10, B25, C5-C6, C22, D8, D10-D11
                                     Theta_Identity_Belonging = A4-A6, B1, B5, B7, C3-C4,C12, D6-D7, D9  
                                     Theta_Motiv_Selfdet_Autonomy_Cont = A7-A11,B2-B3,B6, B8-B9,C1-C2,C7-C8, C24, D1-D5
                                     Theta_Hope = A12-A14, B11-B13, C9-C11, D21-D23
                                     Theta_SelfEff_SelfReac_Forethought_SelfRefl_Cont = A15-A20, B14-B19, C13-C18, D12-D17 
                                     Theta_GrowthMindset = A21-A23, B20-B22, C19-C21, D18-D20
                                     Theta_Purpose = A24-A26, B23-B24, B26, C23, C25-C26, D24-D26
                                     Theta_CaseA = A1-A26
                                     Theta_CaseB = B1-B26
                                     Theta_CaseC = C1-C26
                                     Theta_CaseD = D1-D26", itemnames = Scores_model)

# Fit the general and specific factor model using QMCEM estimation
model_Scov <- mirt::mirt(data = Scores_model, model = structure_Scov, itemtype = "gpcm", method = "QMCEM")
mirt::coef(model_Scov, simplify = TRUE)
mirt::M2(model_Scov, na.rm = TRUE, QMC = TRUE)


# Higher-Order and Subconstruct Model

# This specification further divides selected agency subconstructs
# into narrower dimensions, while retaining the broader constructs
# and the four test-case factors.


structure_SupCoSubcoCa <- mirt::mirt.model("Theta_Agency = A1-A26, B1-B26, C1-C26, D1-D26
                                     Theta_Coagency= A1-A3,B4,B10, B25, C5-C6, C22, D8, D10-D11
                                     Theta_Identity_Belonging = A4-A6, B1, B5, B7, C3-C4,C12, D6-D7, D9  
                                     Theta_Motiv_Selfdet_Autonomy_Cont = A7-A11,B2-B3,B6, B8-B9,C1-C2,C7-C8, C24, D1-D5
                                     Theta_Motiv = A7-A8,B2,B6,C1,C24,D1,D5
                                     Theta_Selfdet = A9,B3,C2,D2
                                     Theta_Autonomy = A10,B8,C8,D3
                                     Theta_Control = A11,B9,C7, D4
                                     Theta_Hope = A12-A14, B11-B13, C9-C11, D21-D23
                                     Theta_SelfEff_SelfReac_Forethought_SelfRefl_Cont = A15-A20, B14-B19, C13-C18, D12-D17
                                     Theta_SelfEff= A15-A16, B14-B15, C13-C14, D12-D13 
                                     Theta_SelfReac= A17, B16, C15, D16 
                                     Theta_Forethought= A18, B17, C16, D17 
                                     Theta_SelfRefl= A19, B18, C17, D14 
                                     Theta_Control= A20, B19, C18, D15 
                                     Theta_GrowthMindset = A21-A23, B20-B22, C19-C21, D18-D20
                                     Theta_Purpose= A24-A26, B23-B24, B26, C23, C25-C26, D24-D26
                                     Theta_CaseA = A1-A26
                                     Theta_CaseB = B1-B26
                                     Theta_CaseC = C1-C26
                                     Theta_CaseD = D1-D26", itemnames = Scores_model)

# Fit the higher-order and subconstruct model using QMCEM estimation
model_SupCoSubcoCa <- mirt::mirt(data = Scores_model, model = structure_SupCoSubcoCa, itemtype = "gpcm", method = "QMCEM")
mirt::coef(model_SupCoSubcoCa, simplify = TRUE)
mirt::M2(model_SupCoSubcoCa, na.rm = TRUE, QMC = TRUE)
