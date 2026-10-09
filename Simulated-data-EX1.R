############# Simulated Data for Exercise 1 - IPW From Scratch ###############

library(tidyverse)

# sample size
N <- 300

# set seed

set.seed(1234)


# --------------------- Generate Covariates X ---------------------

# X1: age at baseline (years)
X1 <- round(runif(N, min = 20, max = 85))

# X2: baseline systolic blood pressure (mmHg)
X2 <- round(rnorm(N, mean = 140, sd = 15),2)

# X3: diabetes diagnosis at baseline (1 = yes, 0 = no)
X3 <- rbinom(N, size = 1, prob = 0.35)



# Visualize covariate distributions

hist(X1)
hist(X2)
table(X3)


data <- as.data.frame(cbind(X1,X2,X3))


# ---------------- Generate Treatment Assignment A ---------------------

# Probability of receiving treatment (A=1)
beta0 <- -0.5
beta1 <- 0.4
beta2 <- 0.7
beta3 <- 0.5

# (Covariates - mean) / sd
normalized_X1 <- (data$X1-52.5)/sqrt((85 - 20)^2 / 12)
normalized_X2 <- (data$X2-140)/15


# logit[P(A=1|X)] = beta0 + beta1*X1 + beta2*X2 + beta3*X3
# plogis = expit
probab_treatment <- plogis(beta0 + beta1*normalized_X1 + beta2*normalized_X2 +
                             beta3*data$X3)

hist(probab_treatment)


data$Prob_Treatment <- round(probab_treatment,4)

# ------------- Generate Potential Outcomes Y(0) and Y(1) ---------------

# Probability of a cardiovascular event within one year (1 = yes, 0 = no)
# logit[P(Y=1|A,X)] = alpha_0 + alpha_1*A + alpha_2*X1 + alpha_3*X2 + alpha_4*X3

alpha0 <- -0.5
alpha1 <- -0.8 # negative, because the treatment protects against the outcome
alpha2 <- 0.4
alpha3 <- 0.7
alpha4 <- 0.2


# Observed treatment
data$A <- rbinom(N, 1, prob = data$Prob_Treatment)


probab_outcome <- function(A){
                  plogis(alpha0 + alpha1*A + alpha2*normalized_X1 +
                           alpha3*normalized_X2 + alpha4*data$X3)
}


probab_Y0 <- probab_outcome(0)

probab_Y1 <- probab_outcome(1)


outcome_under_A0 <- rbinom(N, 1, probab_Y0)
outcome_under_A1 <- rbinom(N, 1, probab_Y1)


data$Y0 <- outcome_under_A0
data$Y1 <- outcome_under_A1

data$Y_Observed <- ifelse(data$A==1,data$Y1,data$Y0)
  
  
### -------------- REAL AVERAGE TREATMENT EFFECT  --------------

# Real ATE in sample N=300
mean(probab_Y1 - probab_Y0)

# Observed ATE (with confounding)
with_treatment <- data[data$A==1,] 
no_treatment <- data[data$A==0,] 

mean(with_treatment$Y_Observed) - mean(no_treatment$Y_Observed)


### ------------------- Save Results ------------------------------


data_observed <- data %>% select(X1,X2,X3,A,Y_Observed)

saveRDS(data_observed,"EX1_Data_Observed.RDS")
saveRDS(data,"EX1_Data_Counterfactual.RDS")

