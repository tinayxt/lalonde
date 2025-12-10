# ====================================================
# PROJECT: LaLonde (1986) Replication and Extension
# FILE: Replication_Code.R
# ====================================================

# Completely clean up the workspace objects
rm(list = ls()) 

# Clear memory (release unused memory)
gc() 

# Try to increase the memory limit that R can use (this is only effective on certain operating systems)
# Objective: Set the memory limit to 8GB or higher (if the system permits)
# warning: This command may have been deprecated or become invalid in some of the latest versions of R or on certain operating systems
# memory.limit(size = 8192) 

# ----------------------------------------------------
# Step 1: Load the necessary R packages (make sure % > % is available)
# ----------------------------------------------------
library(tidyverse)
library(fixest)
library(MatchIt)
library(modelsummary)
library(haven)

# ----------------------------------------------------
# Step 2: Load data
# ----------------------------------------------------
# 2.1 Import the NSW experimental data

data_exp <- read_dta("/Users/yxt/lalonde/data/lalonde/nsw.dta")

# 2.2 Import the data of the CPS control group
data_cps <- read_dta("/Users/yxt/lalonde/data/lalonde/cps_controls.dta")
# ----------------------------------------------------
# Step 3: Run the OLS model (model_ols_2 is required for Step 5)
# ----------------------------------------------------

covariates_exp <- c("age", "education", "black", "hispanic", "married", "nodegree", "re75") 

model_ols_1 <- feols(re78 ~ treat, data = data_exp, vcov = "hetero")
formula_ols_2_exp <- as.formula(paste("re78 ~ treat +", paste(covariates_exp, collapse = " + ")))

model_ols_2 <- feols(formula_ols_2_exp, data = data_exp, vcov = "hetero")
# Key correction: Uniformly define PSM covariates (common columns in the two datasets)

covariates_psm <- c("age", "education", "black", "hispanic", "married", "nodegree", "re75") 
# ----------------------------------------------------
# Step 4: Result presentation (Replicate Lalonde table)
# ----------------------------------------------------
# Use etable to quickly display the results in the console (remains unchanged)
etable(model_ols_1, model_ols_2, 
       title = "OLS Estimates of Treatment Effect (NSW Experimental)",
       dict = c(treat = "Treated"),
       tex = FALSE 
)

# Use modelsummary to generate professional tables (output in markdown format, viewable in Viewer or Console)
modelsummary(
  list("Bivariate OLS" = model_ols_1, 
       "Full Covariates OLS" = model_ols_2),
  title = "Replication of OLS Estimates (Experimental Data)",
  # Note: Changed the output format to Markdown to avoid write permission errors
  output = "markdown" 
)

# 1. Extract the treatment group (Treated) and the control group (Control)
# Ensure that all variables are identified as numerical and undergo strict screening

data_treated <- data_exp %>% 
  mutate(treat = as.numeric(treat)) %>% 
  filter(treat == 1) %>%
  select(all_of(c("treat", "re78", covariates_psm))) 

data_control_cps <- data_cps %>% 
  mutate(treat = as.numeric(treat)) %>% 
  filter(treat == 0) %>%
  select(all_of(c("treat", "re78", covariates_psm)))

# 2. Merge the datasets and conduct final cleaning
data_psm_analysis <- bind_rows(data_treated, data_control_cps) %>%
  mutate(is_treated = as.integer(treat)) %>%
  # Remove any rows that contain "NA" to prevent the matching from failing
  drop_na(all_of(c("is_treated", "re78", covariates_psm))) 

# At this point of execution, check the sample size:
cat("PSM Sample Size:", nrow(data_psm_analysis), "\n")

# 3. Matching Settings
formula_match <- as.formula(paste("is_treated ~", paste(covariates_psm, collapse = " + ")))

# Run Nearest Neighbor matching (if this crashes, try 'method = "full"')
match_results <- matchit(formula_match, 
                         data = data_psm_analysis, 
                         method = "full", 
                         distance = "glm",
                         ratio = 1,
                         replace = FALSE) 

# After successful execution, check the summary of the matching results
cat("\n--- Matching completed successfully --- \n")
summary(match_results)

# 4. Extract matched data and estimate the treatment effect
matched_data <- match.data(match_results)

formula_psm <- as.formula(paste("re78 ~ is_treated +", paste(covariates_psm, collapse = " + ")))
model_psm <- feols(formula_psm, 
                   data = matched_data, 
                   vcov = "hetero") 

# 5. Display results
cat("\n--- PSM Matching Balance Summary ---\n")
summary(match_results, standardize = TRUE)

etable(model_ols_2, model_psm,
       title = "Comparison of OLS and PSM Estimates", 
       dict = c(is_treated = "Treated", treat = "Treated"), 
       tex = FALSE
)

