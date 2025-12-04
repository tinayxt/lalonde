# ====================================================
# PROJECT: LaLonde (1986) Replication and Extension
# FILE: Replication_Code.R
# ====================================================

# 彻底清理工作区对象
rm(list = ls()) 

# 清理内存（释放未使用的内存）
gc() 

# 尝试增加R可以使用的内存限制（仅在某些操作系统上有效，但值得尝试）
# 目标: 将内存限制设置为 8GB 或更高 (如果您的系统允许)
# warning: 此命令在某些最新版本的R或特定操作系统中可能已被弃用或无效。
# memory.limit(size = 8192) 

# 重新运行步骤 1-3 的代码（加载包、加载数据、运行 model_ols_2）
# ----------------------------------------------------
# 步骤 1: 重新加载必要的R包 (确保 % > % 可用)
# ----------------------------------------------------
library(tidyverse)
library(fixest)
library(MatchIt)
library(modelsummary)
library(haven)

# ----------------------------------------------------
# 步骤 2: 重新加载数据
# ----------------------------------------------------
# 2.1 导入 NSW 实验数据
data_exp <- read_dta("/Users/yxt/lalonde/data/lalonde/nsw.dta")

# 2.2 导入 CPS 对照组数据
data_cps <- read_dta("/Users/yxt/lalonde/data/lalonde/cps_controls.dta")
# ----------------------------------------------------
# 步骤 3: 重新运行 OLS 模型 (model_ols_2 是步骤 5 所需的)
# ----------------------------------------------------
covariates_exp <- c("age", "education", "black", "hispanic", "married", "nodegree", "re75") 

model_ols_1 <- feols(re78 ~ treat, data = data_exp, vcov = "hetero")
formula_ols_2_exp <- as.formula(paste("re78 ~ treat +", paste(covariates_exp, collapse = " + ")))

model_ols_2 <- feols(formula_ols_2_exp, data = data_exp, vcov = "hetero")
# **关键修正：统一定义 PSM 协变量 (两个数据集的共同列)**
covariates_psm <- c("age", "education", "black", "hispanic", "married", "nodegree", "re75") 
# ----------------------------------------------------
# 步骤 4: 结果展示 (复制 LaLonde 表格)
# ----------------------------------------------------
# 使用 etable 快速在控制台展示结果 (保持不变)
etable(model_ols_1, model_ols_2, 
       title = "OLS Estimates of Treatment Effect (NSW Experimental)",
       dict = c(treat = "Treated"),
       tex = FALSE 
)

# 使用 modelsummary 生成专业表格 (输出为 markdown，在 Viewer 或 Console 中查看)
modelsummary(
  list("Bivariate OLS" = model_ols_1, 
       "Full Covariates OLS" = model_ols_2),
  title = "Replication of OLS Estimates (Experimental Data)",
  # **修正:** 更改输出格式为 markdown，避免写入权限错误
  output = "markdown" 
)


# 1. 提取处理组 (Treated) 和 对照组 (Control)
# 确保所有变量都被识别为数值，并进行严格筛选
data_treated <- data_exp %>% 
  mutate(treat = as.numeric(treat)) %>% 
  filter(treat == 1) %>%
  select(all_of(c("treat", "re78", covariates_psm))) 

data_control_cps <- data_cps %>% 
  mutate(treat = as.numeric(treat)) %>% 
  filter(treat == 0) %>%
  select(all_of(c("treat", "re78", covariates_psm)))

# 2. 合并数据集，并进行最终清理
data_psm_analysis <- bind_rows(data_treated, data_control_cps) %>%
  mutate(is_treated = as.integer(treat)) %>%
  # 移除任何包含NA的行，防止匹配崩溃
  drop_na(all_of(c("is_treated", "re78", covariates_psm))) 

# 运行到这里，检查样本量：
cat("PSM分析数据集的样本量:", nrow(data_psm_analysis), "\n")

# 3. 匹配设置
formula_match <- as.formula(paste("is_treated ~", paste(covariates_psm, collapse = " + ")))

# 运行 Nearest Neighbor 匹配 (如果这里崩溃，请尝试 'method = "full"')
match_results <- matchit(formula_match, 
                         data = data_psm_analysis, 
                         method = "full", 
                         distance = "glm",
                         ratio = 1,
                         replace = FALSE) 

# 运行成功后，检查匹配结果总结
cat("\n--- 匹配结果成功运行 --- \n")
summary(match_results)

# 4. 提取匹配后的数据和估计处理效应
matched_data <- match.data(match_results)

formula_psm <- as.formula(paste("re78 ~ is_treated +", paste(covariates_psm, collapse = " + ")))
model_psm <- feols(formula_psm, 
                   data = matched_data, 
                   vcov = "hetero") 

# 5. 结果展示
cat("\n--- PSM Matching Balance Summary ---\n")
summary(match_results, standardize = TRUE)

etable(model_ols_2, model_psm,
       title = "Comparison of OLS and PSM Estimates", 
       dict = c(is_treated = "Treated", treat = "Treated"), 
       tex = FALSE
)