library(tidyverse)
library(scales)
library(patchwork)
source("helper.R")

model_levels <- c("Planning", "Exploitation", "Learning", "Equity", "Equality", "General")
model_palette <- c(
  "Planning"     = "#e78ac3",
  "Exploitation" = "#a6d854",
  "Learning"     = "#8da0cb",
  "Equity"       = "#66c2a5",
  "Equality"     = "#e5c494",
  "General"      = "#fc8d62"
)

############### TRAINING ###############

# 1. combine all model evidence:
t1 <- read_csv("output/training/exp1_model_evidence.csv", col_names = FALSE) 
t2 <- read_csv("output/training/exp2_model_evidence.csv", col_names = FALSE)
t3 <- read_csv("output/training/exp3_model_evidence.csv", col_names = FALSE) 

training_model_evidence <- bind_rows(t1, t2, t3)
if (!file.exists(paste0("output/training_model_evidence.csv"))) {
  write.table(training_model_evidence, paste0("output/training_model_evidence.csv"), sep = ",", col.names = FALSE, row.names = FALSE)
}

# 2. random-effects Bayesian model selection:
# pass "output/training_model_evidence.csv" through `run_bms.m`
# outputs: `output/training_posterior.csv`, `output/training_protected_exceedance_probabilities.csv`
pxp <- read_csv("output/training_protected_exceedance_probabilities.csv", show_col_types = FALSE)
pxp$Planning
# the Planning model has a protected exceedance probability of close to 1, meaning that is it the most likely model in the population

# 3. plot model evidence:
colnames(training_model_evidence) <- model_levels
p1 <- plot_model_evidence(training_model_evidence) +
  labs(title = 'Training')

# 4. calculate how many participants are best explained by each model:
best_model_train <- summarize_best(training_model_evidence, model_levels)
p2 <- plot_model_share(best_model_train, model_levels, model_palette)

############### EXECUTION ###############

# 1. combine all log likelihoods:
e1 <- read_csv("output/execution/exp1_loglikelihood.csv", col_names = FALSE, show_col_types = FALSE)
e2 <- read_csv("output/execution/exp2_loglikelihood.csv", col_names = FALSE, show_col_types = FALSE)
e3 <- read_csv("output/execution/exp3_loglikelihood.csv", col_names = FALSE, show_col_types = FALSE) 

execution_log_likelihood <- bind_rows(e1, e2, e3)
if (!file.exists(paste0("output/execution_loglikelihood.csv"))) {
  write.table(execution_log_likelihood, paste0("output/execution_loglikelihood.csv"), sep = ",", col.names = FALSE, row.names = FALSE)
}

# 2. random-effects Bayesian model selection:
# pass "output/training_model_evidence.csv" through `run_bms.m`
# outputs: `output/training_posterior.csv`, `output/training_protected_exceedance_probabilities.csv`
pxp <- read_csv("output/execution_protected_exceedance_probabilities.csv", show_col_types = FALSE)
pxp$Planning
# the Planning model has a protected exceedance probability of close to 1, meaning that is it the most likely model in the population

# 3. plot model evidence:
colnames(execution_log_likelihood) <- model_levels
p3 <- plot_model_evidence(execution_log_likelihood) +
  labs(title = 'Execution')

# 4. calculate how many participants are best explained by each model:
best_model_exec <- summarize_best(execution_log_likelihood, model_levels)
p4 <- plot_model_share(best_model_exec, model_levels, model_palette)

# Figure 3
pdf('./../figures/figure3_model_comparison.pdf', onefile = T, width = 12, height = 6)
(p1 | p3) / (p2 | p4) + plot_annotation(tag_levels = 'A')
dev.off()
