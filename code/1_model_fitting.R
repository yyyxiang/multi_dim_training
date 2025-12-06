library(tidyverse)
source("helper.R")

# load data
dat <- read_csv("./../data/data.csv")

# remove subjects who failed attention check:
failed_subjects <- dat %>%
  filter(passed_attention == FALSE) %>%
  pull(subject) %>%
  unique()

dat <- dat %>%
  filter(!subject %in% failed_subjects) %>% 
  filter(!is.na(trial_id))

# initial parameters:
initial_conditions <- tibble(
  competence_structure = rep(1:3, each = 3),
  scenario = rep(1:3, times = 3),
  Xa = c(10, 15, 15, 2, 2, 5, 10, 5, 5),
  Xb = c(15, 20, 20, 10, 15, 15, 5, 5, 10),
  Ya = c(2, 2, 5, 2, 2, 5, 2, 2, 2),
  Yb = c(5, 5, 6, 2, 2, 5, 15, 20, 20))
difficulty_levels <- c(2, 10)
num_rounds <- 3
R <- 10
alphaX <- 5
alphaY <- 5
effort_vector <- seq(0, 1, by = 0.01)
beta_grid <- seq(0, 10, by = 0.5)

model_fitting <- function(global_exp) {
  planning_exp <- paste("Planning Condition", global_exp) 
  # this is used to pull the corresponding task structure reward calculation for the planning model
  
  ############### TRAINING ###############
  
  # filter data to experiment-specific data 
  exp_data_train <- dat %>%
    filter(exp == global_exp)
  
  # training trajectories
  exp_data_train <- exp_data_train %>%
    filter(round != 0) %>% 
    mutate(trained_weight = paste0(trained, weight_selected)) %>%
    arrange(subject, competence_structure, scenario, round) %>%
    group_by(subject, competence_structure, scenario) %>%
    summarize(trajectory = paste0(trained_weight, collapse = ""),
              .groups = "drop") 
  
  # 1. create lookup tables for all models:
  lookup_table <- tibble()
  
  for (b in beta_grid) {
    for (i in 1:nrow(initial_conditions)) {
      scen <- initial_conditions$competence_structure[i]
      subc <- initial_conditions$scenario[i]
      initial_cond <- c(
        Xa = initial_conditions$Xa[i],
        Xb = initial_conditions$Xb[i],
        Ya = initial_conditions$Ya[i],
        Yb = initial_conditions$Yb[i]
      )
      
      sim <- round_simulation(num_rounds, initial_cond, difficulty_levels, R, alphaX, alphaY, effort_vector)
      final <- sim[[num_rounds + 1]]
      
      df_sim_planning <- process_results(final, num_rounds, R, alphaX, alphaY, difficulty_levels,
                                         path_condition = planning_exp, beta = b)
      df_sim_learning <- process_results(final, num_rounds, R, alphaX, alphaY, difficulty_levels,
                                         path_condition = "Learning", beta = b)
      df_sim_exploitation<- process_results(final, num_rounds, R, alphaX, alphaY, difficulty_levels,
                                            path_condition = "Exploitation", beta = b)
      df_sim_equity <- process_results(final, num_rounds, R, alphaX, alphaY, difficulty_levels,
                                       path_condition = "Equity", beta = b)
      df_sim_equality <- process_results(final, num_rounds, R, alphaX, alphaY, difficulty_levels,
                                         path_condition = "Equality", beta = b)
      df_sim_general <- process_results(final, num_rounds, R, alphaX, alphaY, difficulty_levels,
                                        path_condition = "General", beta = b)
      df_sim <- bind_rows(df_sim_planning, df_sim_learning, df_sim_exploitation,
                          df_sim_equity, df_sim_equality, df_sim_general) %>%
        mutate(
          trajectory = paste0(trained_R1, d_R1, trained_R2, d_R2, trained_R3, d_R3),
          prob = SoftmaxProb,
          model = if_else(str_starts(PathCondition, "Planning"), "Planning", PathCondition),
          beta = b,
          competence_structure = scen,
          scenario = subc) %>%
        select(trajectory, model, prob, beta, competence_structure, scenario, Xa_R3, Xb_R3, Ya_R3, Yb_R3)
      
      lookup_table <- bind_rows(lookup_table, df_sim)
    }
  }
  lookup_save <- lookup_table %>%
    mutate(across(c(competence_structure, scenario, beta, prob, Xa_R3, Xb_R3, Ya_R3, Yb_R3), as.numeric),
           across(c(model, trajectory), as.character))
  
  # local save:
  if (!file.exists(paste0("output/training/exp", global_exp, "_lookup.csv"))) {
    write.csv(lookup_save, paste0("output/training/exp", global_exp, "_lookup.csv"), row.names = FALSE)
  }
  
  # 2. find each subject's best beta for each model:
  lookup_table <- read_csv(paste0("output/training/exp", global_exp, "_lookup.csv"))
  
  best_beta <- NULL
  for (subject_id in unique(exp_data_train$subject)) {
    sub_beta <- exp_data_train %>% filter(subject == subject_id) %>% 
      merge(lookup_table) %>% 
      arrange(model, beta, competence_structure, scenario)
    
    sub_beta <- sub_beta %>% 
      group_by(subject, model, beta) %>% 
      summarize(log_likelihood = sum(log(prob))) %>% 
      slice_max(log_likelihood, n = 1, with_ties = FALSE)
    
    best_beta <- rbind(best_beta, sub_beta)
  }
  
  # local save
  if (!file.exists(paste0("output/training/best_beta_exp", global_exp, ".csv"))) {
    write.csv(best_beta, paste0("output/training/best_beta_exp", global_exp, ".csv"), row.names = FALSE)
  }
  
  # 3. compute model evidence:
  best_beta <- read_csv(paste0("output/training/best_beta_exp", global_exp, ".csv")) %>%
    # reorder models
    mutate(model = factor(model, levels = c("Planning", "Exploitation", "Learning",
                                            "Equity", "Equality", "General"), ordered = TRUE))
  loglik <- best_beta %>%
    select(subject, model, log_likelihood) %>%
    pivot_wider(names_from = model, values_from = log_likelihood) %>%
    # remove subject col for running `run_bms.m`
    select(Planning, Exploitation, Learning, Equity, Equality, General)
  
  bic <- 1 * log(9) - 2 * loglik # 1 free parameter, 9 trajectories per subject
  model_evidence <- -0.5 * bic
  # local save (remove heading for running `run_bms.m`)
  if (!file.exists(paste0("output/training/exp", global_exp, "_model_evidence.csv"))) {
    write.table(model_evidence, paste0("output/training/exp", global_exp, "_model_evidence.csv"), sep = ",", col.names = FALSE, row.names = FALSE)
  }
  
  ############### EXECUTION ###############
  
  # load best beta and lookup tables
  best_beta <- read_csv(paste0("output/training/best_beta_exp", global_exp, ".csv"))
  lookup_table <- read_csv(paste0("output/training/exp", global_exp, "_lookup.csv"))
  
  # model-predicted execution probabilities:
  decision_rows <- list()
  
  for (i in 1:nrow(best_beta)) {
    subject_id <- best_beta$subject[i]
    model      <- best_beta$model[i]
    beta_sub   <- best_beta$beta[i]
    
    for (j in 1:nrow(initial_conditions)) {
      scen <- initial_conditions$competence_structure[j]
      subc <- initial_conditions$scenario[j]
      
      # Filter
      df_state <- lookup_table %>%
        filter(
          model == !!model,
          competence_structure == !!scen,
          scenario == !!subc,
          beta == !!beta_sub)    
      
      if (model == "Planning") {
        if (global_exp == 1) {
          df_alt <- df_state %>%
            rowwise() %>%
            mutate(
              # dim A:
              p_xa = as.numeric(softmax(c(Xa = Xa_R3, Ya = Ya_R3), beta = beta_sub)["Xa"]),
              p_ya = as.numeric(softmax(c(Xa = Xa_R3, Ya = Ya_R3), beta = beta_sub)["Ya"]),
              # dim B: 
              p_xb = as.numeric(softmax(c(Xb = Xb_R3, Yb = Yb_R3), beta = beta_sub)["Xb"]),
              p_yb = as.numeric(softmax(c(Xb = Xb_R3, Yb = Yb_R3), beta = beta_sub)["Yb"])
            ) %>%
            ungroup() %>%
            summarise( Xa = mean(p_xa), Ya = mean(p_ya), Xb = mean(p_xb), Yb = mean(p_yb)) %>%
            pivot_longer(everything(), names_to = "agent", values_to = "p_model")
        } else if (global_exp == 2) {
          df_alt <- df_state %>%
            rowwise() %>%
            mutate(
              pair_probs = list(softmax(c(
                Xa_Yb = Xa_R3 + Yb_R3,
                Xb_Ya = Xb_R3 + Ya_R3
              ))),
              Xa = pair_probs[["Xa_Yb"]],
              Yb = pair_probs[["Xa_Yb"]],
              Xb = pair_probs[["Xb_Ya"]],
              Ya = pair_probs[["Xb_Ya"]],
              row = row_number()
            ) %>%
            ungroup() %>%
            select(row, Xa, Xb, Ya, Yb) %>%
            pivot_longer(cols = -row, names_to = "agent", values_to = "prob") %>%
            group_by(agent) %>%
            summarise(p_model = mean(prob), .groups = "drop")
        } else if (global_exp == 3) {
          df_alt <- df_state %>%
            rowwise() %>%
            mutate(
              pair_probs = list(softmax(c(
                Xa_Yb = min(Xa_R3, Yb_R3),
                Xb_Ya = min(Xb_R3, Ya_R3)
              ))),
              Xa = pair_probs[["Xa_Yb"]],
              Yb = pair_probs[["Xa_Yb"]],
              Xb = pair_probs[["Xb_Ya"]],
              Ya = pair_probs[["Xb_Ya"]],
              row = row_number()
            ) %>%
            ungroup() %>%
            select(row, Xa, Xb, Ya, Yb) %>%
            pivot_longer(cols = -row, names_to = "agent", values_to = "prob") %>%
            group_by(agent) %>%
            summarise(p_model = mean(prob), .groups = "drop")
          
        }} else if (model == "Learning") {
          df_alt <- df_state %>%
            rowwise() %>%
            mutate(
              gain_Xa = max(Xa_R3 / difficulty_levels[1], Xa_R3 / difficulty_levels[2]),
              gain_Ya = max(Ya_R3 / difficulty_levels[1], Ya_R3 / difficulty_levels[2]),
              gain_Xb = max(Xb_R3 / difficulty_levels[1], Xb_R3 / difficulty_levels[2]),
              gain_Yb = max(Yb_R3 / difficulty_levels[1], Yb_R3 / difficulty_levels[2]),
              # dim A:
              prob_xa = as.numeric(softmax(c(Xa = gain_Xa, Ya = gain_Ya), beta = beta_sub)["Xa"]),
              prob_ya = as.numeric(softmax(c(Xa = gain_Xa, Ya = gain_Ya), beta = beta_sub)["Ya"]),
              # dim B:
              prob_xb = as.numeric(softmax(c(Xb = gain_Xb, Yb = gain_Yb), beta = beta_sub)["Xb"]),
              prob_yb = as.numeric(softmax(c(Xb = gain_Xb, Yb = gain_Yb), beta = beta_sub)["Yb"])
            ) %>%
            ungroup() %>%
            summarise(Xa = mean(prob_xa), Ya = mean(prob_ya), Xb = mean(prob_xb), Yb = mean(prob_yb)) %>%
            pivot_longer(everything(), names_to = "agent", values_to = "p_model")
          
        } else if (model == "Exploitation") {
          df_alt <- df_state %>%
            rowwise() %>%
            mutate(
              # dim A:
              prob_xa  = as.numeric(softmax(c(Xa = Xa_R3, Ya = Ya_R3), beta = beta_sub)["Xa"]),
              prob_ya = as.numeric(softmax(c(Xa = Xa_R3, Ya = Ya_R3), beta = beta_sub)["Ya"]),
              # dim B:
              prob_xb  = as.numeric(softmax(c(Xb = Xb_R3, Yb = Yb_R3), beta = beta_sub)["Xb"]),
              prob_yb = as.numeric(softmax(c(Xb = Xb_R3, Yb = Yb_R3), beta = beta_sub)["Yb"])
            ) %>%
            ungroup() %>%
            summarise(Xa = mean(prob_xa), Ya = mean(prob_ya), Xb = mean(prob_xb), Yb = mean(prob_yb)) %>%
            pivot_longer(everything(), names_to = "agent", values_to = "p_model")
          
        } else if (model == "Equity") {
          df_alt <- df_state %>%
            rowwise() %>%
            mutate(
              # dim A:
              prob_xa  = as.numeric(softmax(-c(Xa = Xa_R3, Ya = Ya_R3), beta = beta_sub)["Xa"]),
              prob_ya = as.numeric(softmax(-c(Xa = Xa_R3, Ya = Ya_R3), beta = beta_sub)["Ya"]),
              # dim B
              prob_xb  = as.numeric(softmax(-c(Xb = Xb_R3, Yb = Yb_R3), beta = beta_sub)["Xb"]),
              prob_yb = as.numeric(softmax(-c(Xb = Xb_R3, Yb = Yb_R3), beta = beta_sub)["Yb"])
            ) %>%
            ungroup() %>%
            summarise(Xa = mean(prob_xa), Ya = mean(prob_ya), Xb = mean(prob_xb), Yb = mean(prob_yb)) %>%
            pivot_longer(everything(), names_to = "agent", values_to = "p_model")
          
        } else if (model == "Equality") {
          df_alt <- tibble(agent = c("Xa","Xb","Ya","Yb"), p_model = 0.5)
          
        } else if (model == "General") {
          df_alt <- df_state %>%
            rowwise() %>%
            mutate(
              # dim A
              prob_xa = as.numeric(softmax(c(Xa = Xb_R3 - Xa_R3, Ya = Yb_R3 - Ya_R3), beta = beta_sub)["Xa"]),
              prob_ya = as.numeric(softmax(c(Xa = Xb_R3 - Xa_R3, Ya = Yb_R3 - Ya_R3), beta = beta_sub)["Ya"]),
              # dim B
              prob_xb = as.numeric(softmax(c(Xb = Xa_R3 - Xb_R3, Yb = Ya_R3 - Yb_R3), beta = beta_sub)["Xb"]),
              prob_yb = as.numeric(softmax(c(Xb = Xa_R3 - Xb_R3, Yb = Ya_R3 - Yb_R3), beta = beta_sub)["Yb"])
            ) %>%
            ungroup() %>%
            summarise( Xa = mean(prob_xa), Ya = mean(prob_ya), Xb = mean(prob_xb), Yb = mean(prob_yb)) %>%
            pivot_longer(everything(), names_to = "agent", values_to = "p_model")
        }
      decision_rows[[length(decision_rows) + 1]] <-
        df_alt %>% mutate(subject = subject_id, model = model, competence_structure = scen, scenario = subc)
    }
  }
  decision_all_models_df <- bind_rows(decision_rows)
  
  # local save
  if (!file.exists(paste0("output/execution/exp", global_exp, "_lookup_exec.csv"))) {
    write.csv(decision_all_models_df, paste0("output/execution/exp", global_exp, "_lookup_exec.csv"), row.names = FALSE)
  }
  
  # execution data
  exp_data_decision <- dat %>%
    filter(exp == global_exp)
  
  failed_subjects <- exp_data_decision %>%
    filter(passed_attention == FALSE) %>%
    pull(subject) %>%
    unique()
  
  exp_data_decision <- exp_data_decision %>%
    filter(!subject %in% failed_subjects)
  
  exp_data_decision <- exp_data_decision %>%
    filter(!is.na(trial_id))
  
  decisions_exp <- exp_data_decision %>%
    filter(is.na(round) & !is.na(assign_dim_a) & !is.na(assign_dim_b)) %>%
    select(subject, competence_structure, scenario, assign_dim_a, assign_dim_b)
  
  decisions_exp <- decisions_exp %>%
    pivot_longer(cols = c(assign_dim_a, assign_dim_b), names_to = "dim", values_to = "agent") %>%
    mutate(agent = case_when(dim == "assign_dim_a" & agent == "X" ~ "Xa",
                             dim == "assign_dim_b" & agent == "X" ~ "Xb",
                             dim == "assign_dim_a" & agent == "Y" ~ "Ya",
                             dim == "assign_dim_b" & agent == "Y" ~ "Yb", 
                             TRUE ~ NA_character_)) %>%
    filter(!is.na(agent)) %>%
    select(subject, competence_structure, scenario, agent)
  
  # merge data with model predictions (keep only model predictions for agents whom participants chose)
  decisions_exp_expanded <- decision_all_models_df %>%
    semi_join(decisions_exp, by = c("subject", "competence_structure", "scenario", "agent"))
  
  # compute log likelihood
  log_lik <- decisions_exp_expanded %>%
    group_by(subject, model) %>%
    summarise(log_likelihood = sum(log(p_model)), .groups = "drop")
  
  log_lik <- log_lik %>%
    pivot_wider(names_from = model, values_from = log_likelihood)  %>%
    # remove subject col for running `run_bms.m`
    select(Planning, Exploitation, Learning, Equity, Equality, General)
  
  # local save (remove heading for running `run_bms.m`)
  if (!file.exists(paste0("output/execution/exp", global_exp, "_loglikelihood.csv.csv"))) {
    write.table(log_lik, paste0("output/execution/exp", global_exp, "_loglikelihood.csv"), sep = ",", col.names = FALSE, row.names = FALSE)
  }
}

model_fitting(global_exp = 1)
model_fitting(global_exp = 2)
model_fitting(global_exp = 3)
