# Indicator success function
success_indicator <- function(effort, competence, difficulty) {
  return(effort * competence >= difficulty)
}

# Utility function
util <- function(competence, effort, difficulty, reward, alpha) {
  success <- success_indicator(effort, competence, difficulty)
  return(reward * success - alpha * effort)
}

# Max utility effort value
optimal_e <- function(competence, difficulty, reward, alpha, effort_vector = seq(0, 1, by = 0.01)) {
  utilities <- sapply(effort_vector, function(e) util(competence, e, difficulty, reward, alpha))
  return(effort_vector[which.max(utilities)])
}

# Changed competency
changed_comp <- function(original_comp, trained_comp, difficulty, reward, alpha_X, alpha_Y, history, effort_vector = seq(0, 1, by = 0.01)) {
  new_comp <- original_comp
  alpha <- ifelse(trained_comp %in% c("Xa", "Xb"), alpha_X, alpha_Y)
  effort <- optimal_e(original_comp[trained_comp], difficulty, reward, alpha, effort_vector)
  new_comp[trained_comp] <- original_comp[trained_comp] + effort
  new_history <- c(history, paste0(trained_comp, " (d=", difficulty, ", e=", effort, ")"))
  return(list(comp = new_comp, history = new_history, effort = effort))
}

# Run simulation
round_simulation <- function(total_rounds, starting_cond, difficulty_levels, reward, alpha_X, alpha_Y, effort_vector = seq(0, 1, by = 0.01)) {
  results <- list()
  results[[1]] <- list(list(
    comp = starting_cond, history = c(), trained = NA, effort = NA, difficulty = NA,
    comp_history = list(starting_cond), train_history = list()
  ))
  
  choices <- c("Xa", "Xb", "Ya", "Yb")
  
  for (current_round in 1:total_rounds) {
    new_results <- list()
    for (result in results[[current_round]]) {
      for (comp in choices) {
        for (difficulty in difficulty_levels) { 
          updated <- changed_comp(result$comp, comp, difficulty, reward, alpha_X, alpha_Y, result$history)
          
          train_decision <- list(trained = comp, dim = ifelse(comp %in% c("Xa", "Ya"), "a", "b"), effort = updated$effort, difficulty = difficulty)
          comp_history <- append(result$comp_history, list(updated$comp))
          train_history <- append(result$train_history, list(train_decision))
          
          new_results[[length(new_results) + 1]] <- c(updated, list(comp_history = comp_history, train_history = train_history))
        }
      }
    }
    results[[current_round + 1]] <- new_results
  }
  return(results)
}

# Logsumexp trick
logsumexp <- function(x) {
  c <- max(x)
  return(c + log(sum(exp(x - c))))
}

# Softmax function
softmax <- function(x, beta = 1) {
  z <- beta * x
  exp(z) / sum(exp(z))
}

# Path value function
path_value <- function(final_comp, condition) {
  if (condition == "Planning Condition 1") {
    return(max(final_comp["Xa"], final_comp["Ya"]) + max(final_comp["Xb"], final_comp["Yb"]))
  } else if (condition == "Planning Condition 2") {
    return(max(final_comp["Xa"] + final_comp["Yb"], final_comp["Xb"] + final_comp["Ya"]))
  } else if (condition == "Planning Condition 3") {
    return(max(min(final_comp["Xa"], final_comp["Yb"]), min(final_comp["Xb"], final_comp["Ya"])))
  }else if (condition == "Learning") {
    return(sum(final_comp))
  } else if (condition == "Exploitation") {
    total_X <- final_comp["Xa"] + final_comp["Xb"]
    total_Y <- final_comp["Ya"] + final_comp["Yb"]
    return(max(total_X, total_Y))
  } else if (condition == "Equity") {
    total_X <- final_comp["Xa"] + final_comp["Xb"]
    total_Y <- final_comp["Ya"] + final_comp["Yb"]
    return(-abs(total_X - total_Y))
  } else if (condition == "Equality") {
    return(0)
  } else if (condition == "General") {
    diff_sum <- abs(final_comp["Xa"] - final_comp["Xb"]) +
      abs(final_comp["Ya"] - final_comp["Yb"])
    return(-diff_sum)
  }
}

# Modified process_results function to return all paths with probabilities
process_results <- function(final_results, num_rounds, R, alphaX, alphaY, difficulty_levels, path_condition, beta) {
  expanded_table <- do.call(rbind, lapply(final_results, function(result) {
    comp_history <- result$comp_history
    train_history <- result$train_history
    competence_record <- list()
    training_record <- list()
    
    for (r in 0:num_rounds) {
      comp_snapshot <- comp_history[[r + 1]]
      competence_record[paste0("Xa_R", r)] <- comp_snapshot["Xa"]
      competence_record[paste0("Xb_R", r)] <- comp_snapshot["Xb"]
      competence_record[paste0("Ya_R", r)] <- comp_snapshot["Ya"]
      competence_record[paste0("Yb_R", r)] <- comp_snapshot["Yb"]
      
      if (r > 0) {
        train_snapshot <- train_history[[r]]
        training_record[paste0("trained_R", r)] <- train_snapshot$trained
        training_record[paste0("dim_R", r)] <- train_snapshot$dim
        training_record[paste0("e_R", r)] <- train_snapshot$effort
        training_record[paste0("d_R", r)] <- train_snapshot$difficulty
      }
    }
    
    row <- c(
      list(
        Reward = R,
        Alpha_X = alphaX,
        Alpha_Y = alphaY,
        Difficulty_Levels = paste(difficulty_levels, collapse = ", "),
        PathCondition = as.character(path_condition),
        PathValue = as.numeric(path_value(result$comp, path_condition)),
        History = paste(result$history, collapse = " -> ")
      ),
      competence_record,
      training_record
    )
    
    return(row)
  }))
  
  # Convert to dataframe and sort
  expanded_table <- as.data.frame(expanded_table)
  
  expanded_table$PathCondition <- as.character(expanded_table$PathCondition)
  
  expanded_table <- expanded_table %>%
    mutate(PathValue = as.numeric(PathValue)) %>%
    arrange(desc(PathValue))
  
  # Softmax probability per condition group with logsumexp trick
  expanded_table <- expanded_table %>%
    group_by(PathCondition) %>%
    mutate(
      beta_val = beta * PathValue,
      lse = logsumexp(beta_val),
      SoftmaxProb = exp(beta_val - lse)
    ) %>%
    ungroup() %>%
    select(-beta_val, -lse)
  
  return(expanded_table)
}

generate_training_predictions <- function(dat, choice_levels = c("Xa", "Ya", "Xb", "Yb")) {
  
  # Load lookup tables and best beta saved from model-fitting (`1_model_fitting.R`): 
  lookup1      <- read_csv("./../code/output/training/exp1_lookup.csv")
  best_beta1   <- read_csv("./../code/output/training/best_beta_exp1.csv") 
  lookup2      <- read_csv("./../code/output/training/exp2_lookup.csv")
  best_beta2   <- read_csv("./../code/output/training/best_beta_exp2.csv") 
  lookup3      <- read_csv("./../code/output/training/exp3_lookup.csv")
  best_beta3   <- read_csv("./../code/output/training/best_beta_exp3.csv") 
  
  lookups     <- list(lookup1, lookup2, lookup3)
  best_betas  <- list(best_beta1, best_beta2, best_beta3)
  
  model_prediction_list <- vector("list", length = 3)
  
  for (i in 1:3) {
    best_beta <- best_betas[[i]]
    lookup    <- lookups[[i]]
    
    # Join subject-specific best betas to all trajectories (subject x model x all trajectories x all scenarios)
    sim_base <- best_beta %>%
      inner_join(lookup, by = c("model","beta"), relationship = "many-to-many") %>%
      mutate(exp = i)
    
    # Extract choices from trajectory strings (e.g., "Xa10", "Yb2")
    sim_rounds <- sim_base %>%
      mutate(tokens = str_extract_all(trajectory, "[A-Z][a-z][0-9]{1,2}")) %>%
      unnest(tokens) %>%
      mutate(dim = str_extract(tokens, "^[A-Z][a-z]"))
    
    # Compute choice probabilities from weighted trajectory frequency
    group_keys <- c("model", "subject", "exp", "competence_structure", "dim")
    subj_prediction_i <- sim_rounds %>%
      count(across(group_keys), wt = prob, name = "weighted_prob") %>% 
      complete(dim = choice_levels, fill = list(weighted_prob = 0)) %>%
      group_by(across(setdiff(group_keys, "dim"))) %>%
      mutate(prob = weighted_prob / sum(weighted_prob)) %>%
      ungroup() %>% 
      mutate(dim = factor(dim, levels = choice_levels)) %>% 
      arrange(model, subject, exp, competence_structure, dim)
    
    # Store this exp's data
    model_prediction_list[[i]] <- subj_prediction_i
    
  }

  bind_rows(model_prediction_list)

}

generate_execution_predictions <- function(choice_levels = c("Xa", "Ya", "Xb", "Yb")) {
  
  exec_list <- vector("list", 3)
  
  for (exp_id in 1:3) {
    path <- sprintf("./../code/output/execution/exp%d_lookup_exec.csv", exp_id)
    df_exec <- read_csv(path) %>% 
      dplyr::rename(dim = agent)
    
    exec_list[[exp_id]] <- df_exec %>%
      group_by(subject, model, competence_structure, dim) %>%
      summarise(prob = mean(p_model), .groups = "drop") %>%
      mutate(
        exp = exp_id,
        dim = factor(dim, levels = choice_levels)
      ) %>%
      select(model, subject, exp, competence_structure, dim, prob)
  }
  
  bind_rows(exec_list)
}

plot_model_evidence <- function(df) {
  df %>% as.data.frame() %>%
    mutate(id = row_number()) %>% 
    pivot_longer(-id, names_to = "model", values_to = "evidence") %>%
    mutate(model = factor(model,
                          levels = model_levels,
                          ordered = TRUE)) %>%
    ggplot(aes(model, evidence, color = model)) +
    stat_summary(fun.data = 'mean_cl_normal', geom = 'errorbar', width = 0, color = 'black') +
    stat_summary(fun = 'mean', geom = 'point', size = 3, shape = 21, color = 'black', aes(fill = model)) +
    scale_fill_manual(name = NULL, values = model_palette) +
    theme_classic() +
    labs(title = NULL,
         x = NULL, y = "Model Evidence") +
    theme(legend.position = "none") +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(size = 18))
}

summarize_best <- function(df, model_levels) {
  n_subj <- nrow(df)
  
  apply(df[, model_levels], 1, function(x) model_levels[x == max(x)]) %>%
    unlist() %>%
    as_tibble() %>%
    count(value, name = "Count") %>%
    rename(Model = value) %>%
    complete(Model = model_levels, fill = list(Count = 0)) %>%
    mutate(
      Percent = Count / n_subj,
      SE      = sqrt(Percent * (1 - Percent) / n_subj) * 1.96,  # 95% Wald
      ymin    = pmax(0, Percent - SE),
      ymax    = pmin(1, Percent + SE)
    )
}

plot_model_share <- function(summary_tbl, model_levels, palette) {
  ggplot(summary_tbl,
         aes(x = factor(Model, levels = model_levels), y = Percent, fill = Model)) +
    geom_col(col = 'black') +
    geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0) +
    labs(x = "Model", y = "Percentage of Participants") +
    scale_y_continuous(labels = percent_format(accuracy = 1),
                       limits = c(0, 1)) +
    scale_fill_manual(values = palette) +
    theme_classic() +
    theme(legend.position = "none")
}
