library(tidyverse)
library(scales)
library(ggh4x)   
library(brms)
source('helper.R')

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

all_choices <- c("Xa", "Ya", "Xb", "Yb")

############### Supplement: Plotting 7x3 Data vs Model Prediction (by competence setup) ###############

# 1. generate model predictions:
predictions_all <- generate_training_predictions(dat, choice_levels = all_choices)

# 2. calculate probabilities:

# data
training_data <- dat %>%
  filter(!is.na(trained)) %>%
  mutate(dim = factor(trained, levels = all_choices)) %>%
  count(subject, exp, competence_structure, dim) %>%
  group_by(subject, exp, competence_structure) %>%
  mutate(
    N = sum(n),
    prob = n / N
  ) %>%
  complete(dim, fill = list(n = 0, prob = 0)) %>% 
  ungroup() %>%
  mutate(task_structure = factor(exp, levels = 1:3, 
                                 labels = c("Exp 1 (Dimension-Based)",
                                            "Exp 2 (Agent-Based)",
                                            "Exp 3 (Weakest-Link)")),
         competence_structure = factor(competence_structure, levels = 1:3, 
                                       labels = c("Competence Setup 1",
                                                  "Competence Setup 2",
                                                  "Competence Setup 3")),
         panel_row = "Data") %>% 
  arrange(subject, task_structure, competence_structure, dim)

# model predictions
training_model <- predictions_all %>%
  mutate(dim = factor(dim, levels = all_choices)) %>%
  mutate(task_structure = factor(exp, levels = 1:3, 
                                 labels = c("Exp 1 (Dimension-Based)",
                                            "Exp 2 (Agent-Based)",
                                            "Exp 3 (Weakest-Link)")),
         competence_structure = factor(competence_structure, levels = 1:3, 
                                       labels = c("Competence Setup 1",
                                                  "Competence Setup 2",
                                                  "Competence Setup 3")),
         panel_row = model) %>% 
  arrange(subject, task_structure, competence_structure, dim)

# 3. plot:
row_order <- c("Data", "Planning", "Exploitation", "Learning", "Equity", "Equality", "General")

# combine data and model predictions
combo_training <- bind_rows(training_data, training_model) %>% 
  mutate(panel_row = factor(panel_row, levels = row_order))

plot_probability_by_competence_setup <- function(plotting_structure) {
  
  plot_training <- combo_training %>% 
    filter(competence_structure == paste0("Competence Setup ", plotting_structure)) %>% 
    ggplot(aes(x = dim, y = prob, fill = dim)) +
    stat_summary(fun = mean, geom = "col") +
    stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0, color = "black") +
    facet_grid(rows = vars(panel_row), cols = vars(task_structure), labeller = label_value) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                       limits = c(0, 1.1), expand = c(0, 0)) +
    scale_fill_manual(values = c("Xa"="#63B4D1","Xb"="#7699D4","Ya"="#9448BC","Yb"="#480355")) +
    labs(
      x = "Trained Dimension",
      y = "Proportion of Participants") +
    theme_classic() +
    theme(
      strip.background.y = element_blank(),
      strip.background.x = element_blank(),
      axis.text.y = element_text(size = 4),
      axis.text.x = element_text(size = 4),
      axis.ticks.x = element_blank(),
      strip.background = element_blank(),
      strip.placement = "outside",
      panel.spacing = unit(1, "lines"),
      strip.text.x = element_text(size = 3.5),
      strip.text.y.right  = element_text(size = 5),
      axis.title.x = element_text(size = 4),
      axis.title.y = element_text(size = 4),
      axis.text = element_text(size = 8),
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 8),
      legend.position = "none",
      legend.key.size = unit(0.5, "lines"),
      legend.spacing.x = unit(0.5, "lines"),
      legend.spacing.y = unit(0.5, "lines"),
      legend.box.margin = margin(t = -8))
  
  # save plot
  ggsave(filename = paste0("./../figures/supplement/comparison_s", plotting_structure, ".pdf"),
         plot = plot_training, width = 3, height = 4.5, onefile = TRUE)
  
}

plot_probability_by_competence_setup(plotting_structure = 1)
plot_probability_by_competence_setup(plotting_structure = 2)
plot_probability_by_competence_setup(plotting_structure = 3)

############### Supplement: pre-registered regressions ###############

bmm1 <- brm(formula = prob ~ exp, 
            data = training_data %>% filter(competence_structure == "Competence Setup 1" & dim == 'Xa'),
            file = 'output/cache/supplement/bmm1',
            seed = 1)
summary(bmm1)

bmm2 <- brm(formula = prob ~ exp, 
            data = training_data %>% filter(competence_structure == "Competence Setup 1" & dim == 'Yb'),
            file = 'output/cache/supplement/bmm2',
            seed = 1)
summary(bmm2)

bmm3 <- brm(formula = prob ~ exp, 
            data = training_data %>% filter(competence_structure == "Competence Setup 2" & dim == 'Xb'),
            file = 'output/cache/supplement/bmm3',
            seed = 1)
summary(bmm3)

bmm4 <- brm(formula = prob ~ exp, 
            data = training_data %>% filter(competence_structure == "Competence Setup 2" & dim == 'Ya'),
            file = 'output/cache/supplement/bmm4',
            seed = 1)
summary(bmm4)

bmm5 <- brm(formula = prob ~ exp, 
            data = training_data %>% filter(competence_structure == "Competence Setup 3" & dim == 'Xa'),
            file = 'output/cache/supplement/bmm5',
            seed = 1)
summary(bmm5)

bmm6 <- brm(formula = prob ~ exp, 
            data = training_data %>% filter(competence_structure == "Competence Setup 3" & dim == 'Yb'),
            file = 'output/cache/supplement/bmm6',
            seed = 1)
summary(bmm6)
