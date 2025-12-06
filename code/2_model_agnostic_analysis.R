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

############### Figure 2 ###############

# 1. calculate probability of choosing each option (Xa, Xb, Ya, Yb)
# training - data
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
         phase = "Training") %>% 
  arrange(subject, task_structure, competence_structure, dim)

# execution - data
execution_data <- dat %>%
  filter(!is.na(assign_dim_a), !is.na(assign_dim_b)) %>%
  mutate(pick_a = if_else(assign_dim_a == "X", "Xa", "Ya"),
         pick_b = if_else(assign_dim_b == "X", "Xb", "Yb")) %>%
  pivot_longer(c(pick_a, pick_b), values_to = "dim") %>%
  mutate(dim = factor(dim, levels = all_choices)) %>%
  count(subject, exp, competence_structure, dim) %>%
  group_by(subject, exp, competence_structure) %>%
  mutate(
    N = sum(n) / 2, # dividing 2 because each trial has two execution choices (one for land, one for air)
    prob = n / N
  ) %>%
  complete(dim, fill = list(n = 0, prob = 0)) %>% 
  ungroup() %>% 
  mutate(task_structure = factor(exp, levels = 1:3, 
                                 labels = c("Exp 1 (Dimension-Based)",
                                            "Exp 2 (Agent-Based)",
                                            "Exp 3 (Weakest-Link)")),
         competence_structure  = factor(competence_structure, levels = 1:3,
                                        labels = c("Competence Setup 1",
                                                   "Competence Setup 2",
                                                   "Competence Setup 3")),
         phase = "Execution") %>% 
  arrange(subject, task_structure, competence_structure, dim)

# training - Planning model
training_planning <- generate_training_predictions(dat, choice_levels = all_choices) %>% 
  filter(model == "Planning") %>%
  mutate(phase = "Training")

# execution - Planning model
execution_planning <- generate_execution_predictions(choice_levels = all_choices) %>% 
  filter(model == "Planning") %>%
  mutate(phase = "Execution")

# 2. plot:
combined_data <- bind_rows(training_data, execution_data)
combined_planning <- bind_rows(training_planning, execution_planning) %>%
  mutate(task_structure = factor(exp, levels = 1:3, 
                            labels = c("Exp 1 (Dimension-Based)",
                                       "Exp 2 (Agent-Based)",
                                       "Exp 3 (Weakest-Link)")),
    competence_structure = factor(competence_structure, levels = 1:3,
                                  labels = c("Competence Setup 1",
                                             "Competence Setup 2",
                                             "Competence Setup 3")),
    dim = factor(dim, levels = all_choices)) %>%
  arrange(subject, task_structure, competence_structure, phase, dim)

p_combined <- ggplot(combined_data, 
                     aes(x = dim, y = prob, fill = phase, group = phase)) +
  stat_summary(
    fun = mean,
    geom = "col",
    position  = position_dodge2(width = 0.65),
    color     = alpha("black", 0.8),
    width     = 0.65,
    linewidth = 0.3
  ) +
  stat_summary(
    fun.data = mean_cl_boot,
    geom = "linerange",
    position = position_dodge2(width = 0.65),
    colour = "black",
    linewidth = 0.2
  ) +
  stat_summary(
    data = combined_planning,
    mapping = aes(x = dim, y = prob, group = phase, shape = "Planning Model"),
    fun = mean,
    geom = "point",
    position = position_dodge2(width = 0.65),
    size  = 1.2,
    stroke = 0.4,
    # fill  = "white",
    colour = "black"
  ) +
  facet_grid2(rows = vars(task_structure),
              cols = vars(competence_structure),
              axes = "all",
              switch = "y"
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    breaks = c(0, 0.5, 1.0),
    limits = c(0, 1.05),
    expand = c(0, 0)
  )+
  scale_fill_manual(
    values = c("Training" = "white",
               "Execution" = "gray")) +
  scale_shape_manual(
    name = "",
    values = c("Planning Model" = 2)
  ) +
  labs(x = NULL,
       y = "Probability of choosing each option",
       fill = "Phase") +
  theme_classic(base_size = 12) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside",
    panel.spacing = unit(1, "lines"),
    strip.text.x       = element_text(size = 5),
    strip.text.y.left  = element_text(size = 3.4),
    axis.title.x       = element_text(size = 6),
    axis.title.y       = element_text(size = 6),
    axis.text          = element_text(size = 5),
    legend.title       = element_text(size = 6),
    legend.text        = element_text(size = 6),
    legend.position = "bottom",
    legend.key.size = unit(0.5, "lines"),
    legend.spacing.x = unit(0.2, "lines"),
    legend.spacing.y = unit(0.2, "lines"),
    legend.box.margin = margin(t = -8)) +
    guides(
      fill  = guide_legend(override.aes = list(shape = NA), order = 1),
      shape = guide_legend(order = 2)
    )

ggsave("./../figures/figure2_barplots.pdf",
       p_combined, width = 6, height = 3)

############### Execution probabilities ###############

# Competence set up 1:
# Exp 1 Red-land: 
s1e1_red_land <- execution_data %>% 
  filter(competence_structure == "Competence Setup 1", exp == 1, dim == "Xa") %>%
  summarize(p = mean(prob)) %>%
  pull(p)
round(s1e1_red_land, 2)

# Exp 1 Red-air: 
s1e1_red_air <- execution_data %>% 
  filter(competence_structure == "Competence Setup 1", exp == 1, dim == "Xb") %>%
  summarize(p = mean(prob)) %>%
  pull(p)
round(s1e1_red_air, 2)

# Exp 2 Red-air: 
s1e2_red_air <- execution_data %>% 
  filter(competence_structure == "Competence Setup 1", exp == 2, dim == "Xb") %>%
  summarize(p = mean(prob)) %>%
  pull(p)
round(s1e2_red_air, 2)

# Exp 2 Blue-land: 
s1e2_blue_land <- execution_data %>% 
  filter(competence_structure == "Competence Setup 1", exp == 2, dim == "Ya") %>%
  summarize(p = mean(prob)) %>%
  pull(p)
round(s1e2_blue_land, 2)

# Exp 3 Blue-air:
s1e3_blue_air <- execution_data %>% 
  filter(competence_structure == "Competence Setup 1", exp == 3, dim == "Yb") %>%
  summarize(p = mean(prob)) %>%
  pull(p)
round(s1e3_blue_air, 2)

# Exp 3 Red-land:
s1e3_red_land <- execution_data %>% 
  filter(competence_structure == "Competence Setup 1", exp == 3, dim == "Xa") %>%
  summarize(p = mean(prob)) %>%
  pull(p)
round(s1e3_red_land, 2)

# Competence set up 2:
# Exp 1 Red-air: 
s2e1_red_air <- execution_data %>% 
  filter(competence_structure == "Competence Setup 2", exp == 1, dim == "Xb") %>%
  summarize(p = mean(prob)) %>%
  pull(p)
round(s2e1_red_air, 2)

# Exp 2 Red-air: 
s2e2_red_air <- execution_data %>% 
  filter(competence_structure == "Competence Setup 2", exp == 2, dim == "Xb") %>%
  summarize(p = mean(prob)) %>%
  pull(p)
round(s2e2_red_air, 2)

# Exp 3 Red-air: 
s2e3_red_air <- execution_data %>% 
  filter(competence_structure == "Competence Setup 2", exp == 3, dim == "Xb") %>%
  summarize(p = mean(prob)) %>%
  pull(p)
round(s2e3_red_air, 2)

# Exp 1 Red-land: 
s2e1_red_land <- execution_data %>% 
  filter(competence_structure == "Competence Setup 2", exp == 1, dim == "Xa") %>%
  summarize(p = mean(prob)) %>%
  pull(p)
round(s2e1_red_land, 2)

# Exp 1 Blue-land: 
s2e1_blue_land <- execution_data %>% 
  filter(competence_structure == "Competence Setup 2", exp == 1, dim == "Ya") %>%
  summarize(p = mean(prob)) %>%
  pull(p)
round(s2e1_blue_land, 2)

# Exp 2 Blue-land: 
s2e2_blue_land <- execution_data %>% 
  filter(competence_structure == "Competence Setup 2", exp == 2, dim == "Ya") %>%
  summarize(p = mean(prob)) %>%
  pull(p)
round(s2e2_blue_land, 2)

# Exp 3 Blue-land: 
s2e3_blue_land <- execution_data %>% 
  filter(competence_structure == "Competence Setup 2", exp == 3, dim == "Ya") %>%
  summarize(p = mean(prob)) %>%
  pull(p)
round(s2e3_blue_land, 2)

# Competence set up 3:
# Exp 1 Red-land: 
s3e1_red_land <- execution_data %>% 
  filter(competence_structure == "Competence Setup 3", exp == 1, dim == "Xa") %>%
  summarize(p = mean(prob)) %>%
  pull(p)
round(s3e1_red_land, 2)

# Exp 2 Red-land: 
s3e2_red_land <- execution_data %>% 
  filter(competence_structure == "Competence Setup 3", exp == 2, dim == "Xa") %>%
  summarize(p = mean(prob)) %>%
  pull(p)
round(s3e2_red_land, 2)

# Exp 3 Red-land: 
s3e3_red_land <- execution_data %>% 
  filter(competence_structure == "Competence Setup 3", exp == 3, dim == "Xa") %>%
  summarize(p = mean(prob)) %>%
  pull(p)
round(s3e3_red_land, 2)

# Exp 1 Blue-air: 
s3e1_blue_air <- execution_data %>% 
  filter(competence_structure == "Competence Setup 3", exp == 1, dim == "Yb") %>%
  summarize(p = mean(prob)) %>%
  pull(p)
round(s3e1_blue_air, 2)

# Exp 2 Blue-air: 
s3e2_blue_air <- execution_data %>% 
  filter(competence_structure == "Competence Setup 3", exp == 2, dim == "Yb") %>%
  summarize(p = mean(prob)) %>%
  pull(p)
round(s3e2_blue_air, 2)

# Exp 3 Blue-air: 
s3e3_blue_air <- execution_data %>% 
  filter(competence_structure == "Competence Setup 3", exp == 3, dim == "Yb") %>%
  summarize(p = mean(prob)) %>%
  pull(p)
round(s3e3_blue_air, 2)

############### Regressions - execution ###############

# Competence set up 1: 
# Red-land exp 1 to 2
bmm1 <- brm(formula = prob ~ exp, 
            data = execution_data %>% filter(competence_structure == "Competence Setup 1" & dim == "Xa", exp %in% c(1,2)),
            file = "output/cache/execution/bmm1",
            seed = 1)
summary(bmm1)

# Blue-land exp 1 to 2
bmm2 <- brm(formula = prob ~ exp, 
            data = execution_data %>% filter(competence_structure == "Competence Setup 1" & dim == "Ya", exp %in% c(1,2)),
            file = "output/cache/execution/bmm2",
            seed = 1)
summary(bmm2)

# Blue-land exp 2 to 3
bmm3 <- brm(formula = prob ~ exp, 
            data = execution_data %>% filter(competence_structure == "Competence Setup 1" & dim == "Ya", exp %in% c(2,3)),
            file = "output/cache/execution/bmm3",
            seed = 1)
summary(bmm3)

# Red-air exp 2 to 3
bmm4 <- brm(formula = prob ~ exp, 
            data = execution_data %>% filter(competence_structure == "Competence Setup 1" & dim == "Xb", exp %in% c(2,3)),
            file = "output/cache/execution/bmm4",
            seed = 1)
summary(bmm4)

# Red-land exp 2 to 3
bmm5 <- brm(formula = prob ~ exp, 
            data = execution_data %>% filter(competence_structure == "Competence Setup 1" & dim == "Xa", exp %in% c(2,3)),
            file = "output/cache/execution/bmm5",
            seed = 1)
summary(bmm5)

# Blue-air exp 2 to 3
bmm6 <- brm(formula = prob ~ exp, 
            data = execution_data %>% filter(competence_structure == "Competence Setup 1" & dim == "Yb", exp %in% c(2,3)),
            file = "output/cache/execution/bmm6",
            seed = 1)
summary(bmm6)

# Competence set up 2:
# Red-land exp 1 to 2
bmm7 <- brm(formula = prob ~ exp, 
            data = execution_data %>% filter(competence_structure == "Competence Setup 2" & dim == "Xa", exp %in% c(1,2)),
            file = "output/cache/execution/bmm7",
            seed = 1)
summary(bmm7)

# Blue-land exp 1 to 2
bmm8 <- brm(formula = prob ~ exp, 
            data = execution_data %>% filter(competence_structure == "Competence Setup 2" & dim == "Ya", exp %in% c(1,2)),
            file = "output/cache/execution/bmm8",
            seed = 1)
summary(bmm8)

# Red-land exp 1 to 3
bmm9 <- brm(formula = prob ~ exp, 
            data = execution_data %>% filter(competence_structure == "Competence Setup 2" & dim == "Xa", exp %in% c(1,3)),
            file = "output/cache/execution/bmm9",
            seed = 1)
summary(bmm9)

# Blue-land exp 1 to 3
bmm10 <- brm(formula = prob ~ exp, 
            data = execution_data %>% filter(competence_structure == "Competence Setup 2" & dim == "Ya", exp %in% c(1,3)),
            file = "output/cache/execution/bmm10",
            seed = 1)
summary(bmm10)

############### Regressions - training ###############

# Competence set up 1: 
# Red-land exp 1 to 2
bmm1 <- brm(formula = prob ~ exp, 
            data = training_data %>% filter(competence_structure == "Competence Setup 1" & dim == "Xa", exp %in% c(1,2)),
            file = "output/cache/training/bmm1",
            seed = 1)
summary(bmm1)

# Blue-air: exp 2 to 3
bmm2 <- brm(formula = prob ~ exp, 
            data = training_data %>% filter(competence_structure == "Competence Setup 1" & dim == "Yb", exp %in% c(2,3)),
            file = "output/cache/training/bmm2",
            seed = 1)
summary(bmm2)

# Competence set up 2:
# Blue-land: exp 1 to 2
bmm3 <- brm(formula = prob ~ exp, 
            data = training_data %>% filter(competence_structure == "Competence Setup 2" & dim == "Ya", exp %in% c(1,2)),
            file = "output/cache/training/bmm3",
            seed = 1)
summary(bmm3)

# Blue-land: exp 2 to 3
bmm4 <- brm(formula = prob ~ exp, 
            data = training_data %>% filter(competence_structure == "Competence Setup 2" & dim == "Ya", exp %in% c(2,3)),
            file = "output/cache/training/bmm4",
            seed = 1)
summary(bmm4)

# Competence set up 3:
# Red-land exp 1 to 3:
bmm5 <- brm(formula = prob ~ exp, 
            data = training_data %>% filter(competence_structure == "Competence Setup 3" & dim == "Xa", exp %in% c(1,3)),
            file = "output/cache/training/bmm5",
            seed = 1)
summary(bmm5)

# Red-land exp 2 to 3
bmm6 <- brm(formula = prob ~ exp, 
            data = training_data %>% filter(competence_structure == "Competence Setup 3" & dim == "Xa", exp %in% c(2,3)),
            file = "output/cache/training/bmm6",
            seed = 1)
summary(bmm6)
