# multi_dim_training

Data, code, and materials for: Tian, F., Gershman, S. J., & Xiang, Y. (2025). _Training collaborators for effective division of labor._

## Project Overview

Effective collaboration requires not just dividing labor according to current strengths but also anticipating how individuals’ competences will develop with training. Across three experiments (N=600), participants trained two defense teams to counter different types of attack before assigning them to roles. By manipulating the relative competences of collaborators and the long-term collaborative goal, we examined how people make these training and execution decisions. Overall, participants adapted their choices to task demands and to expected future competence growth. A Planning model that forecasts long-term outcomes best explained their behavior, outperforming heuristic alternatives based on current competence, learning potential, fairness, or versatility. These results suggest that actively cultivate individual competences to support future specialization rather than simply matching existing abilities to tasks.

## Links

Preprint: https://osf.io/preprints/psyarxiv/dxy7u_v1  
Experiment 1: https://gershmanlab.com/experiments/yang/multi_dim_competence/exp1.html  
Experiment 2: https://gershmanlab.com/experiments/yang/multi_dim_competence/exp2.html  
Experiment 3: https://gershmanlab.com/experiments/yang/multi_dim_competence/exp3.html  
Preregistration: https://aspredicted.org/w5hp37.pdf

---

## Code

Located in the `code` folder:

- `1_model_fitting.R` — Fits all six computational models to participants' training data. 
- `2_model_agnostic_analysis.R` — Generates Figure 2 and runs regression analyses on behavioral data.
- `3_model_comparison.R` — Performs random-effects Bayesian model selection and generates Figure 3.
- `4_supplement.R` — Produces all supplemental figures.
- `run_bms.m` — Runs random-effects Bayesian model selection using `bms.m` from the `mfit` package (Gershman, 2015): https://github.com/sjgershm/mfit
- `helper.R` — Helper functions for model simulation and visualization.
- `output/` — Contains intermediate output files generated during simulation and analyses. These files allow the scripts to skip time-consuming computations on subsequent runs.

---

## Data

Located in the `data` folder:

- `data.csv` — Combined dataset from Experiments 1, 2, and 3.
  - `exp`: Experiment index (corresponding to different task structures).
  - `scenario`: Scenario index within each competence setup.
  - `competence_structure`: Competence setup index (1–3), indicating the starting relative competences.
  - `round`: Training round; 0 denotes the starting state.
  - `Xa`, `Xb`, `Ya`, `Yb`: Competences of each team (X or Y) in each dimension (a or b).
  - `trained`: Selected team during the training round.
  - `weight_selected`: Selected training difficulty (2 or 10).
  - `assign_dim_a`, `assign_dim_b`: Role assignments during the execution phase.
  - `passed_attention`: TRUE or FALSE, recorded in the final row of each participant's data.

---

## Experiment Materials

Located in the `experiments` folder:

- `exp1.html`, `exp2.html`, and `exp3.html` — Task scripts for Experiments 1–3.
- `consent.html` and `img/` — Consent forms and experiment images.
- `save_data.php` — Script for writing data to server.
- Experiments were built using jsPsych v7.3.4, available at:  
  https://github.com/jspsych/jsPsych/releases/tag/jspsych%407.3.4  
- Note: To run the experiments locally, the consent form needs to be commented out (i.e., comment out `timeline.push(consent);`) and jsPsych library files need to be added to this folder.
