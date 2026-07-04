# Training collaborators for effective division of labor

Data, code, and materials for: Tian, F., Gershman, S. J., & Xiang, Y. (in press). Training collaborators for effective division of labor. _Cognitive Science._

## Project Overview

By dividing labor, collaboration brings together the complementary strengths of individuals. Yet, division of labor benefits collaboration not only by assigning people to tasks that match their competences, but by allowing them to improve "on-the-job" through training and practice. Thus, optimal division of labor requires anticipating how each individual will develop and what roles they will eventually be best suited for. Existing research on collaboration, however, rarely considers this prospective dimension.  To address this gap, we studied how humans make training decisions, while manipulating the long-term consequences of those decisions. Across three experiments (_N_ = 600), participants trained two military defense teams to counter two types of attacks (land and air), where the long-term goals and the teams' relative competences varied. Participants made a sequence of training decisions before assigning teams to roles in the final battle. Overall, participants divided labor and trained collaborators by considering how each team's competence would develop, and whether they would eventually meet task demands. These patterns were best captured by a Planning model that trained collaborators based on the expected outcome at the time of deployment. The Planning model outperformed several heuristic alternatives that optimized training based on current competence, learning potential, fairness, or versatility. Together, these findings provide a first step toward understanding how training unlocks one of the central benefits of dividing labor—complementary competences. People do not simply match existing competences to tasks; rather, they actively cultivate individual competences to support future specialization.

## Links

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
