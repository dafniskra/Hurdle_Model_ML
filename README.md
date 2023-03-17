# 1 - Parametric and XGBoost Hurdle Model for Estimating Accident Frequency
This repository contains the code related to the article "Parametric and XGBoost Hurdle Model for Estimating Accident Frequency" (with JM. Bardet and J. Rynkiewicz) and the hal preprint (<html><a href="https://hal.science/hal-03739838/document">link</a></html>). The code is written in Python.

To compare our Hurdle model with machine learning, we have implemented four other models:

#### Parametric models:
- Poisson GLM
- Parametric Hurdle
#### Models from the literature:
- Decision tree
- Poisson boosting tree model
For the tree and boosting models, the codes are taken from the article "Case Study: French Motor Third-Party Liability Claims" by A. Noll, R. Salzmann, and M.V. Wüthrich, which is available at https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3164764.

# 2 - Codes
We use FreMTPL2freq for the data, and the codes are available in both R and Python. The code files are:

#### R Code:
Regression GLM.r
Regression Hurdle.r
Regression tree.r
Regression tree boosting.r

#### Python Code:
Hurdle_ML.py
Poisson optimization with optuna
Binary optimization with optuna
