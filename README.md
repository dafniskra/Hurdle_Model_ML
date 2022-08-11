# 1 - Modèle Hurdle paramétrique et Modèle Hurdle apprentissage statistique pour estimer la fréquence des accidents

Here you find the code to the article <html><img src="squelettes-dist/puce.gif" align="left">&nbsp;&nbsp;&nbsp;</html> "<i>Parametric and XGBoost Hurdle Model for estimating accident frequency</i>", (with <html><a href="http://samm.univ-paris1.fr/BARDET-Jean-Marc">JM. BARDET</a></html>), hal preprint (<html><a href="https://hal.archives-ouvertes.fr/hal-03739838/">link</a></html>). Some of the code are in R and some in Python.

To compare our Hurdle model with machine learning we have done 4 other models. 
#### parametric model 
- Poisson GLM
- Parametric Hurdle
#### models that come from the literature :
- decision tree
- Poisson boosting tree model

For the tree and boosting model, the codes come from the article: "Case Study: French Motor Third-Party Liability Claims" from A. Noll, R. Salzmann and M.V. Wüthrich, available from https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3164764.

# 2 - Code

For the data, we use FreMTPL2freq. We have 85% for training and 15% for testing.
The code are in R and Python. 

### R Code
Regression GLM.r
Regression Hurdle.r
Regressionn tree.r
Regression tree boosting.r

### Python Code 
Hurdle_ML.py
Poisson optimisation avec optuna 
Binarie optimisation avec optuna
