# SmartRegress_App
A shiny app to simulate linear regression scenarios, evaluate classical model assumptions, and obtain real-time diagnostic outputs.

## SmartRegress: Interactive Regression Learning App: [Link](https://menashasenanayaka.shinyapps.io/smartregress_app/)

It is important to note that: 

* The dataset is dynamically generated or loaded in order to test classical linear regression scenarios (ex: Good Fit, Multicollinearity, Heteroscedasticity).
* The predictor variables ($X_1, X_2, X_3$) and target response ($Y$) are organized into distinct columns within the dataset.
* The dataset maintains sufficient sample size (adjustable between 50 to 500 data points) to perform robust OLS regression diagnostics.

Limitations:

* The app is designed primarily for educational simulation and standard Ordinary Least Squares (OLS) regression models.
* The Shiny app might not support generalized non-linear models (GAM) or complex time series lag structures directly.
* This app is not addressing situations where there is auto-correlated panel data or non-Gaussian error distributions outside standard diagnostics.

Guidelines:

* First, select a specific regression scenario from the sidebar controls (e.g., Overfitting, Outliers, Heteroscedasticity).
* The required diagnostic plot or table should be selected from the drop-down menu (e.g., Residual vs Fitted, Q-Q Plot, VIF Analysis).
* The users must specify the sample size ($N$) using the slider control to observe how sample scale affects parameter estimates and $p$-values.
* The outputs (R-squared metrics, diagnostic charts, estimated equation, ANOVA, and actionable guidance) will be displayed dynamically.

This shiny app will be helpful to students, researchers, and data science learners who want an interactive learning application to understand linear regression assumptions, diagnostic plots, and model interpretations. 


