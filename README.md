Daily energy usage is influenced by:
- Weather conditions
- Day of the week
- Seasonal effects
- Human activity patterns

The objective of this project is to:
- Explore the dataset and identify key patterns.
- Prepare features such as one-hot encoded weekdays and cleaned dates.
- Train several regression models:
a) Linear Regression
b) Random Forest (ranger)
c) XGBoost
- Calculate RMSE on the test dataset for each model.
- Determine the best model and save the minimum RMSE as selected_rmse.
- Plot predicted vs actual consumption over time.
- Assess similarity of prediction trends using correlation.
- Save the final decision as: trend_similarity = "Yes" or "No"

EDA was performed in EDA_power_consumption.R
