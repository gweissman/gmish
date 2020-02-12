# gmish

A collection of miscellaneous functions for data wrangling and predictive modeling. 

# Install gmish

```{r}
install.packages('devtools')
devtools::install_github("gweissman/gmish")
```

# Example usage

Predict survival aboard the Titanic.

```{r}
# Prepare the data
dd <- ISLR::Credit
head(dd)
# Build a model
m <- glm(Balance == 0 ~ Income + Rating + Cards + Age + Gender, 
          data = dd,   family = binomial)
                                    
# Get predictions for each group
preds1 <- predict(m, type = 'response')

# Evaluate performance of the model with the Scaled Brier Score
sbrier(preds1, dd$Balance == 0)

# Get confidence interval of estimated Scaled Brier Score
bs_ci(preds1, dd$Balance == 0, metric = sbrier)

# Make calibration plot for predictions
results <- data.frame(obs = dd$Balance == 0,
                      model1 = preds1)
                      
calib_plot(obs ~ model1, data = results, cuts = 10)
                      
# Try a second model
library(randomForest)
m_rf <- randomForest(as.factor(Balance == 0) ~ Income + Rating + Cards + Age + Gender, 
          data = dd)
results$model2 <- predict(m_rf, type = 'prob')[,2]

# Examine calibration together
calib_plot(obs ~ model1 + model2, data = results)

# Estimate the difference in performance between the two models
boot_diff(results$model1, results$model2, results$obs, metric = brier)
```

