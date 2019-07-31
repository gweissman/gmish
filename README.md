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
titanic_df <- as.data.frame(Titanic)
# Build a model
m <- glm(as.numeric(Survived) - 1 ~ ., data = titanic_df[,1:4], 
                                      family = binomial, 
                                      weights = titanic_df$Freq)
# Get predictions for each group
preds <- predict(m, type = 'response')

# Evaluate performance of the model with the Scaled Brier Score
sbs(preds, as.numeric(titanic_df$Survived) - 1)

# Get confidence interval of estimated Scaled Brier Score
bs_ci(preds, as.numeric(titanic_df$Survived) - 1, metric = sbs)

# Make calibration plot for predictions
calib_plot(obs ~ model1, 
    data = data.frame(obs = as.numeric(titanic_df$Survived) - 1,
                      model1 = preds), cuts = 5)
```
