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
m <- glm(as.numeric(Survived) - 1 ~ ., data = tt[,1:4], family = binomial, weights = tt$Freq)
# Get predictions for each group
preds <- predict(m, type = 'response')
# Evaluate performance of the model with the Scaled Brier Score
sbs(preds, as.numeric(tt$Survived) - 1)
```
