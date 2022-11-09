# gmish

A collection of miscellaneous functions for data wrangling and predictive modeling. 

# Install gmish

```{r}
install.packages('remotes')
remotes::install_github("gweissman/gmish")
```

# Example usage

Try to predict the penguin species. 

```{r}
# Load the library
library(gmish)

# Prepare the data
dd <- palmerpenguins::penguins
dd <- dd[complete.cases(dd),]
head(dd)

# Build a model
m <- glm(species == 'Adelie' ~ island + bill_length_mm + flipper_length_mm + body_mass_g, 
          data = dd,   family = binomial)
                                    
# Get predictions for each group
preds1 <- predict(m, type = 'response')

# Evaluate performance of the model with the Scaled Brier Score
sbrier(preds1, dd$species == 'Adelie')

# Get confidence interval of estimated Scaled Brier Score
bs_ci(preds1, dd$species == 'Adelie', metric = sbrier)

# Make calibration plot for predictions
results <- data.frame(obs = dd$species == 'Adelie',
                      model1 = preds1)
                      
calib_plot(obs ~ model1, data = results, cuts = 4)
                      
# Try a second model
library(randomForest)
m_rf <- randomForest(as.factor(species == 'Adelie') ~ island + bill_length_mm + flipper_length_mm + body_mass_g, 
          data = dd)
results$model2 <- predict(m_rf, type = 'prob')[,2]

# Examine calibration together
calib_plot(obs ~ model1 + model2, data = results, cuts = 4)

# Estimate the difference in performance between the two models
boot_diff(results$model1, results$model2, results$obs, metric = sbrier)
```
# Guidance for contributors

The `gmish` team welcomes code contributions in the form of pull requests. Guiding principles for accepting pull requests include identifying contributions that:

1. Adds functionality that would be of broad interest without breaking existing interfaces.
2. Corrects any errors (syntax, math, or other types of errors) in the current code.
3. Maintains clean, concise, easy-to-read, and well documented source good.
