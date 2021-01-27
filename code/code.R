# ------------------------------------------
# An introduction to the tidymodels package
# Bruna Wundervald, January 28th 2021
# ------------------------------------------

# Loading libraries and data -------
library(tidyverse)
library(tidymodels)

data(ames, package = "modeldata")
ames <- ames %>% mutate(Sale_Price = log10(Sale_Price))

ames %>%  
  ggplot(aes(x = Sale_Price)) +
  geom_density(fill = "#ff6767") +
  labs(x = "Target Variable", y = "Density") +
  theme_classic(18)

# Train and test sampling  -------
set.seed(2021)
data_split <- initial_split(ames, strata = "Sale_Price", prop = 0.8)
ames_train <- training(data_split)
ames_test  <- testing(data_split)

data_split 

# Model specification with parsnip  -------
# - 1. Create a model specification: the type of 
# model you want to run
# - 2. Set an engine: the package use to run this model
# - 3. Fit the model

model_setup <- rand_forest(
  mode = "regression", trees = 100)

rf_mod <- set_engine(model_setup, "ranger")

rf_fit <- fit(
  rf_mod, 
  Sale_Price ~ Longitude + Latitude,
  data = ames_train
)

rf_fit

# Switching engines:
rf_mod <- model_setup %>% set_engine("randomForest")

rf_fit <- fit(
  rf_mod, 
  Sale_Price ~ Longitude + Latitude,
  data = ames_train
)

rf_fit

# -> List of available models: https://www.tidymodels.org/find/parsnip/

# Predictions  -------
test_pred <- rf_fit %>% 
  predict(ames_test) %>% 
  bind_cols(ames_test)

rmse <- test_pred %>%  rmse(Sale_Price, .pred)

test_pred %>% 
  ggplot(aes(x = Sale_Price, y = .pred)) +
  geom_point(colour = "#ff6767", alpha = 1) +
  geom_abline() + 
  labs(x = "Target Variable", y = "Density", 
       title = 
         paste0("Test MSE: ", round(rmse$.estimate^2, 3))) +
  theme_classic(18) +
  coord_obs_pred()

# Feature engineering with recipes   -------

# 1. Create a `recipe()` to define the processing of the data, e.g.:
  #- Create new classes, clean missing data, transform variables, etc
# 2. Calculate that in the training set with the `prep()` function
# 3. Apply the pre-processig with the `bake()` and 
# `juice()` functions 

mod_rec <- recipe(
  Sale_Price ~ Longitude + Latitude + Neighborhood +
    Central_Air + Year_Built, 
  data = ames_train
) %>%
  # Factor levels that occur in <= 5% of data as "other"
  step_other(Neighborhood, threshold = 0.05) %>%
  # Create dummy variables for all factor variables
  step_dummy(all_nominal()) %>% 
  # Adds an interaction term
  step_interact(~ starts_with("Central_Air"):Year_Built) 

mod_rec
ames_rec <- prep(mod_rec, training = ames_train, verbose = TRUE)
juice(ames_rec)

# Fit the model with the new processed version of the data 
rf_mod <- rand_forest(
  mode = "regression", mtry = 5, trees = 500) %>% 
  set_engine("ranger", regularization.factor = 0.5)

rf_fit <- rf_mod %>% 
  fit(Sale_Price ~ ., data = juice(ames_rec)) 

rf_fit$fit

# Hyperparameter tuning with tune  -------
ctrl <- control_grid(save_pred = TRUE)

rf_mod <- rand_forest(mtry = tune()) %>%
  set_mode("regression") %>%
  set_engine("ranger", regularization.factor = tune())

rf_param <- parameters(rf_mod)
rf_param

set.seed(2021)
data_folds <- vfold_cv(data = juice(ames_rec), v = 5)
ranger_tune <-
  rf_mod %>%
  tune_grid(
    Sale_Price ~ ., 
    resamples = data_folds,
    grid = 10,
    control = ctrl
  )

# Plotting results 
autoplot(ranger_tune, metric = "rmse") + 
  geom_point(colour = "#ff6767", size = 3) +
  geom_hline(yintercept = 0.091, linetype = 'dotted') +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  theme_classic() +
  theme(legend.position = "top")

autoplot(ranger_tune, metric = "rsq") + 
  geom_point(colour = "#ff6767", size = 3) +
  geom_hline(yintercept = 0.73, linetype = 'dotted') +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  theme_classic() +
  theme(legend.position = "top")

# Extracting the best model
best_res <- select_best(ranger_tune, metric = "rmse")
final_rf_mod <- rf_mod <- 
  rand_forest(mtry = best_res$mtry) %>%
  set_mode("regression") %>%
  set_engine("ranger", 
             regularization.factor = best_res$regularization.factor) 

final_rf_fit <- final_rf_mod %>%
  fit(Sale_Price ~ ., data = juice(ames_rec))

test_bake <- bake(ames_rec, new_data = ames_test )
final_pred <- final_rf_fit %>% 
  predict(test_bake) %>% 
  bind_cols(ames_test)

final_rmse <- final_pred %>%  rmse(Sale_Price, .pred)

final_pred %>% 
  ggplot(aes(x = Sale_Price, y = .pred)) +
  geom_point(colour = "#ff6767", alpha = 1) +
  geom_abline() + 
  labs(x = "Target Variable", y = "Density", 
       title = 
         paste0("Test MSE: ", round(final_rmse$.estimate^2, 4))) +
  theme_classic(18) +
  coord_obs_pred()

#------------------------------------------------------
