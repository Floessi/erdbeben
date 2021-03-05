library("pscl")
library("caret")
library("dplyr")
library("purrr")
library("tidyr")


# Create grouping based on whether reponse is 0.
set.seed(2020)
crossVal<-function(full_data, model1,k){
full_data<- full_data %>%
  mutate(trigger = ifelse(triggerCountTh  == 0, TRUE, FALSE))

# Create stratified folds based on these groups
data_kfold <- createFolds(full_data[["trigger"]], k = k, list = TRUE)

# Use the list of row-indices to create data.frames for each fold
data_kdf <- purrr::map_dfr(data_kfold, ~full_data[.x, ],
                              .id = "Fold")

# Check proportion of zeros in each fold:

(check_balance <- data_kdf %>%
    group_by(Fold) %>%
    summarise(proportion_zero = mean(triggerCountTh == 0)))



(original_balance <- full_data %>%
    summarise(proportion_zero = mean(triggerCountTh == 0)))



# One approach using nested data.frames (tidymodels)
data_kdf <- data_kdf %>%
  group_by(Fold) %>%
  nest()


model1<-function(df) zeroinfl(triggerCountTh~ mag+heatFlow+crustalThick+mantleThick+depth+dip+rake, data=df, dist="negbin")

data_kdf <- data_kdf %>%
  mutate(mod_fit = map(data, model1)) %>% # Fit model
  mutate(mod_summary = map(mod_fit, ~summary(.x))) %>% # Store summary
  mutate(mod_observed = map(data, "triggerCountTh")) %>% # Extract observed responses
  mutate(mod_predicted = map(mod_fit, ~predict(.x))) %>% # Extract predicted responses
  mutate(rmse = map2_dbl(mod_observed, mod_predicted, ~RMSE(.x, .y))) # Calculate RMSE for each fold


  return(data_kdf)
}


# For each fold, extract summary for example
model1<-function(df) zeroinfl(triggerCountTh~ mag+heatFlow+crustalThick+mantleThick+depth+dip+rake, data=df, dist="negbin")
b<-crossVal(full_data,model1,5)

b[["rmse"]]
b[["mod_summary"]]
