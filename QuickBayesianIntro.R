#test_case

## https://www.sumsar.net/files/academia/user_2015_tutorial_bayesian_data_analysis_short_version.pdf

# install.packages("rjags")
# install.packages("coda")
# install.packages("ggmcmc", dependencies=TRUE)

library(tidyverse)
library(rjags)
library(coda)

## Lets attempt an approach where we mark 20 fish with tags and then resample 20 fish from the population to see the number of marks on them.  In our, real example we did this and came up with 5 fish

# original code -----------------------------------------------------------
# n_draw <- 100000
# 
# # Defining and drawing from the prior distribution
# n_fish <- sample(20:250, n_draw, replace = TRUE)
# 
# # Defining the generative model
# pick_fish <- function(n_fish) {
#   fish <- rep(0:1, c(n_fish - 20, 20))
#   sum(sample(fish, 20))
# }
# 
# # Simulating the data
# n_marked <- rep(NA, n_draw)
# for(i in 1:n_draw) {
#   n_marked[i] <- pick_fish(n_fish[i])
# }
# 
# # Filtering out those parameter values that didn't result in the
# # data that we actually observed
# post_fish <- n_fish[n_marked == 5]
# 
# hist(post_fish)


# using vectorization -----------------------------------------------------

## Set seed
set.seed(12345) 

## Mark recapture to estimate population size

n_draw <- 100000 # Notice this is a HUGE number


# Defining and drawing from the prior distribution
n_fish <- sample(20:250, n_draw, replace = TRUE)
sample_stor <- tibble(n_fish)

# Defining the generative model that describes the number of fish caught with a mark

pick_fish <- function(n_fish) {
  fish <- rep(0:1, c(n_fish - 20, 20))
  sum(sample(fish, 20))
}

## Run simulation using purrr (i.e., vectorization)
sample_stor |> 
  mutate(n_marked = map_dbl(n_fish, ~pick_fish(.x))) -> sample_stor2

## filter out those that DO NOT match our observed 5 marked fish

sample_stor2 |> 
  filter(n_marked == 5) |> 
  ggplot() +
  geom_histogram(aes(n_fish), binwidth = 5, color = "black")

sample_stor2 |> 
  filter(n_marked == 5) |> 
  summarise(mean = mean(n_fish))

sample_stor2 |> 
  filter(n_marked == 5) |> 
  summarise(med = median(n_fish))

## Now lets assume that fish become "shy" once they are marked (i.e., they become harder to capture again).

pick_fish_shy <- function(n_fish) {
  fish <- rep(0:1, c(n_fish - 20, 20))
  prob_pick <- ifelse(fish == 0, 1.0, 0.5)
  sum(sample(fish, 20, prob = prob_pick))
}

sample_stor2 |> 
  mutate(n_marked_shy = map_dbl(n_fish, ~pick_fish_shy(.x))) -> sample_stor3 

sample_stor3 |> 
  pivot_longer(n_marked:n_marked_shy, names_to = "type", values_to = "values") -> sample_stor3_long

sample_stor3_long |> 
  filter(values == 5) |> 
  ggplot() +
  geom_histogram(aes(n_fish, fill = type), alpha = 0.5, binwidth = 5, color = "black")

sample_stor3_long |> 
  filter(values == 5) |> 
  group_by(type) |> 
  summarise(mean_est = mean(n_fish))

## Modified model that accounts for the “expert” opinion of the fisherman
## We will use a different approach to generate the n_fish given the expert opinion

?rnbinom
sample_stor3$expert_opinion_fish <-  rnbinom(n_draw, mu = 200 - 20, size = 4) + 20 

head(sample_stor3)

sample_stor3 |> 
  mutate(n_marked_shy_expert = map_dbl(expert_opinion_fish, ~pick_fish_shy(.x))) -> sample_stor4

sample_stor4 |> 
  filter(n_marked_shy_expert == 5) |> 
  ggplot() +
  geom_histogram(aes(expert_opinion_fish), alpha = 0.5, binwidth = 5, color = "black")

sample_stor4 |> 
  filter(n_marked_shy_expert == 5) |> 
  summarise(mean_est = mean(expert_opinion_fish))

## Use jags to run the same approach

data_list <- list(
  n_picked = 20,
  n_unmarked = 20 - 5)

model_string <- "
model {
  n_fish_real   ~ dunif(0, 250)
  n_fish <- round(n_fish_real)
  n_unmarked ~ dhyper(n_fish - n_picked, n_picked, n_picked, 1)
}
"

jags_model1 <- jags.model(textConnection(model_string), data = data_list)

samples <- rjags::coda.samples(jags_model1, c("n_fish"), n.iter = 3000)
head(samples)

samples_df <- ggmcmc::ggs(samples)

ggmcmc::ggs_histogram(samples_df)
ggmcmc::ggs_density(samples_df)
ggmcmc::ggs_traceplot(samples_df)

mean(samples_df[,"value"] > 100)