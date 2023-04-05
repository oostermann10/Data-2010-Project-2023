# Importing Libraries
# ----------------------------------------------------------------------------------------
library(tidyverse)
set.seed(420) # so rpois function values are reproducible 
# ----------------------------------------------------------------------------------------

#Reading in the data
# ----------------------------------------------------------------------------------------
wcmatches = read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-29/wcmatches.csv')
worldcups = read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-29/worldcups.csv')
# ----------------------------------------------------------------------------------------





# number of total goals scored
# ----------------------------------------------------------------------------------------
goals = wcmatches %>% 
  mutate(total_goals = home_score + away_score) %>% 
  select(total_goals)

ggplot(data = goals, aes(x = total_goals)) +
  geom_bar() +
  labs(x = "total goals", 
       title = "Observed Amount of Total Goals in a Game from 1930 - 2018 in World Cup Matches") +
  geom_vline(xintercept = mean(goals$total_goals), colour = "red", linetype = "dotted", lwd = 0.75) + 
  scale_x_discrete(limits = c(0:max(goals$total_goals)))
#This looks like it follows a poisson distribution, how could we check this? 

# we know the expectation of a poisson is lambda
# calculate the lambda value, which is just the expectation
totals_lambda = mean(goals$total_goals) 

# the actual data points sampled from possion(lambda), there is 900 random observations
totals_points = as.data.frame(rpois(nrow(goals), lambda = totals_lambda))
names(totals_points) <- c("random_variables")

# now lets plot this random distribution that we simulated 
ggplot(data = totals_points, aes(x = random_variables)) +
  geom_bar() +
  labs(x = "random poisson variable", 
       title = "Randomly Simulated Poisson Distribution (Lambda = 2.831), 900 Observations (Total Goals for all Games)") + 
  geom_vline(xintercept = totals_lambda, colour = "red", linetype = "dotted", lwd = 0.75) +
  scale_x_discrete(limits = c(0:max(totals_points$random_variables)))


# how can we test if this is actually a poisson distribution
# using the ks.test

# Null hyp -> the two samples are from the same distribution
# if p-value > 0.05 then there is no significant difference in the two samples 
# at the 95% confidence level
ks.test(totals_points$random_variables, goals$total_goals)
# Because we know totals_points is sampled from a poisson distribution
# we can conclude that because p-value > 0.05 we fail to reject the null hypothesis
# thus the two samples are from the same distribution. 
# ----------------------------------------------------------------------------------------





# number of total goals scored by France
# ----------------------------------------------------------------------------------------
france_goals = wcmatches %>% 
  mutate(goals = ifelse(home_team == "France",home_score,away_score)) %>% 
  filter(home_team == "France" | away_team == "France") %>% 
  select(goals)

ggplot(data = france_goals, aes(x = goals)) +
  geom_bar() +
  labs(x = "France's Goals", 
       title = "Observed Amount of Total Goals Scored by France from 1930 - 2018 in World Cup Matches") +
  geom_vline(xintercept = mean(france_goals$goals), colour = "red", linetype = "dotted", lwd = 0.75) +
  scale_x_discrete(limits = c(0:max(france_goals$goals)))
  
#This looks like it follows a poisson distribution, how could we check this? 

# we know the expectation of a poisson is lambda
# calculate the lambda value, which is just the expectation
france_lambda = mean(france_goals$goals) 

# the actual data points sampled from possion(lambda), there is 66 random observations
france_points = as.data.frame(rpois(nrow(france_goals), lambda = france_lambda))
names(france_points) <- c("random_variables")

# now lets plot this random distribution that we simulated 
ggplot(data = france_points, aes(x = random_variables)) +
  geom_bar() +
  labs(x = "random poisson variable", 
       title = "Randomly Simulated Poisson Distribution (Lambda = 1.818), 66 Observations (France Total Goals)") + 
  geom_vline(xintercept = france_lambda, colour = "red", linetype = "dotted", lwd = 0.75) +
  scale_x_discrete(limits = c(0:max(france_points$random_variables)))


# how can we test if this is actually a poisson distribution
# using the ks.test

# Null hyp -> the two samples are from the same distribution
# if p-value > 0.05 then there is no significant difference in the two samples 
# at the 95% confidence level
ks.test(france_points$random_variables, france_goals$goals)
# Because we know totals_points is sampled from a poisson distribution
# we can conclude that because p-value > 0.05 we fail to reject the null hypothesis
# thus the two samples are from the same distribution. 
# ----------------------------------------------------------------------------------------





# difference in goals scored
# ----------------------------------------------------------------------------------------
diff_goals = wcmatches %>% 
  mutate(difference = abs(home_score - away_score)) %>% 
  mutate(diff_other_way = abs(away_score - home_score)) %>% 
  select(difference, diff_other_way)

# we take the absolute value of the difference as poission can only be positive values
# this accounts for the difference between home - away/ away - home
# lets just double check this using the identical function 

identical(diff_goals$difference, diff_goals$diff_other_way)
#we get true, so it does account for both cases

diff_goals = diff_goals %>% 
  select(difference)

ggplot(data = diff_goals, aes(x = difference)) +
  geom_bar() +
  labs(x = "Difference in Goals", 
       title = "Observed Amount of Difference in Goals Scored by Teams from 1930 - 2018 in World Cup Matches") +
  geom_vline(xintercept = mean(diff_goals$difference), colour = "red", linetype = "dotted", lwd = 0.75) +
  scale_x_discrete(limits = c(0:max(diff_goals$difference)))

#This looks like it follows a poisson distribution, how could we check this? 

# we know the expectation of a poisson is lambda
# calculate the lambda value, which is just the expectation
difference_lambda = mean(diff_goals$difference) 

# the actual data points sampled from possion(lambda), there is 900 random observations
difference_points = as.data.frame(rpois(nrow(diff_goals), lambda = difference_lambda))
names(difference_points) <- c("random_variables")

# now lets plot this random distribution that we simulated 
ggplot(data = difference_points, aes(x = random_variables)) +
  geom_bar() +
  labs(x = "random poisson variable", 
       title = "Randomly Simulated Poisson Distribution (Lambda = 1.5), 900 Observations (Difference in Goals)") + 
  geom_vline(xintercept = difference_lambda, colour = "red", linetype = "dotted", lwd = 0.75) +
  scale_x_discrete(limits = c(0:max(difference_points$random_variables)))


# how can we test if this is actually a poisson distribution
# using the ks.test

# Null hyp -> the two samples are from the same distribution
# if p-value > 0.05 then there is no significant difference in the two samples 
# at the 95% confidence level
ks.test(difference_points$random_variables, diff_goals$difference)
# default is 95%
# Because we know totals_points is sampled from a poisson distribution
# we can conclude that because p-value > 0.05 we fail to reject the null hypothesis
# thus the two samples are from the same distribution. 
# ----------------------------------------------------------------------------------------