#libraries 

library(data.table)
library(ggplot2)
library(GGally)
library(gganimate)
library(transformr)
library(animation)

# Loading the data

obs_gender <- fread("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-05/jobs_gender.csv")
earnings_female <- fread("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-05/earnings_female.csv") 
employed_gender <- fread("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-05/employed_gender.csv") 

str(obs_gender)
summary(obs_gender)
plot(obs_gender)


# EDA 

plot(x = obs_gender$total_earnings_male, y = obs_gender$total_earnings_female)

# We can already see somewhat of a discrepancy but this may be because it is so general to all job positions.
# We will look at one grouped by position or better level

plot(x = obs_gender$year, y = obs_gender$total_workers)
plot(x = obs_gender$year, y = obs_gender$total_earnings)

# Looks like the number of workers and total earnings didn't change much over the four years
# Therefore this doesn't seem like an important factor to consider.

female <- obs_gender[, list(total_workers = sum(total_workers), percent_female = mean(percent_female),
                            total_earnings = mean(total_earnings), wage_percent_of_male = mean(wage_percent_of_male, na.rm = TRUE)),
                     by = 'minor_category']

theme_custom <- theme(
  text = element_text(family = "Palatino", size = 12),
  aspect.ratio = 2/3,
  legend.position = 'bottom'
)

ggplot( female, aes( x = minor_category, y = total_workers)) +
  geom_histogram( stat = 'identity') +
  theme_bw() + theme_custom + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = 'Number of total workers per industry', y = 'Total number of workers')

ggplot( female, aes( x = minor_category, y = percent_female)) +
  geom_histogram( stat = 'identity') +
  theme_bw() + theme_custom + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = 'Percentage of female workers per industry', y = 'Percentage of female workers')

ggplot( female, aes( x = minor_category, y = total_earnings)) +
  geom_histogram( stat = 'identity') +
  theme_bw() + theme_custom + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = 'Yearly earning per industry', y = 'Salary ($)')

ggplot( female, aes( x = minor_category, y = wage_percent_of_male)) +
  geom_histogram( stat = 'identity') +
  theme_bw() + theme_custom + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = 'Percentage of female wage per industry', y = 'Percentage of female wage')

# Second dataset

str(earnings_female)

unique(earnings_female$group)

# Looks like there is a totals category that I will explore

wage_gap <- earnings_female[group != "Total, 16 years and older", ]

ggplot(wage_gap, aes(x = Year, y = percent, color = group)) +
  geom_point() +
  geom_smooth(method = 'lm', se = TRUE) +
  theme_bw() + theme(legend.position = "bottom") +
  transition_states(group) +
  labs(title = '{closest_state}', y = 'Percent of female wages of men (%)')

       
# The makeup of the workforce

ggplot(data = employed_gender) +
  geom_line(aes(x = year, y = full_time_female), color = 'maroon2') +
  geom_line(aes(x = year, y = full_time_male), color = 'blue') +
  theme_bw() +
  transition_reveal(year) +
  labs(title = 'Change in percentage of Full-time work', y = "Percentage of full-time workers")

ggplot(data = employed_gender) +
  geom_line(aes(x = year, y = part_time_female), color = 'maroon2') +
  geom_line(aes(x = year, y = part_time_male), color = 'blue') +
  theme_bw() +
  transition_reveal(year) +
  labs(title = 'Change in percentage of part-time work', y = "Percentage of part-time workers")






