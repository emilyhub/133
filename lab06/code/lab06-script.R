# ===================================================================
# Title: Cleaning Data
# Description:
#   This script performs cleaning tasks and transformations on 
#   various columns of the raw data file.
# Input(s): data file 'raw-data.csv'
# Output(s): data file 'clean-data.csv'
# Author: Emily Lan
# Date: 3-1-2018
# ===================================================================
library(readr)
library(dplyr)
library(ggplot2)
dat <- read_csv(file = '../data/nba2017-players.csv')
warriors <- arrange(filter(dat, team == "GSW"), salary)
write.csv(warriors, file = "../data/warriors.csv", row.names = FALSE)
lakers <- arrange(filter(dat, team == "GSW"), desc(experience))
write.csv(warriors, file = "../data/lakers.csv", row.names = FALSE)
sink(file = "../output/datastructure.txt")
str(dat)
sink()
sink(file = "../output/summary-warriors.txt")
summary(warriors)
sink()
sink(file = "../output/summary-lakers.txt")
summary(lakers)
sink()
png(filename = "../images/heightweight1.png")
plot(dat$height, dat$weight, xlab = "Height", ylab = "Weight")
dev.off()
png(filename = "../images/heightweight_highres.png", res = 80)
plot(dat$height, dat$weight, xlab = "Height", ylab = "Weight")
dev.off()
png(filename = "../images/age1.png", width = 600, height = 400)
plot(dat$age)
dev.off()
pdf(file = "../images/age.pdf", width = 7, height = 5)
plot(dat$age)
dev.off()
gg_pts_salary <- ggplot(dat, aes(x=points, y=salary)) + geom_point()
ggsave("../images/points_salary.pdf", plot=gg_pts_salary, width = 7, height = 5)
gg_ht_wt_positions <- ggplot(dat, aes(x=height, y=weight)) + facet_grid(position ~ .) + geom_point()
ggsave("../images/height_weight_by_position.pdf", plot=gg_ht_wt_positions, width = 6, height = 4)
dat %>%
  filter(team == "LAL")
dat %>%
  filter(position == "PG") %>%
  select(player, salary)
dat %>%
  filter(experience > 10) %>%
  select(player, age, team)
dat %>%
  filter(age == 20, experience < 10) %>%
  select(player, team, height, weight) %>%
  tail(n = 5)
gsw_mpg <-
  dat %>%
  filter(team == "GSW") %>%
  mutate(min_per_game = minutes/games) %>%
  arrange(desc(min_per_game))
dat %>%
  group_by(team) %>%
  summarise(triple = mean(points3)) %>%
  arrange(triple) %>%
  slice(1:5)
dat %>%
  filter(position == "PF" & experience >= 5 & experience <= 10) %>%
  summarise(meanage = mean(age), sdage = sd(age))