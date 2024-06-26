
####################################
#
# Check journal ranking by deciles
#
####################################

rm(list = ls())

#load packages
library(here)
library(dplyr)

#load data
journals <- read.csv(here("database/scimagojr 2022  Subject Category - Law.csv"), sep=";")

#https://support.clarivate.com/ScientificandAcademicResearch/s/article/Journal-Citation-Reports-Quartile-rankings-and-other-metrics?language=en_US
#Z = (X / Y)
#X = Rank
#Y = Number journals in category

#calculate Y
Y <- nrow(journals)

#calculate Z
journals <- journals %>%
  mutate(Z = Rank / Y)

#calcualte quartiles
journals <- journals %>%
  mutate(Quartile.comp = ifelse(Z <= 0.25, 'Q1', NA),
         Quartile.comp = ifelse(Z > 0.25 & Z <= 0.5, 'Q2', Quartile.comp),
         Quartile.comp = ifelse(Z > 0.5 & Z <= 0.75, 'Q3', Quartile.comp),
         Quartile.comp = ifelse(Z > 0.75, 'Q4', Quartile.comp))

#calculate deciles
journals <- journals %>%
  mutate(Decile.comp = ifelse(Z <= 0.1, 'D1', NA),
         Decile.comp = ifelse(Z > 0.1 & Z <= 0.2, 'D2', Decile.comp),
         Decile.comp = ifelse(Z > 0.2 & Z <= 0.3, 'D3', Decile.comp),
         Decile.comp = ifelse(Z > 0.3 & Z <= 0.4, 'D4', Decile.comp),
         Decile.comp = ifelse(Z > 0.4 & Z <= 0.5, 'D5', Decile.comp),
         Decile.comp = ifelse(Z > 0.5 & Z <= 0.6, 'D6', Decile.comp),
         Decile.comp = ifelse(Z > 0.6 & Z <= 0.7, 'D7', Decile.comp),
         Decile.comp = ifelse(Z > 0.7 & Z <= 0.8, 'D8', Decile.comp),
         Decile.comp = ifelse(Z > 0.8 & Z <= 0.9, 'D9', Decile.comp),
         Decile.comp = ifelse(Z > 0.9, 'D10', Decile.comp))

#check first decile journals
D1.journals <- journals %>%
  filter(Decile.comp == "D1") %>%
  pull(Title)

#save first decile titles
write.table(D1.journals, file = "output/D1.journals.txt")
write.csv(D1.journals, here("output/D1_journals.csv"))

#check first quartile journals
Q1.journals <- journals %>%
  filter(SJR.Quartile == "Q1") %>%
  pull(Title)

#save first quartile titles
write.table(Q1.journals, file = "output/Q1.journals.txt")
write.csv(Q1.journals, here("output/Q1_journals.csv"))
