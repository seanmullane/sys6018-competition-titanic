# This is an attempt at the Titanic competition on Kaggle

setwd("~/DSI/classes/fall2018/SYS6018/kaggle/sys6018-competition-titanic/")

library(tidyverse)
library(ggplot2)

#------------------------------------------------------------------------------------
# Read in data
#------------------------------------------------------------------------------------

d.gender_raw <- read_csv("data/gender_submission.csv")
d.train_raw <- read_csv("data/train.csv")
d.test_raw <- read_csv("data/test.csv")

#------------------------------------------------------------------------------------
# Exploratory analysis
#------------------------------------------------------------------------------------

sapply(d.train_raw, function(x) {sum(is.na(x))/length(x)})
sapply(d.test_raw, function(x) {sum(is.na(x))/length(x)})
# 20% of age missing, 77% of cabin missing, tiny amt of fare is missing in test, tiny amt of embarked is missing in train, nothing else missing
# cabin missingness ~20% for 1st class px, nearly 100% for 2nd, 3rd

sapply(d.train_raw[d.train_raw$Pclass == 1,], function(x) {sum(is.na(x))/length(x)})
sapply(d.test_raw[d.test_raw$Pclass == 1,], function(x) {sum(is.na(x))/length(x)})

sapply(d.train_raw, function(x) {is.numeric(x)})
sapply(d.test_raw, function(x) {})

table(d.train_raw$Embarked)
table(d.test_raw$Embarked)

hist(d.train_raw$Fare)
ggplot(data = d.train_raw) +
  geom_histogram(mapping = aes(x = Fare, group = Pclass)) +
  facet_wrap(~ Pclass)

table(d.train_raw$Parch)
table(d.train_raw[d.train_raw$Pclass == 1,]$Parch)
table(d.train_raw[d.train_raw$Pclass == 2,]$Parch)
table(d.train_raw[d.train_raw$Pclass == 3,]$Parch)

table(d.all[d.train_raw$Pclass == 1,]$Deck)
table(d.all[d.train_raw$Pclass == 2,]$Deck)
table(d.all[d.train_raw$Pclass == 3,]$Deck)

#------------------------------------------------------------------------------------
# Munging
#------------------------------------------------------------------------------------

d.all <- bind_rows(d.train_raw, d.test_raw)
d.all <- arrange(d.all, PassengerId)
train_ind <- which(!is.na(d.all$Survived))
Survived <- d.all[train_ind,]$Survived
d.all$Survived <- NULL
d.all$PassengerId <- NULL

# Features based on names
d.all <- d.all %>%
  mutate(nlen = str_length(Name),
         Name_short = str_extract(Name, pattern = '[\\w ,\\.\'\"-]+'),
         Name_paren = str_extract(Name, pattern = '\\([\\w ,\\.\'\"-]+\\)'),
         nslen = str_length(Name_short),
         nplen = str_length(Name_paren),
         nscnt = str_count(Name_short, pattern = '\\w+'),
         npcnt = str_count(Name_paren, pattern = '\\w+')) %>%
  select(-Name, -Name_short, -Name_paren)

d.all <- d.all %>%
  mutate(Deck = substr(Cabin, 1,1),
         Cabnum = str_extract(Cabin, '\\d+'),
         Cabodd = (as.integer(Cabnum) %% 2),
         Cabcnt = str_count(Cabin, '\\w+')) %>%
  select(-Cabin)

# Fix this
#d.all <- d.all %>%
#  separate(col = Ticket, into = c("first","second", "third"), sep = " ", remove = F) %>%
#  mutate(Tixstr = ifelse(!is.na(third), ifelse(!is.na(second), first, NA), NA),
#         Tixnum = ifelse(is.na(third), ifelse(is.na(second), first, second), third))

# Fill in NAs with imputed values

mean_Age <- mean(as.integer(d.all[train_ind,]$Age), na.rm = T)
mean_Cabnum <- mean(as.integer(d.all[train_ind,]$Cabnum), na.rm = T)
mean_Fare <- mean(as.integer(d.all[train_ind,]$Fare), na.rm = T)

d.all <- d.all %>%
  mutate(Deck = ifelse(is.na(Deck), ifelse(Pclass == 1, "C", ifelse(Pclass == 2, "F", "G")), Deck),
         nplen = ifelse(is.na(nplen), 0, nplen),
         npcnt = ifelse(is.na(npcnt), 0, npcnt),
         Cabnum = ifelse(is.na(Cabnum), mean_Cabnum, Cabnum),
         Cabcnt = ifelse(is.na(Cabcnt), 1, Cabcnt),
         Cabodd = ifelse(is.na(Cabodd), -1, Cabodd),
         Age = ifelse(is.na(Age), mean_Age, Age),
         Fare = ifelse(is.na(Fare), mean_Fare, Fare),
         Farediv = Fare/Cabcnt) %>%
  select(-Ticket)

d.all <- d.all %>%
  mutate_at(vars(Cabnum), funs(as.numeric)) %>%
  mutate_at(vars(Pclass), funs(as.character))

#------------------------------------------------------------------------------------
# Modeling
#------------------------------------------------------------------------------------

prev <- table(d.train_raw$Survived)[2]/sum(table(d.train_raw$Survived))

d.train <- d.all[train_ind,]
d.test <- d.all[base::setdiff(seq(1,nrow(d.all)), train_ind),]
d.train$Survived <- Survived

model.lm <- glm(formula = Survived ~ ., family = "binomial", data = d.train)

preds <- predict.glm(object = model.lm, newdata = d.test, type = "response")
threshold <- quantile(preds, probs = 1-prev)

preds_bin <- ifelse(preds < threshold, 0, 1)

#------------------------------------------------------------------------------------
# Analyse Model
#------------------------------------------------------------------------------------


  