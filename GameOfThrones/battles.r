df_battles <- read.csv("battles.csv")
str(df_battles)

head(df_battles)

library(ggplot2)

#Which region had most battles?
qplot(x = region, data = df_battles,color = attacker_king)

#Which year had most battles?
df_battles$year <- as.factor(df_battles$year)
qplot(x = year, data = df_battles)
  
# Most battles initiated by king? what was the outcome
df_battles_clean <- df_battles[!is.na(df_battles$attacker_outcome),]
qplot(x = attacker_king, data = df_battles[!is.na(df_battles$attacker_outcome),])+
  facet_wrap(~attacker_outcome)

#Most battles defended
qplot(x = defender_king, data = df_battles)

#Which type of battle was most fought?
qplot(x = battle_type, data = df_battles)


#attacker size vs defender size; outcome of battle
df_battles_1 <- df_battles[!is.na(df_battles$attacker_size) 
                           & !is.na(df_battles$defender_size) 
                           & !is.na(df_battles$attacker_outcome),]

ggplot(aes(x = attacker_size, y = defender_size), data = df_battles_1)+
  geom_point(aes(color = attacker_outcome))



