df_battles <- read.csv("battles.csv")
str(df_battles)

head(df_battles)

library(ggplot2)

#Which region had most battles?
ggplot(aes(x = region), data = df_battles,color = attacker_king)+
  geom_bar(aes(fill = factor(attacker_king)))

#Which year had most battles? Who was the attcker; who won?
df_battles$year <- as.factor(df_battles$year)
ggplot(aes(x = year), data = df_battles)+
  geom_bar(aes(fill = factor(attacker_king)))+
  facet_wrap(~attacker_outcome)
  
# Most battles initiated by king? what was the outcome
df_battles_clean <- df_battles[!is.na(df_battles$attacker_outcome),]
ggplot(aes(x = attacker_king), data = df_battles[!is.na(df_battles$attacker_outcome),])+
  geom_bar(aes(fill = battle_type))+
  facet_wrap(~attacker_outcome)

#Most battles defended; what was the outcome?
ggplot(aes(x = defender_king), data = df_battles)+
  geom_bar(aes(fill = attacker_outcome))

#Which type of battle was most fought at what location?
ggplot(aes(x = location), data = df_battles)+
  geom_bar(aes(fill = factor(battle_type)))


#attacker size vs defender size; outcome of battle
df_battles_1 <- df_battles[!is.na(df_battles$attacker_size) 
                           & !is.na(df_battles$defender_size) 
                           & !is.na(df_battles$attacker_outcome),]

ggplot(aes(x = attacker_size, y = defender_size), data = df_battles_1)+
  geom_point(aes(color = attacker_outcome))


#which commander was most successfull
ggplot(aes(x = attacker_size, y = attacker_commander), data = df_battles)+
  geom_point(stat = "identity", aes(fill = factor(attacker_king), shape = attacker_outcome))+
  scale_x_continuous(limits = c(0,22000), breaks = seq(0,22000,2000))


#learning plotly
library(plotly)

p1 <- plot_ly(df_battles, x = region)
p1

p2 <- plot_ly(df_battles, x = region, type = "bar")
p2

p3 <- plot_ly(df_battles, x = region, mode = "markers")
p3

p4 <- plot_ly(df_battles, x = region, mode = "markers", color = attacker_outcome)
p4
