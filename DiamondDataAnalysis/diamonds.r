
# load required libraries
library(ggplot2)
library(reshape2)
library(GGally)

#load diamond dataset
diamonds <- data(diamonds)

# Histogram of diamond prices, faceted by diamond cut
qplot(x = price, data = diamonds, binwidth = 10)+
  scale_x_continuous(limits = c(300,1000), breaks = seq(300,1000,50))+
  facet_wrap(~cut)


# Histogram of diamond prices, faceted by diamond color
qplot(x = price, data = diamonds, binwidth = 10)+
  scale_x_continuous(limits = c(300,1000), breaks = seq(300,1000,50))+
  facet_wrap(~color)

summary(diamonds)

a1 <- nrow(diamonds[diamonds$price < 500,])
a2 <- nrow(diamonds[diamonds$price < 250,])
a3 <- nrow(diamonds[diamonds$price >= 15000,])

# Histogram of diamond prices, faceted by diamond cut, with a different scale
qplot(x = price, data = diamonds) + facet_wrap(~cut, scales = "free_y")
str(diamonds)


# Histogram of diamond prices, per carat
qplot(x = price/carat, data = diamonds, binwidth = 1)+
  scale_x_log10()+
  facet_wrap(~cut, scales = "free_y")

  
qplot(x = price, data = diamonds, binwidth = 5)+
  scale_x_log10(limits = c(0,1000), breaks = seq(0,1000,50))+
  facet_wrap(~cut, scales = "free_y")

str(diamonds)

#boxplot of price per carat vs diamond cut
qplot(y = price/carat, x= cut, data=diamonds, geom = "boxplot")+
  scale_y_continuous(limits = c(0,5000), breaks = seq(0,5000,500))

# boxplot of price vs diamond clarity
qplot(y = price, x= clarity, data=diamonds, geom = "boxplot")+
  scale_y_continuous(limits = c(0,5000), breaks = seq(0,5000,500))

# boxplot of price vs diamond color
qplot(y = price, x= color, data=diamonds, geom = "boxplot")+
  scale_y_continuous(limits = c(0,5000), breaks = seq(0,5000,500))

#Finding interquartile ranges
IQR(subset(diamonds, price <1000)$price)
IQR(subset(diamonds, color == "D")$price)

IQR(subset(diamonds, color == "J")$price)

summary(subset(diamonds, color=="D")$price)
summary(subset(diamonds, color=="J")$price)

#histogram of diamond carat
qplot(x = carat, data = diamonds, binwidth = 0.01)+
  scale_x_continuous(limits = c(0,3), breaks = seq(0,3,0.1))+
  scale_y_continuous(limits = c(0,9000), breaks = seq(0,9000,1000))

# price vs length scatterplot
ggplot(aes(x = x, y = price), data = diamonds)+
  geom_point()

#correlation matrix between diamond price and lenghth, breadth, height resp
cor(x = diamonds$price, y = diamonds$x)

cor(x = diamonds$price, y = diamonds$y)

cor(x = diamonds$price, y = diamonds$z)

# scatterplot: depth vs price
ggplot(aes(x = depth, y = price), data = diamonds)+
  geom_point(alpha = 1/100)+
  scale_x_continuous(breaks = seq(50,80,2))

# check if a correlation exists between diamond price and depth
cor(x = diamonds$price, y = diamonds$depth)

# scatterplot: carat vs price
ggplot(aes(x = carat, y = price), data = diamonds)+
  geom_point()

# calculate volume of diamond
diamonds$volume = diamonds$x * diamonds$y * diamonds$z

# scatterplot of volume vs price
ggplot(aes(x = volume, y = price), data = diamonds)+
  geom_point()+
  scale_x_continuous(limits = c(0,500), breaks = seq(0,500,50))

# create a filter to include diamonds with volume not equal to 0 and less than 800
diamonds_cleaned <- diamonds[diamonds$volume != 0 & diamonds$volume < 800, ]
str(diamonds_cleaned)

# calculate correlation between the price and volume, using filtered dataset
# avoid errors due to outliers and zeros
cor(diamonds_cleaned$price, diamonds_cleaned$volume)

# plot volume vs price applying the filter
ggplot(aes(x = volume, y = price), 
       data = subset(diamonds, subset = diamonds$volume != 0 & diamonds$volume < 800))+
  geom_point()+
  scale_x_continuous(limits = c(0,500), breaks = seq(0,500,50))+
  geom_smooth()

#detach plyr before attaching dplyr to avoid conflict
detach("package:plyr", unload = TRUE)
library(dplyr)

# group diamond by clarity; create new dataset with mean and median price,
# Minimum and maximum in the group, number of diamonds in each group
clarity_groups <- group_by(diamonds, clarity)
diamondsByClarity <- summarise(clarity_groups,
                               mean_price = mean(diamonds$price),
                               median_price = median(diamonds$price),
                               min_price = min(diamonds$price),
                               max_price = max(diamonds$price),
                               n = n())
# sort by clarity
diamondsByClarity <- arrange(diamondsByClarity, clarity)


# group diamonds by clarity and find mean price of each group
diamonds_by_clarity <- group_by(diamonds, clarity)
diamonds_mp_by_clarity <- summarise(diamonds_by_clarity, mean_price = mean(price))
diamonds_mp_by_clarity$clarity <- as.factor(diamonds_mp_by_clarity$clarity)

# plot clarity vs mean price using new dataset
p1 <- ggplot(aes(x = clarity, y = mean_price), data = diamonds_mp_by_clarity)+
  geom_point()
  

# group diamonds by color and find mean price of each group
diamonds_by_color <- group_by(diamonds, color)
diamonds_mp_by_color <- summarise(diamonds_by_color, mean_price = mean(price))
diamonds_mp_by_color$color <- as.factor(diamonds_mp_by_color$color)

# plot color vs mean price using new dataset
p2 <- ggplot(aes(x = color, y = mean_price), data = diamonds_mp_by_color)+
  geom_point()

# load required library and display both plots together
library(gridExtra)
grid.arrange(p1,p2, ncol=1)


# histogram of diamond prices
qplot(x = price, data = diamonds,binwidth = 10, geom='bar', color = cut)+
  facet_wrap(~color)

ggplot(aes(x = price, y = table), data = diamonds)+
  geom_point(aes(color = cut))+
  scale_y_continuous(limits = c(50,80), breaks = seq(50,80,2))

diamonds$volume = diamonds$x * diamonds$y * diamonds$z

ggplot(aes(x = price , y = volume), data = diamonds)+
  geom_point(aes(color = clarity))+
  scale_color_brewer(type = 'div')+
  scale_y_log10()

ggplot(aes(x = cut, y = price/carat), data = diamonds)+
  geom_point(aes(color = color))+
  facet_wrap(~ clarity)+
  scale_color_brewer(type = 'div')

#scatterplot of carat vs price
# xlim and ylim help in trimming 1% of the data
ggplot(aes(x = carat, y = price), data = diamonds)+
  geom_point(aes(color = 'brown'))+
  xlim(0, quantile(diamonds$carat, 0.99))+
  ylim(0, quantile(diamonds$price,0.99))


# create correlation matrix of all columns
# set seed and sample data
set.seed(77777)
diamonds_sample <- diamonds[sample(1:length(diamonds$price),10000),]

ggpairs(diamonds_sample, wrap = c(shape = I('.'), outlier.shape = I('.')))

# histograms of price, one with a normal scale and one with log scale
library(gridExtra)

plot1 <- qplot(x = price, data = diamonds, binwidth = 5) + 
  ggtitle('Price')

plot2 <- qplot(x = price, data = diamonds) +
  ggtitle('Price (log10)')+
  scale_x_log10()

grid.arrange(plot1,plot2)


# scatterplot with half transparency, cuberoot function and jitte to avoid overplotting

#cuberoot function
library(scales)
library(memisc)
cuberoot_trans <- function(){ 
  trans_new('cuberoot',
  transform =  function(x) {x^(1/3)},
  inverse = function(x) {x ^ 3} )}
       

ggplot(aes(carat, price), data = diamonds) + 
  geom_point(alpha = 0.5, size = 0.75, position = 'jitter') + 
  scale_x_continuous(trans = cuberoot_trans(), limits = c(0.2, 3),
                     breaks = c(0.2, 0.5, 1, 2, 3)) + 
  scale_y_continuous(trans = log10_trans(), limits = c(350, 15000),
                     breaks = c(350, 1000, 5000, 10000, 15000)) +
  ggtitle('Price (log10) by Cube-Root of Carat')


# adjust plot to color points by clarity
library(RColorBrewer)

ggplot(aes(x = carat, y = price), data = diamonds) + 
  geom_point(aes(color = clarity), alpha = 0.5, size = 1, position = 'jitter') +
  scale_color_brewer(type = 'div',
                     guide = guide_legend(title = 'Clarity', reverse = T,
                                          override.aes = list(alpha = 1, size = 2))) +  
  scale_x_continuous(trans = cuberoot_trans(), limits = c(0.2, 3),
                     breaks = c(0.2, 0.5, 1, 2, 3)) + 
  scale_y_continuous(trans = log10_trans(), limits = c(350, 15000),
                     breaks = c(350, 1000, 5000, 10000, 15000)) +
  ggtitle('Price (log10) by Cube-Root of Cut and Clarity')

# adjusting plot to color by cut
ggplot(aes(x = carat, y = price), data = diamonds) + 
  geom_point(aes(color = cut), alpha = 0.5, size = 1, position = 'jitter') +
  scale_color_brewer(type = 'div',
                     guide = guide_legend(title = 'Cut', reverse = T,
                                          override.aes = list(alpha = 1, size = 2))) +  
  scale_x_continuous(trans = cuberoot_trans(), limits = c(0.2, 3),
                     breaks = c(0.2, 0.5, 1, 2, 3)) + 
  scale_y_continuous(trans = log10_trans(), limits = c(350, 15000),
                     breaks = c(350, 1000, 5000, 10000, 15000)) +
  ggtitle('Price (log10) by Cube-Root of Carat and Cut')

# adjusting plot to color by diamond color
ggplot(aes(x = carat, y = price), data = diamonds) + 
  geom_point(aes(color = color), alpha = 0.5, size = 1, position = 'jitter') +
  scale_color_brewer(type = 'div',
                     guide = guide_legend(title = 'Color', 
                                          override.aes = list(alpha = 1, size = 2))) +  
  scale_x_continuous(trans = cuberoot_trans(), limits = c(0.2, 3),
                     breaks = c(0.2, 0.5, 1, 2, 3)) + 
  scale_y_continuous(trans = log10_trans(), limits = c(350, 15000),
                     breaks = c(350, 1000, 5000, 10000, 15000)) +
  ggtitle('Price (log10) by Cube-Root of Carat and Color')


# Building a linear model
m1 <- lm(I(log(price)) ~ I(carat^(1/3)), data = diamonds)
m2 <- update(m1, ~. + carat)
m3 <- update(m2, ~. + cut)
m4 <- update(m3, ~. + clarity)
m5 <- update(m4, ~. + color)

mtable(m1,m2,m3,m4,m5)              #requires package memisc



# using a better diamond data set
library(RCurl)
library(bitops)

diamondsURL <- getBinaryURL('https://github.com/SolomonMg/diamonds-data/blob/master/BigDiamonds.Rda')
load(rawConnection(diamondsURL))

# Extended diamond dataset
load('BigDiamonds.Rda')

m1 <- lm(I(log(price)) ~ I(carat^(1/3)), data = diamondsbig)
m2 <- update(m1, ~. + carat)
m3 <- update(m2, ~. + cut)
m4 <- update(m3, ~. + clarity)
m5 <- update(m4, ~. + color)

mtable(m1,m2,m3,m4,m5)

# predict based on model built (m5)
#create sample data to test model

thisDiamond <- data.frame(carat = 1.0, cut = 'V.Good', color = 'I', clarity = 'VS1')

modelEstimate <- predict(m5, newdata = thisDiamond, interval = 'prediction', level = 0.95)

exp(modelEstimate)
