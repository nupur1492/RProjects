install.packages(c("installr", "Rcpp"))
library(installr)
install.Rtools()

library(Rcpp)
#check if cpp works
evalCpp("1+1")

library(reshape2)
library(ggplot2)


data("diamonds")
str(diamonds)

#Use c++ to find mean of price for a specific color

cppFunction('float price_mean(NumericVector x){
            int len = x.size();
            float sum = 0.0;
            
            for(int i = 0; i< len; i++){
                sum = sum + x[i];
            }

            float avg = (float)sum/(float)len;
            return avg;
            }')

color = "I"
price_amt = as.vector(diamonds[diamonds$color == color, ]$price[1:1000])
price_mean(price_amt)
