library(ggplot2) #must load the ggplot package first
data(diamonds) #loads the diamonds data set since it comes with the ggplot package
dim(diamonds)   #printing the dimensions of the dataset
str(diamonds)   #displaying the internal structure of the dataset & type of variables
summary(diamonds)


#Creating a histogram of the price of diamonds.
#the distribution is long tailed
p1 <- ggplot(data= diamonds, aes(x=price)) +
  geom_histogram(binwidth = 500, color = "black", fill = "SkyBlue") +
  scale_x_continuous(breaks = seq(0,20000,1000), limits =c(0,20000))

#now we are using a scaling layers
p2 <- ggplot(data= diamonds, aes(x=price)) +
  geom_histogram(color = "black", fill = "SkyBlue") +
  xlab("log10(price)") +
  scale_x_log10() 
p3 <- ggplot(data= diamonds, aes(x=price)) +
  geom_histogram(color = "black", fill = "SkyBlue") +
  xlab("sqrt(price)") +
  scale_x_sqrt()

library(gridExtra)
grid.arrange(p1, p2, p3, ncol=1)


#How many diamonds cost less than 250 dollars?
length(diamonds$price[diamonds$price <250])
#How many diamonds cost more than or equal 15000 dollars?
length(diamonds$price[diamonds$price >= 15000]) 


#Creating a histogram of the price of diamonds, classified by cut
ggplot(data = diamonds, aes(x=price)) +
  geom_histogram(binwidth = 500, color = "black", fill = "SeaGreen") +
  facet_wrap(cut ~ .)


#Creating a boxplot of the price of diamonds by cut
ggplot(data = diamonds, aes(x=cut, y=price)) +
  geom_boxplot() +
  ggtitle("Price distribution of diamonds by cut") 

by(diamonds$price, diamonds$cut, summary)

#Creating a boxplot of the price of diamonds by clarity
ggplot(data = diamonds, aes(x=clarity, y=price)) +
  geom_boxplot() +
  ggtitle("Price distribution of diamonds by clarity") 

by(diamonds$price, diamonds$clarity, summary)


#Creating a boxplot of the price of diamonds by color
ggplot(data = diamonds, aes(x=color, y=price)) +
  geom_boxplot() +
  ggtitle("Price distribution of diamonds by color")

by(diamonds$price, diamonds$color, summary)

#Creating a boxplot of the price of diamonds per Carat by Color
ggplot(data = diamonds, aes(x=color, y=price/carat)) +
  geom_boxplot(aes(color = color)) +       
  ggtitle("Price per Carat distribution of diamonds by color")


#finding the Interguartile range of best and worest diamonds identified from diamond colour, from D (best) to J (worst)
IQR(subset(diamonds, color == "D")$price)  
IQR(subset(diamonds, color == "J")$price)


#Creating a carat frequency polygon
ggplot(data = diamonds, aes(x=carat)) +
  geom_freqpoly( binwidth = 0.1) +  
  ggtitle("carat frequency polygon") +
  scale_x_continuous(breaks = seq(0,2.5,0.1), limits =c(0,2.5))


#Which diamond carat has been sold more than 2000 times?
library(dplyr)
Sold_Carats <- diamonds %>% group_by(carat) %>% summarise(count = n())
High_sold_Carats <- subset(y, count > 2000)


#creating a scatterplot of price vs x
ggplot(data=diamonds, aes(x, price)) +
  geom_point() +
  ggtitle("scatterplot of price vs x") +
  xlim(3,9) +
  geom_smooth(color="blue")

#Finding the correlations between the price and x, y, and z
cor(diamonds$price, diamonds$x)    #they are correlated
cor(diamonds$price, diamonds$y)    #they are correlated
cor(diamonds$price, diamonds$z)    #they are correlated

#Creating a scatterplot of price vs depth
ggplot(data=diamonds, aes(x=depth, y=price))+
  ggtitle("scatterplot of price vs depth") +
  geom_point(alpha=1/100) +     
  scale_x_continuous(breaks = seq(50,74,2))
#based on the scatterplot most diamonds are between values 58 and 64 of depth
#corelation between depth and price
cor(diamonds$price, diamonds$depth)    #they are not correlated


#Creating a scatterplot of price vs. volume (x * y * z).
diamonds$volume <- (diamonds$x * diamonds$y * diamonds$z) 
ggplot(data=subset(diamonds, 0 < volume & volume < 450), aes(volume, price)) +
  geom_point(alpha = 1/20) +
  ggtitle("scatterplot of price vs volume") +  
  xlim(0,450) +
  geom_smooth(method = "gam", color = "red")
#corelation between price and volume
with(subset(diamonds, 0 < volume & volume < 450), cor.test(price, volume))   #they are correlated


#Creating a summary dataframe "diamonds_by_clarity" that is grouped by clarity
library(dplyr)
clarity_groups <- group_by(diamonds, clarity)   
diamonds_by_clarity <- summarise(clarity_groups, count=n(), mean_price = mean(price), median_price = median(price),
                               max_price = max(price), min_price = min(price))

#Creating a summary dataframe "diamonds_by_color" that is grouped by color
color_groups <- group_by(diamonds, color)   
diamonds_by_color <- summarise(color_groups, count=n(), mean_price = mean(price), median_price = median(price),
                                 max_price = max(price), min_price = min(price))


#creating two bar plots of "diamonds_by_clarity" and "diamonds_by_color"
p1 <- ggplot(data = diamonds_by_clarity, aes(x= clarity, y=mean_price)) +
  geom_col(fill = "skyblue") +
  labs(title = "Mean Price by Clarity", x = "Clarity", y = "Mean Price") +
  theme_minimal()

p2 <- ggplot(data = diamonds_by_color, aes(x= color, y=mean_price)) +
  geom_col(fill = "lightgreen") +
  labs(title = "Mean Price by Color", x = "Color", y = "Mean Price") +
  theme_minimal()

grid.arrange(p1, p2, ncol=1)



#Histograms of price by cut and Color
ggplot(data = diamonds, aes(x= price, fill = cut))+  
  geom_histogram() +
  facet_wrap( ~ color) +
  ggtitle("Histograms of price by cut and Color") +
  scale_fill_brewer(type = 'qual')


#Creating a scatterplot of diamond price vs. table and color the points by the cut of the diamond.
ggplot(data= diamonds, aes(x=table, y=price, color=cut)) +
  geom_point()
#most ideal cut diamonds have a table value between 53 and 58
#most fair cut diamonds have a table value between 66 and 67
#corelation between price and table
cor(diamonds$price, diamonds$table)    #they are not correlated


#Creating a scatterplot of diamond log10 of price vs. volume (x * y * z) and color the points by the clarity of diamonds.
ggplot(data= diamonds, aes(x=volume, y=log10(price), color=clarity)) +
  geom_point() +
  scale_x_continuous(limits = c(0,600)) +
  scale_color_brewer(type = 'div')


#Creating a scatter plot of the price/carat ratio of diamonds vs. the cut by clarity.
ggplot(data= diamonds, aes(x=cut, y=(price/carat), color=color)) +
  geom_jitter() +
  facet_wrap( ~ clarity) +
  scale_color_brewer(type = 'div')


#Creating a scatterplot of price vs carat and limiting x-axis and y-axis to omit the top 1% of values.
filtered_data <- diamonds %>% filter(carat <= quantile(carat, 0.99), price <= quantile(price, 0.99))
ggplot(data = filtered_data, aes(x= carat, y = price)) +
  geom_point() +
  ggtitle("99% quantile of price vs 99% quantile of depth")
  

#Sampling 10,000 diamonds form the dataset and plot the ggpairs to create a scatterplot matrix
library(GGally)
set.seed(20022)
diamonds_samp <- diamonds[sample(1 : length(diamonds$price), 10000), ]
ggpairs(diamonds_samp,
        lower = list(continuous = wrap("points", shape = I('.'))),
        upper = list(combo = wrap("box", outlier.shape = I('.'))))

#What the Visualization Does:
#1) Diagonal: Shows density or histogram plots of individual variables (by default in ggpairs).
#2) Lower Triangle: Scatterplots of pairwise relationships between numeric variables.
#3) Upper Triangle: Boxplots showing distributions and variability for pairwise relationships.

#Creating two price histograms plots and in one output image.
p3 <- ggplot(data = diamonds, aes(x=price)) + 
  geom_histogram(binwidth = 500, color="PaleVioletRed", fill="LightPink") +
  ggtitle("Histogram of diamonds price")
p4 <- ggplot(data = diamonds, aes(x=log10(price))) + 
  geom_histogram(binwidth = 0.05, color = "CornflowerBlue", fill="LightSkyBlue") +
  ggtitle("Histogram of log 10 of diamonds price")
grid.arrange(p3, p4, ncol=1)


#creating a scatterplot of price vs carat.
ggplot(data=diamonds, aes(carat, price)) +
  geom_point() +
  xlim(0,3)
#The relationship between price and carat is not linear


#scatterplot transformation
#creating a scatter plot of price vs. carat

#creating a custom function to transform the carat variable to the cube root of carat
cuberoot_trans = function() trans_new("cuberoot", transform = function(x) x^(1/3), inverse = function(x)x^3)

library(RColorBrewer)
#Applying transformations on an axis' scale compresses larger values of a variable while retaining relative proportions, making the plot easier to interpret.
ggplot(data=diamonds, aes(x=carat, y=price, color = clarity)) +   #coloring the plot by clarity
  geom_point(alpha = 0.5, size = 3/4, position = "jitter") +
  scale_color_brewer(type = 'div', guide = guide_legend(title = 'Clarity', reverse = T, override.aes = list(alpha = 1, size = 2))) +
  scale_x_continuous(trans = cuberoot_trans(), limits = c(0.2, 3), breaks = c(0.2, 0.5, 1, 2, 3)) +               
  scale_y_continuous(trans = log10_trans(), limits = c(350, 15000), breaks = c(350, 1000, 5000, 10000, 15000)) +  
  ggtitle("price (on a log10 scale) vs. carat (on a cubic root scale) and clarity")
#Now the plot is almost linear, now we can model our data using a linear model
###Clarity has an effect on diamond price


#Adjusting the previous code to to color the points by cut.
#creating a custom function to transform the carat variable to the cube root of carat
ggplot(data=diamonds, aes(x=carat, y=price, color = cut)) +   #coloring the plot by cut
  geom_point(alpha = 0.5, size = 3/4, position = "jitter") +
  scale_color_brewer(type = 'div', guide = guide_legend(title = 'Cut', reverse = T, override.aes = list(alpha = 1, size = 2))) +
  scale_x_continuous(trans = cuberoot_trans(), limits = c(0.2, 3), breaks = c(0.2, 0.5, 1, 2, 3)) +               
  scale_y_continuous(trans = log10_trans(), limits = c(350, 15000), breaks = c(350, 1000, 5000, 10000, 15000)) +  
  ggtitle("price (on a log10 scale) vs. carat (on a cubic root scale) and cut")
###Cut does not have an effect on diamond price


#Adjusting the previous code to to color the points by color.
#creating a custom function to transform the carat variable to the cube root of carat
ggplot(data=diamonds, aes(x=carat, y=price, color = color)) +   #coloring the plot by color
  geom_point(alpha = 0.5, size = 3/4, position = "jitter") +
  scale_color_brewer(type = 'div', guide = guide_legend(title = 'Color', override.aes = list(alpha = 1, size = 2))) +
  scale_x_continuous(trans = cuberoot_trans(), limits = c(0.2, 3), breaks = c(0.2, 0.5, 1, 2, 3)) +               
  scale_y_continuous(trans = log10_trans(), limits = c(350, 15000), breaks = c(350, 1000, 5000, 10000, 15000)) +  
  ggtitle("price (on a log10 scale) vs. carat (on a cubic root scale) and color")
###Color has an effect on diamond price


#Bulding a linear model to predict the price of a diamond
#lm(y~x)   #y is the outcome variable #x is the explanatory variable
m1 <- lm(I(log(price)) ~ I(carat^(1/3)), data = diamonds)
m2 <- update(m1, ~ . + carat)
m3 <- update(m2, ~ . + clarity)    #Adds clarity as an additional predictor.
m4 <- update(m3, ~ . + color)      #Adds color as an additional predictor.
m5 <- update(m4, ~ . + cut)        #Adds cut as an additional predictor.
mtable(m1, m2, m3, m4, m5, sdigits = 3)

#Our model is: log(price)= 0.415 + 9.144*(carat^(1/3)) - 1.093*carat + ...*clarity + ...*color + ...*cut + error_term

#The price prediction
This_Diamond = data.frame(carat = 1.50, color = "I", cut = "Premium", clarity = "SI1")
Model_Estimate = predict(m5, newdata = This_Diamond, interval = "prediction", level = 0.95)
exp(Model_Estimate)

