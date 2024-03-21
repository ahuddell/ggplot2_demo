
#install packages if you need them
#install.packages("datasets")
#install.packages("tidyverse")



#load the packages
library(datasets) # Load this for the iris dataset
library(tidyverse) #Load this for ggplot


# load and view the data --------------------------------------------------

#load the dataset "iris" 
data(iris)

#two ways to take a quick look at iris 
glimpse(iris)
head(iris)

#this is how it prints out as a dataframe
iris


#let's transform it into a tidyverse "tibble" and print it again
iris<-as_tibble(iris)
iris
#see how it prints out more politely?


# scatter plot ------------------------------------------------------------

#first, look at the basic map of the data
ggplot(data=iris, aes(x=Petal.Length, y=Petal.Width)) 


#now add the points
ggplot(data=iris, aes(x=Petal.Length, y=Petal.Width)) +
  geom_point()


#now add the black white theme to make it prettier
ggplot(data=iris, aes(x=Petal.Length, y=Petal.Width)) +
  geom_point()+
  theme_bw()

#now let's reformat the X and y axes and add units
ggplot(data=iris, aes(x=Petal.Length, y=Petal.Width)) +
  geom_point()+
  theme_bw() +
  xlab('petal length (cm)') +
  ylab('petal width (cm)')



#how do we add a line with the linear model fit?
#let's look at the documentation for this function
?geom_smooth
#method we want to set to "lm" for linear model
#by default, it will plot "se" 95% confidence interval around the model estimate

#plot the points with a linear regression line and confidence intervals and add 
#a plot title
ggplot(data=iris, aes(x=Petal.Length, y=Petal.Width)) +
  geom_point()+
  theme_bw() +
  xlab('petal length (cm)') +
  ylab('petal width (cm)') +
  geom_smooth(method="lm") +
  ggtitle('linear model fit with 95% confidence interval')

#just to visualize the differences
#change the confidence interval to 50%
ggplot(data=iris, aes(x=Petal.Length, y=Petal.Width)) +
  geom_point()+
  theme_bw() +
  xlab('petal length (cm)') +
  ylab('petal width (cm)') +
  geom_smooth(method="lm", se=TRUE, level=.50) +
  ggtitle('linear model fit with 50% confidence interval')


#change the confidence interval to 99%
ggplot(data=iris, aes(x=Petal.Length, y=Petal.Width)) +
  geom_point()+
  theme_bw() +
  xlab('petal length (cm)') +
  ylab('petal width (cm)') +
  geom_smooth(method="lm", se=TRUE, level=.99) +
  ggtitle('linear model fit with 99% confidence interval')


# other modifications -----------------------------------------------------

#now let's make the points color vary by species
ggplot(data=iris, aes(x=Petal.Length, y=Petal.Width, col=Species)) +
  geom_point()+
  theme_bw() +
  xlab('petal length (cm)') +
  ylab('petal width (cm)') +
  geom_smooth(method="lm") +
  ggtitle('linear model fit with 95% confidence interval')

#see how it fits the linear models separately for each species? that's because 
#of the way we set up the mapping. Here's another way to do it where we
#set the mapping within each geom layer--first we map the species by color,
#but then we fit the model to all three species rather than each one

ggplot() +
  geom_point(data=iris, aes(x=Petal.Length, y=Petal.Width, col=Species))+
  theme_bw() +
  xlab('petal length (cm)') +
  ylab('petal width (cm)') +
  geom_smooth(data=iris, aes(x=Petal.Length, y=Petal.Width), method="lm") +
  ggtitle('linear model fit with 95% confidence interval')



# plot facets -------------------------------------------------------------

#another way to visualize differences among species would be to "facet" them into separate plots

ggplot() +
  geom_point(data=iris, aes(x=Petal.Length, y=Petal.Width))+
  theme_bw() +
  xlab('petal length (cm)') +
  ylab('petal width (cm)') +
  geom_smooth(data=iris, aes(x=Petal.Length, y=Petal.Width), method="lm") +
  ggtitle('linear model fit with 95% confidence interval') +
  facet_wrap(~Species)


# box plot ----------------------------------------------------------------

#Now let's look at another type of plot for discrete and continuous data--a boxplot
#Say we want to plot petal width by species. 

ggplot(data=iris, aes(x=Species, y=Petal.Width)) +
  geom_boxplot()+
  theme_bw()

#you can look in the documentation here to see how the box median, tail and whiskers are determined


#something else I like to do with boxplots is to plot the points on top
ggplot(data=iris, aes(x=Species, y=Petal.Width)) +
  geom_boxplot()+
  theme_bw() +
  geom_point()

#another tweak is to jitter the points around in the box
#I usually specify the width argument when I do that to make it look nice
#You could also change the transparency of the points by changing the alpha value
#to make them light
ggplot(data=iris, aes(x=Species, y=Petal.Width)) +
  geom_boxplot()+
  theme_bw() +
  geom_jitter(width=0.2, alpha=.3)


# plot a bar plot of Sepal Width means and standard errors by species  ------------------------------------------------------

# We'll have to wrangle our data to 
# Calculate the mean and standard errors by species


iris %>% #use the iris dataset
  group_by(Species) %>% #set the grouping variable we want to summarize
  summarise(mean_se(Sepal.Width, mult=2)) #calculate the mean and values for 2 standard errors above and below the mean

# One cool thing about the tidyverse is that you can send the output of your 
# data wrangling directly into the plot

iris %>% #use the iris dataset
  group_by(Species) %>% #set the grouping variable we want to summarize
  summarise(mean_se(Sepal.Width, mult=2)) %>% #calculate the mean and values for 2 standard errors above and below the mean
  #now we pipe that directly into a ggplot
  ggplot(aes(x=Species, y=y, fill=Species)) + #make the colors different by species
  geom_col()+  #add the bar chart
  ylab('sepal width (cm)')+
  geom_errorbar(aes(x=Species, ymin=ymin, ymax=ymax), width = 0.5)+ #specify the width
  theme_bw()


# adding letters to indicate statistical differences ----------------------


#now let's do a statistical test on these means
hist(iris$Sepal.Width) #the data appear to be normal
shapiro.test(iris$Sepal.Width) #this also suggests the data are normally distributed

#fit a linear model to the Sepal Width with x being species
lm.iris<-lm(Sepal.Width~Species,data=iris) 
summary(lm.iris) #examine model


iris.aov<-aov(lm.iris) 
summary(iris.aov) #there appear to be significant differences among species, but which?

#Run a Tukey HSD test to detect treatment differences
TukeyHSD(iris.aov)



#we find that setosa is significantly higher than either species, 
#and that veriscolor is significantly lower than virginia
#so they would each get their own unique letter since they are statistically P<0.05
#different from each other


#let's add letter annotations to denote this on the figure
iris %>% #use the iris dataset
  group_by(Species) %>% #set the grouping variable we want to summarize
  summarise(mean_se(Sepal.Width, mult=2)) %>% #calculate the mean and values for 2 standard errors above and below the mean
  #now we pipe that directly into a ggplot %>%
  ggplot(aes(x=Species, y=y, fill=Species)) + #make the colors different by species
  geom_col()+  #add the bar chart
  ylab('sepal width (cm)')+
  geom_errorbar(aes(x=Species, ymin=ymin, ymax=ymax), width = 0.5)+ #specify the width
  theme_bw()+
  geom_text(aes(label = c('a','b','c'), y = c(3.7,3,3.25)), size=5, fontface="bold") 
  



