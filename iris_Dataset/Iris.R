getwd()
setwd("/home/vinayak/Downloads/R Projects/iris_Dataset")

##Importing the Iris Dataset
iris_data <- read.csv("iris.csv",stringsAsFactors = FALSE)
print(iris_data)

##Performing Exploratory Data Analysis (EDA)

##Print first 3 Records from Dataset
head(iris_data,3)
##Dimension of Dataset
dim(iris_data)
##Names , Class of features in the Dataset
names(iris_data)
class(iris_data)
##Removing Missing values (if any) & make the data consistent by removing it
is.na(iris_data)
na.omit(iris_data)
##Structure of Data
str(iris_data)
##Mean, Median, Quartile, Max, Min data for every feature
summary(iris_data)
View(iris_data)

##Plot a Boxplot Graph, Pie chart respective to their Species

png(file = "pie1.png")
pie(table(iris_data$species),labels = c("Setosa","Versicolor","Virginica") )
dev.off()

library("ggplot2")
png(file = "boxplot.png")
boxplot(iris$Sepal.Length~iris$Species)
boxplot(iris$Sepal.Length~iris$Species,xlab="Species",ylab="Sepal Length",main="Species Information")
dev.off()

##Subset tuples based on their Species in different R-Object

sl <- subset(iris_data,sepal_length == 4:8)
species <- subset(iris_data,sepal_lenght=sl)
print(species$sepal_length)


sw <- subset(iris_data,sepal_width == 2:4.4)
species <- subset(iris_data,sepal_length=sw)
print(species$sepal_width)

pl <- subset(iris_data,petal_length == 1:6.9)
species <- subset(iris_data,petal_length=pl)
print(species$petal_length)

pw <- subset(iris_data,petal_width == 0.1:2.5)
species <- subset(iris_data,petal_width=pw)
print(species$petal_width)

ss <- subset(iris_data,species == character())
species <- subset(iris_data,species=ss)
print(species$species)

##Plot a BoxPlot Graph for Individual R-Object

png(file="boxplot2.png")
box <- ggplot(data=iris_data, aes(x=species, y=sepal_length)) +
  geom_boxplot(aes(fill=species)) +  ylab("Sepal Length") +  ggtitle("Iris Boxplot") +
  stat_summary(fun.y=mean, geom="point", shape=5, size=4) 
dev.off()

##Histogram on feature Petal lengths of iris dataset

png(file="histo1.png")
hist(iris_data$petal_length,xlab = "length",col="blue",border="green")
dev.off()

##Plot a Histogram for Petal Lengths of Different Species on different Graph

png(file="histo2.png")
ggplot(iris_data, aes(species, petal_length, fill=species)) + 
  geom_boxplot()+
  scale_y_continuous("Petal Length (cm)", breaks= seq(0,30, by=.5))+
  labs(title = "Iris Petal Length Box Plot", x = "Species")
dev.off()

##Correlation between multiple features also plot a scatter plot for correlation

corr <- cor(iris_data[,1:4])
round(corr,3)
png(file = "scatterplot1.png")
pairs(iris_data[,1:4])
dev.off()


##Classify Data based on iris Species and plot a Decision Tree

library(rpart)
library(rattle)
library(rpart.plot)
png(file ="decisiontree2.png")
input.dat <- iris_data
print(input.dat)
fm = species ~ sepal_length + sepal_width + petal_length + petal_width
fit <- rpart(formula = fm,input.dat,method = 'class')
fancyRpartPlot(fit)
dev.off()
