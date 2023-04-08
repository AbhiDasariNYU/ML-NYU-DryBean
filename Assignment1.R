list.of.packages <- c("readr", "dplyr","ggplot2","GGally","outliers","corrplot","MASS","caret","EnvStats","randomForest")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library('readr')
library(dplyr) 
#library('tidyverse')
library('ggplot2')
library("GGally")
library('outliers')
library('corrplot')
library('MASS')
library('caret')
library('EnvStats')
library('randomForest')
df <- read_csv('Dry_Bean_Dataset.csv')
# Various functions to take a look at data

#Taking a look at the data using various functions
head(df)
glimpse(df)
tail(df)
str(df)


#EDA
summary(df)

#checking for null and empty fields 
print(colSums(is.na(df)))
print(colSums(df==""))
print(colSums(df=="Unknown"))


ggplot(data = df) +
  geom_bar(mapping = aes(x = Class))

#Understanding the Classes
names(df)
classes=table(df$Class)
classes
#plotting the class Distribution in a pie chart
pie(table(df$Class))
names(classes)
as.numeric(classes)

print(ifelse(length(classes)==2, "Binary Classification", "MultiClass"))

#Understandning the distribution of data
Mean <- df%>% summarise_if(is.numeric, mean)
print(mean)
Standard_Deviation <- df%>% summarise_if(is.numeric, sd)
print(Standard_Deviation)

#subseting of data can be done in various ways
subset(df,Area < 25000)
data_Dermason <- df %>%   filter(Class=="DERMASON") 

ggplot(data=df,aes(x=roundness, y=EquivDiameter,color=Class)) + geom_point() + theme_minimal()
ggplot(data=df,aes(x=Area, y=Perimeter,color=Class)) + geom_point() + theme_minimal()
ggplot(data=df,aes(x=ConvexArea, y=EquivDiameter,color=Class)) + geom_point() + theme_minimal()
ggplot(data=df,aes(x=ShapeFactor1, y=ShapeFactor2,color=Class)) + geom_point() + theme_minimal()
ggplot(data=df,aes(x=ShapeFactor3, y=ShapeFactor4,color=Class)) + geom_point() + theme_minimal()


p <- ggplot(df, aes(x = Area, y = MajorAxisLength, z=Perimeter))+ geom_point() + stat_density2d()
ggplot(data=df,aes(x=Class, y=Eccentricity,color=Class))+ geom_boxplot() +theme_minimal()+ theme(legend.position="none")

#grouping data by class
dataGroupBy_Class <- df %>%group_by(Class)
dataGroupBy_Class

p + stat_density2d(aes(colour = ..level..))
p + stat_density2d(aes(fill = ..density..), geom = "raster", contour = FALSE)
#Density Plot smooth density estimate
ggplot(df) + geom_density(aes(x = Compactness, fill = Class), alpha=0.25)

#understanding the correlation using spearman and kendall 
cor(df$Area, df$Perimeter, method = 'spearman')
cor(df$Area, df$MajorAxisLength, method = 'kendall')


#plotting the correlation
cor(df[1:16])
corrplot(cor(df[1:16]))


#using grubbs method for to find the outliers of data
grubbs.flag <- function(x) {   
  outliers <- NULL   
  test <- x   
  grubbs.result <-   grubbs.test(test)   
  pv <- grubbs.result$p.value   
  while(pv < 0.05) {   
    outliers <-   c(outliers,as.numeric(strsplit(grubbs.result$alternative," ")[[1]][3]))   
    test <- x[!x %in% outliers]   
    grubbs.result <-   grubbs.test(test)   
    pv <- grubbs.result$p.value   
  }   
  return(data.frame(X=x,Outlier=(x   %in% outliers)))   
} 


#finding outliers in roundness column
outliers=grubbs.flag(data_Dermason$roundness)
data_Dermason <- df %>%   filter(Class=="DERMASON") 
outliers <- outliers %>% filter(Outlier=="TRUE")
outliers
ggplot(grubbs.flag(data_Dermason$roundness),aes(x=data_Dermason$roundness,color=Outlier,fill=Outlier))+geom_histogram(binwidth=diff(range(data_Dermason$roundness))/30)+ theme_bw()   

#finding outliers in Area column
outliers=grubbs.flag(data_Dermason$Area)
data_Dermason <- df %>%   filter(Class=="DERMASON") 
outliers <- outliers %>% filter(Outlier=="TRUE")
outliers
ggplot(grubbs.flag(data_Dermason$Area),aes(x=data_Dermason$Area,color=Outlier,fill=Outlier))+geom_histogram(binwidth=diff(range(data_Dermason$Area))/30)+ theme_bw()   



outliers <- rosnerTest(df$Area, k=10, warn=TRUE)
outliers
outliers$all.stats

class_df <- data.frame(Class = c('BARBUNYA','BOMBAY','CALI','DERMASON','HOROZ','SEKER','SIRA'),Class_num = c(0,1,2,3,4,5,6),stringsAsFactors = FALSE)
data_new <- merge(df, class_df, by = "Class", all.x = TRUE)
set.seed(4543)

#running Random forest to get the variable importance
df.rf <- randomForest(data_new$Class_num ~ ., data=data_new[2:16], ntree=501,
                    keep.forest=FALSE, importance=TRUE)
importance(df.rf)
importance(df.rf, type=1)
varImpPlot(df.rf,type=2)

#Other Box Plots
ggplot(data=df,aes(x=Class, y=EquivDiameter,color=Class))+ geom_boxplot() +theme_minimal()+ theme(legend.position="none")
ggplot(data=df,aes(x=Class, y=Perimeter,color=Class))+ geom_boxplot() +theme_minimal()+ theme(legend.position="none")
ggplot(data=df,aes(x=Class, y=Area,color=Class))+ geom_boxplot() +theme_minimal()+ theme(legend.position="none")
ggplot(data=df,aes(x=Class, y=roundness,color=Class))+ geom_boxplot() +theme_minimal()+ theme(legend.position="none")

#other Density Plots of other columns
ggplot(df) + geom_density(aes(x = Extent, fill = Class), alpha=0.25)
ggplot(df) + geom_density(aes(x = Perimeter, fill = Class), alpha=0.25)
ggplot(df) + geom_density(aes(x = Area, fill = Class), alpha=0.25)
ggplot(df) + geom_density(aes(x = EquivDiameter, fill = Class), alpha=0.25)
ggplot(df) + geom_density(aes(x = Compactness, fill = Class), alpha=0.25)
ggplot(df) + geom_density(aes(x = ShapeFactor2, fill = Class), alpha=0.25)
