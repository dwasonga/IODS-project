#Name: Daniel Wasonga 
#Date: Nov 6 2019
#Data wrangling and regression code exercise IDOS

#       DATA WRANGLING

library(dplyr)

#Load learning data file into R
lrn14 <- read.table("http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS3-data.txt",
                           header=T,sep="\t")
#lrn14 #view learning data

#dimensions of data
dim(lrn14)
#structure of data
str(lrn14)
#showing header and 1st 6 obs
head(lrn14)

#output comments
#The learning2014 data consist of 183 observations and 60 variables. 
#The measurements /concern variables are age, attitude, points and gender. 

#combining questions of data sets
gender_questions <- c()
age_questions <- c( )
attitude_questions <- c("Da", "Db", "Dc", "Dd", "De", "Df", "Dg", "Dh", "Di", "Dj")
deep_questions <- c("D03", "D11", "D19", "D27", "D07", "D14", "D22", "D30","D06",  "D15", "D23", "D31")
surface_questions <- c("SU02","SU10","SU18","SU26", "SU05","SU13","SU21","SU29","SU08","SU16","SU24","SU32")
strategic_questions <- c("ST01","ST09","ST17","ST25","ST04","ST12","ST20","ST28")
points_questions <- c("Ca", "Cb", "Cc", "Cd", "Ce", "Cf", "Cg", "Ch")

# select and create specific columns by averaging
deep_columns <- select(lrn14, one_of(deep_questions))
lrn14$deep <- rowMeans(deep_columns)

surface_columns <- select(lrn14, one_of(surface_questions))
lrn14$surf <- rowMeans(surface_columns)

strategic_columns <- select(lrn14, one_of(strategic_questions))
lrn14$stra <- rowMeans(strategic_columns)


#scaling combination of varibales
lrn14deep <- lrn14$deep/10 
lrn14surf <- lrn14$surf/10
lrn14stra <- lrn14$stra/10

#lrn14scale <- c(lrn14deep,lrn14surf, lrn14stra)
lrn14scale
lrn14scale <- filter(lrn14scale, points() > "0")



#                REGRESSION ANALYSIS

library(GGally)
library (ggplot2)

lrn14 <- read.table("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/learning2014.txt",
                    header=T,sep=",")

dim(lrn14) #dimension of data
str(lrn14) #structure of data
head(lrn14)

#Data has 166 observations and 7 variables.
#Students were assessed on their perception to learning statistics especially on their attitude,
#strategic, deep and surface learning. In addition, their gender and age variations were recorded. 

#Data visualisation - create an advanced plot with ggpairs
p <- ggpairs(lrn14, mapping = aes( ), 
               lower = list(combo = wrap("facethist", bins = 20))) #inside the 'aes' one can modify to add "col = gender"
p #show graph
summary(lrn14)

#visualising the model

mod3<- ggplot(lrn14, aes(x=deep + stra +attitude, y=points)) + geom_point() + ggtitle("Student strategic, deep & attitude vs exam points") +
  geom_smooth(method = "lm")
mod3

Attd <- ggplot(lrn14, aes(x=attitude, y=points)) + geom_point() + ggtitle("Student attitude vs exam points") +
  geom_smooth(method = "lm")
Attd

#Fit regression model
my_model<- lm(points ~ attitude + deep + stra, data = lrn14) #multiple linreg
summary(my_model)
my_model2<- lm(points ~ attitude + deep, data = lrn14) # can remove stra coz lowest contrb & not sig
summary(my_model2)

#Diagnostic plots
par(mfrow = c(2,2))
plot(my_model2, which = c(1,2,5)) # check ?plot.lim in R for the 'which' reg type
