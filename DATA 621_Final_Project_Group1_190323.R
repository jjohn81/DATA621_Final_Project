
library(tidyverse)
library(caret)

#loading data from github (raw)
red <- data.frame(read.csv("https://raw.githubusercontent.com/jjohn81/DATA621_Final_Project/master/winequality-red.csv", sep = ";"))
white <- data.frame(read.csv("https://raw.githubusercontent.com/jjohn81/DATA621_Final_Project/master/winequality-white.csv", sep = ";"))

#checking that column names are the same
names(red)
names(white)

#quality of reds from 3 to 8 (mean = 5.636)
summary(red)

#quality of whites from 3 to 9 (mean = 5.878)
summary(white)

#setting a type category, then binding the two datasets
red$type <- as.factor("R")
white$type <- as.factor("W")
wines <- rbind(red,white)

# 6497 obs. of  13 variables
str(wines)
  

# Data Visualizations (to fill in) - boxplots, etc. 

# Scatterplot matrix

panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

pairs(~quality + fixed.acidity + volatile.acidity + citric.acid + residual.sugar + 
        chlorides + free.sulfur.dioxide + total.sulfur.dioxide + density +pH + 
        sulphates + alcohol + type, data = wines,
      lower.panel=panel.smooth, upper.panel=panel.cor, pch=20, main="Wines Scatterplot Matrix")

# One notes the high correlation between free.sulfur.dioxide and total.sulfur.dioxide (0.72),
# between type and total.sulfur.dioxide (0.70),
# between density and alcohol (0.69),
# between type and volatile.aciditiy (0.65)



# Setting first regression
# Using 10-fold Cross-Validation
# http://www.sthda.com/english/articles/38-regression-model-validation/157-cross-validation-essentials-in-r/

# Define training control
set.seed(123) 
train.control <- trainControl(method = "cv", number = 10)

# Train the model
model <- train(quality ~ ., data = wines, method = "lm",
               trControl = train.control)

# Summarize the results
print(model)
summary(model)
# noting that typeW has a negative coefficient, slightly surprising given that the whites had the higher mean for quality

# noting that citric.acid is not statistically significant

model <- train(quality ~ . -citric.acid, data = wines, method = "lm",
               trControl = train.control)
print(model)
summary(model)
# Adjusted R-squared:  0.2953 
