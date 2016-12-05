library(lubridate)
library(randomForest)
library(nnet)

df <- read.csv('data/train.csv')
df$Hour <- as.factor(hour(as.POSIXlt(df$Dates, tz="UTC")))
df$Year <- factor(year(df$Dates))
df$DayOfWeek <- factor(df$DayOfWeek, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday",
                                              "Friday", "Saturday"))
df$Month <- month.abb[month(df$Dates)]
df$Month <- factor(df$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
                                        "Oct", "Nov", "Dec"))

# Analysis from this point on is less generalized. Data is split and analyzed based on whether the
# crime is violent or non-violent.
violentDF <- df[df$Category %in% c("ROBBERY", "ASSAULT", "SEX OFFENSES FORCIBLE", "KIDNAPPING"),]
theftDF <- df[df$Category %in% c("LARCENY/THEFT", "VEHICLE THEFT", "BURGLARY"),]
violentDF <- droplevels(violentDF)
theftDF <- droplevels(theftDF)

set.seed(123)  #let's get reproducible results
violentTrainInd <- sample(seq_len(nrow(violentDF)), nrow(violentDF) * .75)
violentTrain <- violentDF[violentTrainInd,]
violentTest <- violentDF[-violentTrainInd,]

rf <- randomForest(Category ~ Hour + Month + X + Y + DayOfWeek, ntree = 50, data = violentTrain)
preds <- predict(rf, violentTest)
correctPreds <- preds == violentTest$Category
t <- table(preds, violentTest$Category)
print(t)
accuracy <- sum(correctPreds)/nrow(violentTest)
print(accuracy)