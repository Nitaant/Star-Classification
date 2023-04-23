# Importing libraries
library(RCurl) # for downloading the star CSV file
library(randomForest)
library(caret)

# Importing the Star data set
stardata <- read.csv("star.csv")
stardata$Star.color <- gsub("Blue ", "Blue", stardata$Star.color)
stardata$Star.color <- gsub("Blue-White", "Blue_White", stardata$Star.color)
stardata$Star.type <-gsub("0", "Brown Dwarf", stardata$Star.type)
stardata$Star.type <-gsub("1", "Red Dwarf ", stardata$Star.type)
stardata$Star.type <-gsub("2", "White Dwarf", stardata$Star.type)
stardata$Star.type <-gsub("3", "Main Sequence", stardata$Star.type)
stardata$Star.type <-gsub("4", "Supergiant", stardata$Star.type)
stardata$Star.type <-gsub("5", "Hypergiant ", stardata$Star.type)
stardata$Luminosity.L.Lo. <- (stardata$Luminosity - min(stardata$Luminosity))/(max(stardata$Luminosity) - min(stardata$Luminosity))

# Performs stratified random split of the data set
# Performs stratified random split of the data set
set.seed(123) # for reproducibility
TrainingIndex <- createDataPartition(stardata$Star.type, p = 0.8, list = FALSE)
TrainingSet <- stardata[TrainingIndex,] # Training Set
TestingSet <- stardata[-TrainingIndex,] # Test Set

write.csv(TrainingSet, "training.csv")
write.csv(TestingSet, "testing.csv")

TrainSet <- read.csv("training.csv", header = TRUE)
TrainSet <- TrainSet[,-1]

model <- randomForest(as.factor(Star.type) ~ . - Luminosity.L.Lo., data = TrainSet, ntree = 500, mtry = 4, importance = TRUE)


# Save model to RDS file
saveRDS(model, "model.rds")

