library(datasets)
library(caret)

data("dhfr")
View(dhfr)

head(dhfr,4)
tail(dhfr,4)

summary(dhfr)
summary(dhfr$moeGao_Abra_L)

sum(is.na(dhfr))

library(skimr)
skim(dhfr)

dhfr %>%
  dplyr::group_by(Y) %>%
  skim()

# Quick Data Visualization

plot(dhfr)
plot(dhfr$moeGao_Abra_L)

# Scatter plot

plot(dhfr$moeGao_Abra_acidity,dhfr$moeGao_Abra_basicity,col='red')

plot(dhfr$moeGao_Abra_L,dhfr$moeGao_Abra_R,col=dhfr$Y,xlab='Abra L',ylab='Abra R')

# Histogram

hist(dhfr$moeGao_Abra_L)

hist(dhfr$moeGao_Abra_R,col='red')

#Feature Plot

featurePlot(x=dhfr[,2:20],
            y=dhfr$Y,
            plot='box',
            strip=strip.custom(par.strip.text=list(cex=0.7)),
            scales=list(x=list(relation="free"),
                        y=list(relation="free")))

# To achieve reproducible model; set random seed number

set.seed(100)

# Performs stratified random split of the data set

TrainingIndex <- createDataPartition(dhfr$Y, p=0.8, list = FALSE)
TrainingSet <- dhfr[TrainingIndex,]   # Training set
TestingSet <- dhfr[-TrainingIndex,]   # Testing set

# SVM model (polynomial kernel)

# Build Training model

Model <- train(Y ~ ., data = TrainingSet,
               method = 'svmPoly',
               na.action = na.omit,
               preProcess = c("scale","center"),
               trControl = trainControl(method = 'none'),
               tuneGrid = data.frame(degree = 1, scale = 1, C = 1)
)

# Build CV model

Model.cv <- train(Y ~ ., data = TrainingSet,
                  method = 'svmPoly',
                  na.action = na.omit,
                  preProcess = c("scale","center"),
                  trControl = trainControl(method = 'cv', number=10),
                  tuneGrid = data.frame(degree = 1, scale = 1, C = 1)
)

# Apply model for prediction

Model.training <- predict(Model, TrainingSet)
Model.testing <- predict(Model,TestingSet)
Model.cv <- predict(Model.cv, TrainingSet)

# Model Performance (Displays confusion matrix and statistics)

Model.training.confusion <- confusionMatrix(Model.training,TrainingSet$Y) 
Model.testing.confusion <- confusionMatrix(Model.testing,TestingSet$Y)
Model.cv.cofusion <- confusionMatrix(Model.cv,TrainingSet$Y)

print(Model.training.confusion)
print(Model.testing.confusion)
print(Model.cv.cofusion)

Importance <- varImp(Model)
plot(Importance, top=25)
plot(Importance, col='red')








