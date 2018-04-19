library('caret')
library('dplyr')
library(AUC)
library('nnet')

##### Load the clean dataset #####

mdat1 <- read.csv('MentalHealthCleanedDataset.csv')   # Reading the dataset

mdat1 %>% select(age,gender,work.country,work.us.state,leave.sanction,discuss.supervisor,discuss.coworker,employee.count,mental.health.options,current.mental.disorder,self.employed,company.resources,mental.health.coverage,prev.anonymity.protected,family.history) %>% str()

mdat1$age <-cut(mdat1$age, breaks = c(14,35,78), labels = c('Young','Senior'))


#### discuss.supervisor ####

mdat2 <- mdat1 %>% filter(discuss.supervisor !="")
levels(mdat2$discuss.supervisor)
table(mdat2$discuss.supervisor)
mdat2$discuss.supervisor <- factor(mdat2$discuss.supervisor, levels = c("Maybe","No","Yes"))
levels(mdat2$discuss.supervisor)

#### employee count variable ######


mdat2$organization.size.large <- rep(0, nrow(mdat2))
mdat2$organization.size.large[mdat2$employee.count=="More than 1000"] <- 1
mdat2$organization.size.large <- as.factor(mdat2$organization.size.large)

levels(mdat2$organization.size.large) <- c("No","Yes")

levels(mdat2$organization.size.large)
table(mdat2$organization.size.large)

#### mental health coverage #####

mdat3 <- mdat2 %>% filter(mental.health.coverage !="")
table(mdat3$mental.health.coverage)
mdat3$mental.health.coverage <- factor(mdat3$mental.health.coverage, levels = c("I don't know","Yes","No", "Not eligible for coverage / N/A"))
levels(mdat3$mental.health.coverage)
dim(mdat3)

##### prev.anonymity.protected #####

mdat4 <- mdat3 %>% filter(prev.anonymity.protected !="")
table(mdat4$prev.anonymity.protected)
mdat4$prev.anonymity.protected <- factor(mdat4$prev.anonymity.protected, levels = c("I don't know","No","Sometimes","Yes, always"))
levels(mdat4$prev.anonymity.protected)

#### mental health options #####

mdat4 <- mdat4 %>% filter(mental.health.options !="")
mdat4$mental.health.options <- factor(mdat4$mental.health.options, levels = c("I am not sure", "N/A", "No", "Yes"))
levels(mdat4$mental.health.options)


######################################

mdat4$discuss.supervisor <- relevel(mdat4$discuss.supervisor, ref="No")

mdat4$prev.anonymity.protected <- relevel(mdat4$prev.anonymity.protected, ref="No")

mdat4$mental.health.coverage <- relevel(mdat4$mental.health.coverage, ref="No")

mdat4$mental.health.options <- relevel(mdat4$mental.health.options, ref="No")

levels(mdat4$discuss.supervisor)

with(mdat4, table(discuss.supervisor,mental.health.coverage,prev.anonymity.protected,age))

#### Logistic regression model with all our predictors

mod1 <- multinom(discuss.supervisor~ age + organization.size.large + prev.anonymity.protected + mental.health.coverage + mental.health.options, data=mdat4)

summary(mod1)

z <- summary(mod1)$coefficients/summary(mod1)$standard.errors
z
p <- (1 - pnorm(abs(z), 0, 1))*2
p

y <- factor(mdat4$discuss.supervisor)

fits <- fitted(mod1)
fits
rr <- roc(fits,y)
rr
roc.df <- data.frame(rr$fpr, rr$tpr, rr$cutoffs)
roc.df
ggplot(roc.df,aes(rr.fpr,rr.tpr)) + 
  geom_line(size = 2, alpha = 0.7)+
  labs(title= "ROC curve", 
       x = "False Positive Rate (1-Specificity)", 
       y = "True Positive Rate (Sensitivity)") +
  geom_abline(intercept = 0, slope = 1)

plot(mdat4$mental.health.coverage, mdat4$discuss.supervisor, col="blue")
points(mdat4$mental.health.coverage, fits, pch=19, cex=0.3)
plot(rr)
auc(rr)


##### Model 2 ####

mod2 <- multinom(discuss.supervisor~ organization.size.large + prev.anonymity.protected + mental.health.coverage + mental.health.options, data=mdat4)

summary(mod2)

z <- summary(mod2)$coefficients/summary(mod2)$standard.errors
z
p <- (1 - pnorm(abs(z), 0, 1))*2
p

y <- factor(mdat4$discuss.supervisor)

fits <- fitted(mod2)
fits
rr <- roc(fits,y)
rr
roc.df <- data.frame(rr$fpr, rr$tpr, rr$cutoffs)
roc.df
ggplot(roc.df,aes(rr.fpr,rr.tpr)) + 
  geom_line(size = 2, alpha = 0.7)+
  labs(title= "ROC curve", 
       x = "False Positive Rate (1-Specificity)", 
       y = "True Positive Rate (Sensitivity)") +
  geom_abline(intercept = 0, slope = 1)

plot(mdat4$mental.health.coverage, mdat4$discuss.supervisor, col="blue")
points(mdat4$mental.health.coverage, fits, pch=19, cex=0.3)
plot(rr)
auc(rr)

####################### K Fold Validation ##################

###### Model 1 ######

Train <- createDataPartition(mdat4$discuss.supervisor, p=0.8, list=FALSE)

training <- mdat4[Train, ]
testing <- mdat4[-Train, ]

ctrl <- trainControl(method = "repeatedcv", number = 6, savePredictions = TRUE)

mod_fit <- train(discuss.supervisor~age + organization.size.large + prev.anonymity.protected + mental.health.coverage + mental.health.options,  data=training, method="multinom",
                 trControl = ctrl, tuneLength = 5)


pred = predict(mod_fit, newdata=testing)
cm <- confusionMatrix(data=pred, testing$discuss.supervisor)
mc <- cm$table
percent$value[1] <- sum(diag(mc)) / sum(mc) * 100
percent
##### model 2 ####

mod_fit2 <- train(discuss.supervisor~organization.size.large + mental.health.coverage + prev.anonymity.protected + mental.health.options,  data=training, method="multinom",
                 trControl = ctrl, tuneLength = 5)


pred2 = predict(mod_fit2, newdata=testing)
cm2 <- confusionMatrix(data=pred2, testing$discuss.supervisor)
mc2 <- cm2$table
percent$value[2] <- sum(diag(mc2)) / sum(mc2) * 100
percent

#### Random Forest ######

percent$value[3] <- 60.09
percent$value[4] <- 61.57
#plotting accurcy of all models
percent <- data.frame(methods=c("Multinomial Regression - Full Model", "Multinomial Regression - Reduced Model","Random Forest - Full Model","Random Forest - Reduced Model"), value=c(0,0,0,0))
percent$methods <- paste(percent$methods, " - " , round(percent$value,digits = 2) , "%" , sep = "")
percent1 <- data.frame(methods = percent$methods, value = percent$value)
percent2 <- rbind(percent1,data.frame(value=30, methods=percent1$methods))
ggplot() +
  geom_point(data = percent, aes(x = value, y = methods, color = 'grey'), size = 4) +
  geom_path(data = percent2, aes(x = value, y = methods, color = 'grey'), size = 2) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5)) +
  labs(
    x = "Percentage Accuracy",
    y = "Models",
    title = "Percentage success of the models"
  ) 

