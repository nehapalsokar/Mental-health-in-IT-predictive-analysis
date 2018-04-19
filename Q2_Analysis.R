#What percentage of employees reach out for help to the employer, given that the employer is
#providing benefits to those who are suffering from mental illness?
library(ggplot2)
library(RColorBrewer)
library(dplyr)

data<-read.csv("MentalHealthCleanedDataset.csv", header = TRUE, stringsAsFactors = TRUE)
names(data) = tolower(names(data))

dim(data)
# DISCARDING RECORDS FOR SELF-EMPLOYED PEOPLE
survey.data <- data %>% filter(self.employed==0)
#data <- data[data$self.employed==0,]
dim(survey.data)
#1 Distribution of employees getting Mental Health Benefits from employers
table(survey.data$mental.health.coverage)

ggplot(data =  survey.data, aes(x=mental.health.coverage,fill=factor(mental.health.coverage)))+ 
geom_bar(aes(x=mental.health.coverage), position = "dodge")+
scale_fill_brewer(palette = 'Set3', name = "Benefits Provided")+
ggtitle("Distribution of employees getting Mental Health Benefits from employers")+
  xlab("Employees getting Mental Health Benefits from employers")+
  theme(legend.position = "bottom")


#2 Distribution of employees' comfort level in discussing issues with Supervisors
survey.data$discuss.supervisor <- factor(survey.data$discuss.supervisor)
table(survey.data$discuss.supervisor)

ggplot(data =  survey.data, aes(x=discuss.supervisor,fill=factor(discuss.supervisor)))+ 
  geom_bar(aes(x=discuss.supervisor), position = "dodge")+
  scale_fill_brewer(palette = 'Set3', name = "Discuss with Supervisor")+
  ggtitle("Distribution of employees' comfort level in discussing issues with Supervisors")+
  xlab("Employees' comfort level in discussing issues with Supervisors")+
  theme(legend.position = "bottom")

#3 a. People reaching out to employer with respect to mental benefits being provided

data.hlthcov <- survey.data %>% filter(mental.health.coverage != 'Not eligible for coverage / N/A' & mental.health.coverage !="I don't know")

ggplot(data.hlthcov, aes(x = discuss.supervisor,y=Percentage, fill=factor(data.hlthcov$mental.health.coverage))) +  
  geom_bar(aes(y = ((..count..)/sum(..count..))*100), position = "dodge")+
  scale_fill_brewer(palette = 'Set3', name = "Benefits Provided")+
  ggtitle("People reaching out to employer with respect to mental benefits being provided")+
  xlab("People reaching out to employer")+
  theme(legend.position = "bottom")


#3 b. People reaching out to employer with respect to awareness of mental health care options 

	data.hlthoptions <- survey.data %>% filter(mental.health.options != 'N/A')

ggplot(data.hlthoptions , aes(x = discuss.supervisor,y=Percentage, fill=factor(mental.health.options))) + 
  geom_bar(aes(y = ((..count..)/sum(..count..))*100), position = "dodge") +
  scale_fill_brewer(palette = 'Set3', name = "awareness of mental health care options")+
  ggtitle("People reaching out to employer with respect to awareness of mental health care options") +
  xlab("People reaching out to employer")+
theme(legend.position = "bottom")


# Chi-square test

survey.data$mental.health.options <- factor(survey.data$mental.health.options)
chisq.test(table(survey.data$discuss.supervisor, survey.data$mental.health.coverage))

chisq.test(table(survey.data$discuss.supervisor, survey.data$mental.health.options))

# Multinomial logistic regression

require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)
# mental health coverage
survey.data$mental.health.comfort.supervisor2 <- relevel(survey.data$discuss.supervisor, ref = "No")
mod <- multinom(mental.health.comfort.supervisor2 ~ mental.health.coverage, data = survey.data)
summary(mod)

z <- summary(mod)$coefficients/summary(mod)$standard.errors
z

p <- (1 - pnorm(abs(z), 0, 1))*2
p

exp(coef(mod))

# mental.health.options
mod2 <- multinom(mental.health.comfort.supervisor2 ~ mental.health.options, data = survey.data)
summary(mod2)


z <- summary(mod2)$coefficients/summary(mod2)$standard.errors
z

p <- (1 - pnorm(abs(z), 0, 1))*2
p

exp(coef(mod2))


 