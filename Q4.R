library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(nnet)

# load dataset
survey.data <- read.csv("MentalHealthCleanedDataset.csv", header = TRUE, stringsAsFactors = TRUE)
str(survey.data)

dim(survey.data)

# filter the missing responses

company.size <- survey.data %>% filter(employee.count != '')
summary(company.size$employee.count)
levels(company.size$organization.size.large)
company.size$organization.size.large <- rep("0", nrow(company.size))
company.size$organization.size.large[company.size$employee.count=="More than 1000"] <- "1"
company.size$organization.size.large <- as.factor(company.size$organization.size.large)
levels(company.size$organization.size.large) <- c("No","Yes")
table(company.size$organization.size.large)

# ordering the factors
company.size$discuss.supervisor <- factor(company.size$discuss.supervisor, levels = c('Maybe', 'No', 'Yes'))

levels(survey.data$prev.anonymity.protected)
survey.data$prev.anonymity.protected <- relevel(survey.data$prev.anonymity.protected, ref = 'No')
# Frequency distribution of respondents overall by company size
ggplot(company.size, aes(x = organization.size.large, fill=organization.size.large)) + 
  geom_bar() + scale_fill_brewer(palette = "Reds") + 
  ggtitle('Frequency distribution of respondents by company size') +
  xlab('Organization has more than 1000 employees') +
  ylab('Respondent Count') +
  guides(fill=guide_legend(title='More than 1000 employees'))

# Relation between company size and discussing a mental health issue with a supervisor
summary(company.size$discuss.supervisor)
# Relation between prev anonymity protec and discussing a mental health issue with a supervisor
anom.group <- survey.data %>% group_by(prev.anonymity.protected, discuss.supervisor)
anom.plot <- anom.group %>% summarise(count = n()) %>% mutate(frequency = count/sum(count))
# plot the frequency distribution
ggplot(anom.plot, 
       aes(x = prev.anonymity.protected, y = frequency, fill = discuss.supervisor)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Reds") +
  ggtitle('Comfort Vs protection of previous anonymity') +
  xlab('Prev anonymity protected') +
  ylab('Proportion') +
  guides(fill=guide_legend(title='Comfort in Discussion'))

chisq.test(table(company.size$discuss.supervisor, company.size$organization.size.large))

levels(survey.data$discuss.supervisor)
survey.data$discuss.supervisor <- relevel(survey.data$discuss.supervisor, ref = 'No')

company.size$discuss.supervisor <- relevel(company.size$discuss.supervisor, ref = "No")
mod <- multinom(discuss.supervisor ~ prev.anonymity.protected, data = survey.data)
summary(mod)

z <- summary(mod)$coefficients/summary(mod)$standard.errors
z
p <- (1 - pnorm(abs(z), 0, 1))*2
p
?pnorm


#3 a. People reaching out to employer with respect to mental benefits being provided

survey.data$mental.health.comfort.supervisor2 <- relevel(survey.data$discuss.supervisor, ref = "No")

discuss.supervisor.group <- survey.data %>% group_by(mental.health.coverage, mental.health.comfort.supervisor2)
discuss.supervisor.group.plot <- discuss.supervisor.group %>% summarise(count = n()) %>% mutate(frequency = count/sum(count))

ggplot(discuss.supervisor.group.plot, 
       aes(x = mental.health.coverage, y = frequency, fill = mental.health.comfort.supervisor2)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Reds") +
  ggtitle('People reaching out to employer with respect to mental benefits being provided') +
  xlab('Mental health benefits provided from employer') +
  ylab('Proportion') +
  theme(axis.text.x = element_text(angle=35,vjust=0.6))+
  guides(fill=guide_legend(title='Comfort in Discussion'))




# mental health coverage
survey.data$mental.health.comfort.supervisor2 <- relevel(survey.data$discuss.supervisor, ref = "No")
survey.data$mental.health.coverage<- relevel(survey.data$mental.health.coverage,ref="No")
mod <- multinom(mental.health.comfort.supervisor2 ~ mental.health.coverage + mental.health.options, data = survey.data)
summary(mod)

z <- summary(mod)$coefficients/summary(mod)$standard.errors
z

p <- (1 - pnorm(abs(z), 0, 1))*2
p

