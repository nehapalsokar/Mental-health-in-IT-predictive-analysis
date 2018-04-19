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

# Frequency distribution of respondents overall by company size
ggplot(company.size, aes(x = organization.size.large, fill=organization.size.large)) + 
  geom_bar() + scale_fill_brewer(palette = "Reds") + 
  ggtitle('Frequency distribution of respondents by company size') +
  xlab('Organization has more than 1000 employees') +
  ylab('Respondent Count') +
  guides(fill=guide_legend(title='More than 1000 employees'))

# Relation between company size and discussing a mental health issue with a supervisor
summary(company.size$discuss.supervisor)
discuss.supervisor.group <- company.size %>% group_by(organization.size.large, discuss.supervisor)
discuss.supervisor.group.plot <- discuss.supervisor.group %>% summarise(count = n()) %>% mutate(frequency = count/sum(count))
# plot the frequency distribution
ggplot(discuss.supervisor.group.plot, 
       aes(x = organization.size.large, y = frequency, fill = discuss.supervisor)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Reds") +
  ggtitle('Comfort discussing mental health issues with superviser based on company size') +
  xlab('Company has more than 1000 employees') +
  ylab('Proportion') +
  guides(fill=guide_legend(title='Comfort in Discussion'))

chisq.test(table(company.size$discuss.supervisor, company.size$organization.size.large))

levels(company.size$discuss.supervisor)
company.size$discuss.supervisor <- relevel(company.size$discuss.supervisor, ref = "No")
mod <- multinom(discuss.supervisor ~ organization.size.large, data = company.size)
summary(mod)

z <- summary(mod)$coefficients/summary(mod)$standard.errors
z
p <- (1 - pnorm(abs(z), 0, 1))*2
p
?pnorm
