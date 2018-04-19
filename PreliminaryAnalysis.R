library(ggplot2)
library(magrittr)
library(dplyr)
library(plotly)
library(RColorBrewer)
library(gridExtra)

# Data Cleaning

# Gender
data <- read.csv("MentalHealthCleanedDataset.csv", header = TRUE, stringsAsFactors = TRUE)
new.names <- c("self.employed", "num.employees", "tech.company", "tech.role", "mental.health.coverage", "mental.health.options", "mental.health.formally.discussed", "mental.health.resources", "anonymity.protected", "medical.leave", "mental.health.negative", "physical.health.negative", "mental.health.comfort.coworker", "mental.health.comfort.supervisor", "mental.health.taken.seriously", "coworker.negative.consequences", "private.med.coverage", "resources", "reveal.diagnosis.clients.or.business", "revealed.negative.consequences.CB", "reveal.diagnosis.coworkers", "revealed.negative.consequences.CW", "productivity.effected", "percentage", "previous.employer", "prevemp.mental.health.coverage", "prevemp.mental.health.options", "prevemp.mental.health.formally.discussed", "prevemp.mental.health.resources", "prevemp.anonymity.protected", "prevemp.mental.health.negative",
               "prevemp.physical.health.negative", "prevemp.mental.health.coworker", "prevemp.mental.health.comfort.supervisor", "prevemp.mental.health.taken.seriously", "prevemp.coworker.negative.consequences", "mention.phsyical.issue.interview", "why.whynot.physical", "mention.mental.health.interview", "why.whynot.mental", "career.hurt", "viewed.negatively.by.coworkers", "share.with.family", "observed.poor.handling", "observations.lead.less.likely.to.reveal", "family.history", "ever.had.mental.disorder", "current.disorder", "if.yes.what", "if.maybe.what", "medical.prof.diagnosis", "what.conditions", "sought.prof.treatment", "treatment.affects.work", "no.treatment.affects.work", "age", "gender", "country.living", "US.state", "country.work", "state.work", "work.position", "remotely"  )
getwd()
colnames(data) <- new.names
data$gender <- as.character(data$gender)

table(is.na(data$gender))

data[data$gender == "Male", "gender"] <- "M"
data[data$gender == "male", "gender"] <- "M"
data[data$gender == "MALE", "gender"] <- "M"
data[data$gender == "Man", "gender"] <- "M"
data[data$gender == "man", "gender"] <- "M"
data[data$gender == "m", "gender"] <- "M"
data[data$gender == "man ", "gender"] <- "M"
data[data$gender == "Dude", "gender"] <- "M"
data[data$gender == "mail", "gender"] <- "M"
data[data$gender == "M|", "gender"] <- "M"
data[data$gender == "Cis male", "gender"] <- "M"
data[data$gender == "Male (cis)", "gender"] <- "M"
data[data$gender == "Cis Male", "gender"] <- "M"
data[data$gender == "cis male", "gender"] <- "M"
data[data$gender == "cisdude", "gender"] <- "M"
data[data$gender == "cis man", "gender"] <- "M"
data[data$gender == "Male.", "gender"] <- "M"
data[data$gender == "Male ", "gender"] <- "M"
data[data$gender == "male ", "gender"] <- "M"
data[data$gender == "Malr", "gender"] <- "M"
data[841,"gender"] <- "M"

data[data$gender == "Female", "gender"] <- "F"
data[data$gender == "Female ", "gender"] <- "F"
data[data$gender == " Female", "gender"] <- "F"
data[data$gender == "female", "gender"] <- "F"
data[data$gender == "female ", "gender"] <- "F"
data[data$gender == "Woman", "gender"] <- "F"
data[data$gender == "woman", "gender"] <- "F"
data[data$gender == "f", "gender"] <- "F"
data[data$gender == "Cis female", "gender"] <- "F"
data[data$gender == "Cis female ", "gender"] <- "F"
data[data$gender == "Cisgender Female", "gender"] <- "F"
data[data$gender == "Cis-woman", "gender"] <- "F"
data[data$gender == "fem", "gender"] <- "F"
data[1091, "gender"] <- "F"
data[17, "gender"] <- "F"

# gender queer (GQ)
data[!is.na(data$gender)&data$gender == "Agender", "gender"] <- "GQ"
data[!is.na(data$gender)&data$gender == "Androgynous", "gender"] <- "GQ"
data[!is.na(data$gender)&data$gender == "Bigender", "gender"] <- "GQ"
data[!is.na(data$gender)&data$gender == "Female or Multi-Gender Femme", "gender"] <- "GQ"
data[!is.na(data$gender)&data$gender == "female-bodied; no feelings about gender", "gender"] <- "GQ"
data[!is.na(data$gender)&data$gender == "Fluid", "gender"] <- "GQ"
data[!is.na(data$gender)&data$gender == "fm", "gender"] <- "GQ"
data[!is.na(data$gender)&data$gender == "GenderFluid", "gender"] <- "GQ"
data[!is.na(data$gender)&data$gender == "GenderFluid (born female)", "gender"] <- "GQ"
data[!is.na(data$gender)&data$gender == "Genderflux demi-girl", "gender"] <- "GQ"
data[!is.na(data$gender)&data$gender == "genderqueer", "gender"] <- "GQ"
data[!is.na(data$gender)&data$gender == "Genderqueer", "gender"] <- "GQ"
data[!is.na(data$gender)&data$gender == "fm", "gender"] <- "GQ"
data[!is.na(data$gender)&data$gender == "genderqueer woman", "gender"] <- "GQ"
data[!is.na(data$gender)&data$gender == "human", "gender"] <- "GQ"
data[!is.na(data$gender)&data$gender == "Human", "gender"] <- "GQ"
data[!is.na(data$gender)&data$gender == "Unicorn", "gender"] <- "GQ"
data[!is.na(data$gender)&data$gender == "Male/genderqueer", "gender"] <- "GQ"
data[!is.na(data$gender)&data$gender == "nb masculine", "gender"] <- "GQ"
data[!is.na(data$gender)&data$gender == "non-binary", "gender"] <- "GQ"
data[!is.na(data$gender)&data$gender == "Nonbinary", "gender"] <- "GQ"
data[!is.na(data$gender)&data$gender == "AFAB", "gender"] <- "GQ"

# transgender (TG)
data[!is.na(data$gender)&data$gender == "Male (trans, FtM)", "gender"] <- "TG"
data[!is.na(data$gender)&data$gender == "Transgender woman", "gender"] <- "TG"

# see what's left
index <- which(data$gender != "M" & data$gender != "F" & data$gender != "GQ" & data$gender != "TG")

data[index, "gender"]
##  [1] "Female assigned at birth " "Transitioned, M2F"        
##  [3] "Genderfluid (born female)" "Other/Transfeminine"      
##  [5] "female/woman"              "male 9:1 female, roughly" 
##  [7] "N/A"                       "Other"                    
##  [9] "Sex is male"               "none of your business"    
## [11] "Genderfluid"               "N/A"                      
## [13] "Enby"                      "mtf"                      
## [15] "Queer"                     ""
# create vector of final gender values to fill in based on index
last.genders <- c("F", "TG", "GQ", "GQ", "F", "GQ", "GQ", "GQ", "M", "Refused", "GQ", "GQ", "GQ", "TG", "GQ", NA)

# fill in remaining values
data[index, "gender"] <- last.genders

# check gender
table(data$gender)

# convert gender back to factor
data$gender <- as.factor(data$gender)

# Age Cleaning
summary(data$age)
# Filter records with age 3 and 323
#company.age <- data %>% filter(age != 323 & age !=3)
#summary(company.age$age)
# Max age is still 99
#company.age <- data %>% filter(data$age > 15 & data$age <= 80)
#dim(company.age)

Age_subset<-subset(data, data$age>=15 & data$age<=75)

# Summary of gender distribution
levels(data$gender)
gender.subset=subset(data, data$gender=='M' | data$gender=='F', data, data$gender=='M' | data$gender=='F')
ggplot(data, aes(x = gender, fill=gender)) + 
  geom_bar() + 
  scale_fill_brewer(palette = "Set3", name="Gender",
                    breaks=c("M", "F", "TG", "GQ", "Refused", "NA"),
                    labels=c("Male", "Female", "Transgender", "Gender Queer", "Refused", "NA")) + 
  ggtitle('Frequency distribution by gender') +
  xlab('Gender') +
  ylab('Count')

# Summary of age distribution
ggplot(Age_subset, aes(x = age,fill=age)) + 
  geom_bar(aes(fill=age),fill="#BEBADA") +
  ggtitle('Frequency distribution by Age') +
  xlab('Age') +
  ylab('Count')
  

ggplot(Age_subset) +
  geom_boxplot(aes(x = "", y = age)) +
  ggtitle('Boxplot of respondent age') +
  ylab('Age') +
  xlab('')

# Respondent country summary
summary(data$country.living)
plot.country.data <- data %>%
  group_by(country.living) %>%
  summarize(count = n()) %>%
  top_n(n=10, wt=count) %>%
  arrange(desc(count))

ggplot(plot.country.data, aes(x = reorder(country.living, count), y= count, fill=country.living )) + 
  geom_bar(stat = 'identity') + 
  scale_fill_brewer(palette = "Set3") + 
  ggtitle('Frequency distribution for top 10 Countries') +
  xlab('Country') +
  ylab('Count') + 
  theme(legend.position="none") +
  coord_flip()

# Summary of company size distribution

# filter the missing responses
company.size <- data %>% filter(num.employees != '')
summary(company.size$num.employees)

# ordering the factors
company.size$num.employees <- factor(company.size$num.employees, levels = c('1-5','6-25','26-100','100-500','500-1000','More than 1000'))

ggplot(company.size, aes(x = num.employees, fill=num.employees)) + 
  geom_bar() + scale_fill_brewer(palette = "Set3") + 
  ggtitle('Frequency distribution of respondents by company size') +
  xlab('Number of Employees in Company') +
  ylab('Count')

# Analysis of self-employment and diagnosis of disorder

all <- data %>% group_by(self.employed, current.disorder)

# calculate frequencies
forPlotting <- all %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

# show freqs
forPlotting
# plot relative frequencies with a stacked bar plot filled by current diagnosis
gg <- ggplot(forPlotting, aes(x = self.employed, y = freq, fill = current.disorder)) + 
  geom_bar(stat = "identity",position = "stack") +
  ggtitle("Self-employed respondents appear more likely to suffer from mental illness") +
  xlab("Self-employment status") +
  ylab("Relative frequency") +
  guides(fill=guide_legend(title=NULL)) +
  scale_fill_brewer(palette = "Set3")+
  theme_bw()

ggplotly(gg)

# check for statistical significance
chisq.test(table(all$self.employed, all$current.disorder))

# Relation between effective treatment and impact on work

# ordering the factors
data$treatment.affects.work <- factor(data$treatment.affects.work, levels = c('Not applicable to me', 'Never', 'Rarely','Sometimes','Often'))

tekkies.grouped <- data %>% group_by(treatment.affects.work)
forPlotting <- tekkies.grouped %>% summarise(n = n()) %>% mutate(freq = n / sum(n))

plot1 <- ggplot(forPlotting, aes(x = '', y = freq, fill = treatment.affects.work)) + 
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Blues", breaks=levels(data$treatment.affects.work)) +
  ggtitle("Tech Workers Being Treated Effectively") +
  xlab("") +
  ylab("Work Affected") +
  guides(fill=guide_legend(title='Work Affected Frequency'))

# ordering the factors
data$no.treatment.affects.work <- factor(data$no.treatment.affects.work, levels = c('Not applicable to me', 'Never', 'Rarely','Sometimes','Often'))
tekkies.grouped2 <- data %>% group_by(no.treatment.affects.work)
forPlotting <- tekkies.grouped2 %>% summarise(n = n()) %>% mutate(freq = n / sum(n))

plot2 <- ggplot(forPlotting, aes(x = '', y = freq, fill = no.treatment.affects.work)) + 
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Blues", breaks=levels(data$no.treatment.affects.work)) +
  ggtitle("Tech Workers Not Being Treated Effectively") +
  xlab("") +
  ylab("Work Affected") +
  guides(fill=guide_legend(title='Work Affected Frequency'))

grid.arrange(plot1, plot2, ncol=2)

# Exploring the relation between family history and diagnosis of mental disorder
levels(data$current.disorder)
levels(data$family.history)

# Filter out respondents who respondend with 'Maybe'
current.disorder.filtered <- data %>% filter(current.disorder != 'Maybe')

# Plot the relation
data$family.history <- factor(data$family.history, levels = c('I don\'t know', 'No', 'Yes'))
family.history.grouped <- data %>% filter(current.disorder != 'Maybe') %>% group_by(current.disorder, family.history)
family.history.plot.data <- family.history.grouped %>% summarise(n = n()) %>% mutate(freq = n/sum(n))
ggplot(family.history.plot.data, aes(x = current.disorder, y = freq, fill = family.history)) + 
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set3")+
  ggtitle("Family History & Incidence of Mental Health Issues") +
  xlab("Suffering from a Mental Health Disorder") +
  ylab("Family History Proportion") +
  guides(fill=guide_legend(title='Family History'))

# Pick family history 'No' as baseline
current.disorder.filtered$family.history <- relevel(current.disorder.filtered$family.history, "No")

# Run logistic regression
mod <- glm(current.disorder ~ family.history, data = current.disorder.filtered, family = "binomial" )
summary(mod)

