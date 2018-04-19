###### PACKAGES & LIBRARIES ######

library('plyr')
library('dplyr')
library('ggplot2')
library('ggmap')
library('maps')
library('mapmdat')
library('RColorBrewer')

###### READING THE DATASET ######

mentalHealth <- read.csv('mentalhealth.csv')                   # Reading the dataset

head(mentalHealth)                                                                   # Glimpse of the dataset
colnames(mentalHealth)                                                               # Columns in the dataset
dim(mentalHealth)                                                                    # Dimensions of the dataset


###### DATASET PREPARATION ######

# RENAMING COLUMNS & CREATING A DATAFRAME

df <- data.frame(mentalHealth)

mdat <- rename(df, 
              work.country = What.country.do.you.work.in., 
              work.us.state = What.US.state.or.territory.do.you.work.in., 
              age = What.is.your.age.,
              gender = What.is.your.gender.,
              discuss.supervisor = Would.you.feel.comfortable.discussing.a.mental.health.disorder.with.your.direct.supervisor.s..,
              discuss.coworker = Would.you.feel.comfortable.discussing.a.mental.health.disorder.with.your.coworkers.,
              leave.sanction = If.a.mental.health.issue.prompted.you.to.request.a.medical.leave.from.work..asking.for.that.leave.would.be.,
              employee.count= How.many.employees.does.your.company.or.organization.have.,
              mental.health.options = Do.you.know.the.options.for.mental.health.care.available.under.your.employer.provided.coverage.,
              current.mental.disorder = Do.you.currently.have.a.mental.health.disorder.,
              self.employed = Are.you.self.employed.,
              company.resources = Does.your.employer.offer.resources.to.learn.more.about.mental.health.concerns.and.options.for.seeking.help.,
              mental.health.coverage = Does.your.employer.provide.mental.health.benefits.as.part.of.healthcare.coverage.,
              prev.anonymity.protected = Was.your.anonymity.protected.if.you.chose.to.take.advantage.of.mental.health.or.substance.abuse.treatment.resources.with.previous.employers.,
              family.history = Do.you.have.a.family.history.of.mental.illness.)

mdat %>% select(age,gender,work.country,work.us.state,leave.sanction,discuss.supervisor,discuss.coworker,employee.count,mental.health.options,current.mental.disorder,self.employed,company.resources,mental.health.coverage,prev.anonymity.protected,family.history) %>% str()


# CLEANING GENDER COLUMN #

mdat$gender <- as.character(mdat$gender)                         # Converting gender column to character type

table(is.na(mdat$gender))                                        # Checking for null values


# Cleaning Male values
mdat[mdat$gender == "Male", "gender"] <- "M"
mdat[mdat$gender == "male", "gender"] <- "M"
mdat[mdat$gender == "MALE", "gender"] <- "M"
mdat[mdat$gender == "Man", "gender"] <- "M"
mdat[mdat$gender == "man", "gender"] <- "M"
mdat[mdat$gender == "m", "gender"] <- "M"
mdat[mdat$gender == "man ", "gender"] <- "M"
mdat[mdat$gender == "Dude", "gender"] <- "M"
mdat[mdat$gender == "mail", "gender"] <- "M"
mdat[mdat$gender == "M|", "gender"] <- "M"
mdat[mdat$gender == "Cis male", "gender"] <- "M"
mdat[mdat$gender == "Male (cis)", "gender"] <- "M"
mdat[mdat$gender == "Cis Male", "gender"] <- "M"
mdat[mdat$gender == "cis male", "gender"] <- "M"
mdat[mdat$gender == "cisdude", "gender"] <- "M"
mdat[mdat$gender == "cis man", "gender"] <- "M"
mdat[mdat$gender == "Male.", "gender"] <- "M"
mdat[mdat$gender == "Male ", "gender"] <- "M"
mdat[mdat$gender == "male ", "gender"] <- "M"
mdat[mdat$gender == "Malr", "gender"] <- "M"
mdat[841,"gender"] <- "M"

# Cleaning Female values
mdat[mdat$gender == "Female", "gender"] <- "F"
mdat[mdat$gender == "Female ", "gender"] <- "F"
mdat[mdat$gender == " Female", "gender"] <- "F"
mdat[mdat$gender == "female", "gender"] <- "F"
mdat[mdat$gender == "female ", "gender"] <- "F"
mdat[mdat$gender == "Woman", "gender"] <- "F"
mdat[mdat$gender == "woman", "gender"] <- "F"
mdat[mdat$gender == "f", "gender"] <- "F"
mdat[mdat$gender == "Cis female", "gender"] <- "F"
mdat[mdat$gender == "Cis female ", "gender"] <- "F"
mdat[mdat$gender == "Cisgender Female", "gender"] <- "F"
mdat[mdat$gender == "Cis-woman", "gender"] <- "F"
mdat[mdat$gender == "fem", "gender"] <- "F"
mdat[1091, "gender"] <- "F"
mdat[17, "gender"] <- "F"

# Cleaning Gender Queer Values
mdat[!is.na(mdat$gender)&mdat$gender == "Agender", "gender"] <- "GQ"
mdat[!is.na(mdat$gender)&mdat$gender == "Androgynous", "gender"] <- "GQ"
mdat[!is.na(mdat$gender)&mdat$gender == "Bigender", "gender"] <- "GQ"
mdat[!is.na(mdat$gender)&mdat$gender == "Female or Multi-Gender Femme", "gender"] <- "GQ"
mdat[!is.na(mdat$gender)&mdat$gender == "female-bodied; no feelings about gender", "gender"] <- "GQ"
mdat[!is.na(mdat$gender)&mdat$gender == "Fluid", "gender"] <- "GQ"
mdat[!is.na(mdat$gender)&mdat$gender == "fm", "gender"] <- "GQ"
mdat[!is.na(mdat$gender)&mdat$gender == "GenderFluid", "gender"] <- "GQ"
mdat[!is.na(mdat$gender)&mdat$gender == "GenderFluid (born female)", "gender"] <- "GQ"
mdat[!is.na(mdat$gender)&mdat$gender == "Genderflux demi-girl", "gender"] <- "GQ"
mdat[!is.na(mdat$gender)&mdat$gender == "genderqueer", "gender"] <- "GQ"
mdat[!is.na(mdat$gender)&mdat$gender == "Genderqueer", "gender"] <- "GQ"
mdat[!is.na(mdat$gender)&mdat$gender == "fm", "gender"] <- "GQ"
mdat[!is.na(mdat$gender)&mdat$gender == "genderqueer woman", "gender"] <- "GQ"
mdat[!is.na(mdat$gender)&mdat$gender == "human", "gender"] <- "GQ"
mdat[!is.na(mdat$gender)&mdat$gender == "Human", "gender"] <- "GQ"
mdat[!is.na(mdat$gender)&mdat$gender == "Unicorn", "gender"] <- "GQ"
mdat[!is.na(mdat$gender)&mdat$gender == "Male/genderqueer", "gender"] <- "GQ"
mdat[!is.na(mdat$gender)&mdat$gender == "nb masculine", "gender"] <- "GQ"
mdat[!is.na(mdat$gender)&mdat$gender == "non-binary", "gender"] <- "GQ"
mdat[!is.na(mdat$gender)&mdat$gender == "Nonbinary", "gender"] <- "GQ"
mdat[!is.na(mdat$gender)&mdat$gender == "AFAB", "gender"] <- "GQ"

# Cleaning Transgender values
mdat[!is.na(mdat$gender)&mdat$gender == "Male (trans, FtM)", "gender"] <- "TG"
mdat[!is.na(mdat$gender)&mdat$gender == "Transgender woman", "gender"] <- "TG"


# see what's left
index <- which(mdat$gender != "M" & mdat$gender != "F" & mdat$gender != "GQ" & mdat$gender != "TG")

mdat[index, "gender"]

# create vector of final gender values to fill in based on index
last.genders <- c("F", "TG", "GQ", "GQ", "F", "GQ", "GQ", "GQ", "M", "Refused", "GQ", "GQ", "GQ", "TG", "GQ", NA)

# fill in remaining values
mdat[index, "gender"] <- last.genders

# check gender
table(data$gender)

# convert gender back to factor
mdat$gender <- as.factor(mdat$gender)

# Plotting gender with frequency
ggplot(mdat, aes(x = gender, fill=gender)) + 
  geom_bar() + 
  scale_fill_brewer(palette = "Dark2", name="Gender",
                    breaks=c("M", "F", "TG", "GQ", "Refused", "NA"),
                    labels=c("Male", "Female", "Transgender", "Gender Queer", "Refused", "NA")) + 
  ggtitle('Frequency distribution by gender') +
  xlab('Gender') +
  ylab('Count')


# CLEANING aGE COLUM #

summary(mdat$age)
# Filter records with age 3 and 323
mdat1 <- mdat %>% filter(age != 323 & age !=3)
summary(mdat1$age)
# Max age is still 99
mdat1 <- mdat %>% filter(age > 15 & age <= 80)
summary(mdat1$age)
dim(mdat1)

# Summary of age distribution
ggplot(mdat1, aes(x = age, fill=age)) + 
  geom_bar() +
  ggtitle('Frequency distribution by Age') +
  xlab('Age') +
  ylab('Count')

ggplot(mdat1) +
  geom_boxplot(aes(x = "", y = age)) +
  ggtitle('Boxplot of respondent age') +
  ylab('Age') +
  xlab('')


mdat1 %>% select(age,gender,work.country,work.us.state,leave.sanction,discuss.supervisor,discuss.coworker,employee.count,mental.health.options,current.mental.disorder,self.employed,company.resources,mental.health.coverage,prev.anonymity.protected,family.history) %>% str()
plot(mdat1[1:5])

write.csv(mdat1, 'MentalHealthCleanedDataset.csv')
