###### PACKAGES & LIBRARIES ######

install.packages('RColorBrewer')
install.packages('maptools')
install.packages('sp')
install.packages('rworldmap')
install.packages('ggplot2')
install.packages('dplyr')


library('plyr')
library('dplyr')
library('ggplot2')
library('ggmap')
library('maps')
library('maptools')
library('RColorBrewer')

library(maps)
library(maptools)
library(sp)
library(rworldmap)
require(dplyr)
require(ggplot2)
require(tidyr)


gpclibPermit()

###### READING THE DATASET ######

mentalHealth <- read.csv('mental-heath-in-tech-2016_20161114.csv',header = TRUE, stringsAsFactors = TRUE)                   # Reading the dataset

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
table(mdat$gender)

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
dim(mdat)

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

dim(mdat1)


#### AGE GROUP AND Comfort level ####

# Analzing Age Feature

mdat1$age <-cut(mdat1$age, breaks = c(14,35,78), labels = c('Young','Senior'))

plot(table(mdat1$age))

ggplot(mdat1, aes(x =age, fill = age, group = age)) +
  geom_bar(position = 'dodge') + scale_fill_brewer(palette = "Set3")+ labs(title="Respondent Age Groups", x = "Age Group", y = "Total Number of Respondents")



# analyzing the discuss.supervisor feature

mdat2 <- mdat1 %>% filter(discuss.supervisor !="")
table(mdat2$discuss.supervisor)
mdat2$discuss.supervisor <- factor(mdat2$discuss.supervisor, levels = c("Maybe","Yes","No"))

ggplot(mdat2, aes(x =discuss.supervisor, fill = discuss.supervisor, group = discuss.supervisor)) +
  geom_bar(position = 'dodge') + scale_fill_brewer(palette = "Set3") + labs(title="Respondent's Comfort Level in discussion with Supervisor", x = "Comfort Level", y = "Total Number of Respondents")

summary(mdat2$discuss.supervisor)


#### Age with comfort level with Supervisor #####

plot(mdat2$discuss.supervisor,mdat2$age, col=brewer.pal(8, "Set3"), ylab="Age of the Respondent", xlab="Comfort level in discussion with Supervisor",main="Comfort level of Employees for discussing mental health issues with Supervisor with Age")

# Plot this without grouping age into Young and Senior
plot(mdat2$discuss.supervisor,mdat2$age, col=brewer.pal(8, "Set3"), ylab="Age of the Respondent", xlab="Comfort level in discussion with Supervisor",main="Comfort level of Employees for discussing mental health issues with Supervisor with Age")

ggplot(mdat2, aes(x=discuss.supervisor, fill=age, group=age)) + geom_bar(position = 'dodge') + scale_fill_brewer(palette = "Set3")


###### Chi-square test #####

library(MASS)

### Pearson's Chisq test ####

mdat1$age <-cut(mdat1$age, breaks = c(14,35,78), labels = c('Young','Senior'))

mdat2 <- mdat1 %>% filter(discuss.supervisor !="")
table(mdat2$discuss.supervisor)
mdat2$discuss.supervisor <- factor(mdat2$discuss.supervisor, levels = c("Maybe","Yes","No"))
mdat2$discuss.supervisor
t <- with(mdat2, table(discuss.supervisor, age))

chisq.test(t)

#### Multinomial Regression ####

install.packages('nnet')
library(nnet)

k <- with(mdat2, table(discuss.supervisor,age))
k

with(mdat2, do.call(rbind, tapply(age, function(x) c(M = mean(x), SD = sd(x)))))

mdat2$discuss.supervisor <- relevel(mdat2$discuss.supervisor, ref="No")

mod <- multinom(mdat2$discuss.supervisor~age, data = mdat2)

summary(mod)

z <- summary(mod)$coefficients/summary(mod)$standard.errors
z

# 2-tailed z test
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p





###### Model Using various factors #######

dim(mdat2)
mdat2$discuss.supervisor
mdat2$age
mdat2$employee.count
mdat2$prev.anonymity.protected
mdat2$mental.health.coverage

#### discuss.supervisor ####

mdat2 <- mdat1 %>% filter(discuss.supervisor !="" & discuss.supervisor != "Maybe")
table(mdat2$discuss.supervisor)
mdat2$discuss.supervisor <- factor(mdat2$discuss.supervisor, levels = c("Yes","No"))
levels(mdat2$discuss.supervisor)

#### employee count variable ######


mdat2$organization.size.large <- rep(0, nrow(mdat2))
mdat2$organization.size.large[mdat2$employee.count=="More than 1000"] <- "1"

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


######################################

mdat4$discuss.supervisor <- relevel(mdat4$discuss.supervisor, ref="No")

mdat4$prev.anonymity.protected <- relevel(mdat4$prev.anonymity.protected, ref="No")

mdat4$mental.health.coverage <- relevel(mdat4$mental.health.coverage, ref="No")


levels(mdat4$organization.size.large)

with(mdat4, table(discuss.supervisor,mental.health.coverage,prev.anonymity.protected,age))


mod1 <- glm(discuss.supervisor~age + organization.size.large + mental.health.coverage + prev.anonymity.protected, data=mdat4, family = "binomial")

summary(mod1)


z <- summary(mod1)$coefficients/summary(mod1)$standard.errors
z

# 2-tailed z test
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p


##### MOdel 2 ####

mod2 <- glm(discuss.supervisor~organization.size.large + mental.health.coverage + prev.anonymity.protected, data=mdat4, family="binomial")

summary(mod2)


z <- summary(mod2)$coefficients/summary(mod2)$standard.errors
z

# 2-tailed z test
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

y <- factor(mdat4$discuss.supervisor)

fits <- fitted(mod2)

library(AUC)
rr <- roc(fits,y)
plot(rr)
auc(rr)

####################### K Fold Validation ##################

library(caret)

Train <- createDataPartition(mdat4$discuss.supervisor, p=0.8, list=FALSE)

training <- mdat4[Train, ]
testing <- mdat4[-Train, ]

ctrl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)

mod_fit <- train(discuss.supervisor~organization.size.large + mental.health.coverage + prev.anonymity.protected,  data=mdat4, method="multinom",
                 trControl = ctrl, tuneLength = 5)


pred = predict(mod_fit, newdata=testing)
confusionMatrix(data=pred, testing$discuss.supervisor)

?train

###### Model 1 ######

Train <- createDataPartition(mdat4$discuss.supervisor, p=0.8, list=FALSE)

training <- mdat4[Train, ]
testing <- mdat4[-Train, ]

ctrl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)

mod_fit <- train(discuss.supervisor~organization.size.large + mental.health.coverage + prev.anonymity.protected,  data=mdat4, method="glm",
                 trControl = ctrl, tuneLength = 5)


pred = predict(mod_fit, newdata=testing)
confusionMatrix(data=pred, testing$discuss.supervisor)

##### model 3 ####

Train <- createDataPartition(mdat4$discuss.supervisor, p=0.8, list=FALSE)

training <- mdat4[Train, ]
testing <- mdat4[-Train, ]

ctrl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)

mod_fit <- train(discuss.supervisor~mental.health.coverage,  data=mdat4, method="multinom",
                 trControl = ctrl, tuneLength = 5)


pred = predict(mod_fit, newdata=testing)
confusionMatrix(data=pred, testing$discuss.supervisor)


##### 

fits <- fitted(mod2)

y <- factor(mdat4$discuss.supervisor)

rr <- roc(fits,y)

plot(rr)

auc(rr)










# analyzing the comfort level in asking for leave

mdat2 <- mdat1 %>% filter(mdat1$leave.sanction != "")

ggplot(mdat1, aes(x =leave.sanction, fill = age, group = age)) +
  geom_bar(position = 'dodge') 
plot(mdat2$leave.sanction, mdat2$age)





plot(mdat1$discuss.coworker, mdat1$age)

mdat2 <- mdat1 %>% filter(mdat1$leave.sanction != "")

ggplot(mdat1, aes(x =leave.sanction, fill = age, group = age)) +
  geom_bar(position = 'dodge') 
plot(mdat2$leave.sanction, mdat2$age)


ggplot(mdat2, aes(x =leave.sanction, fill = age, group = age)) +
  geom_bar(position = 'dodge') 



ggplot(mdat1, aes(x =discuss.supervisor, fill = age, group = age)) +
  geom_bar(position = 'dodge') + labs(title="Comfort level in discussing mental health with Supervisor Vs Age", x="Comfort level with Supervisor")

mod1 <- glm(age~discuss.supervisor, data=mdat1, family="binomial")

summary(mod1)

ggplot(mdat2, aes(x = discuss.supervisor, y = age)) + 
  geom_boxplot() +
  ggtitle("No age differences according to comfort with supervisor") +
  xlab("Comfort with supervisor") +
  ylab("age")  +
  theme_bw() + scale_fill_brewer(palette = "Set3")


###### Analyzing Country Specific response distribution ###########
mdat1<-read.csv("MentalHealthCleanedDataset.csv")

k <- table(mdat1$work.country)
l <- data.frame(k)
l
colnames(l) <- c("country","freq")
levels(l$country) <- tolower(levels(l$country))

mapDevice('x11')
spdf <- joinCountryData2Map(l, joinCode="NAME", nameJoinColumn="country")
mapCountryData(spdf, nameColumnToPlot="freq", catMethod="fixedWidth", mapTitle = "Survey Responses Distribution - World",colourPalette=c('yellow','white','brown'))

?mapCountryData

####### Analyzing US Specific reponse distribution ######

t <- table(mdat1$work.us.state)
t
w <- as.data.frame(t)
w <- data.frame(w[-c(1),])
colnames(w) <- c("state","freq")
levels(w$state) <- tolower(levels(w$state))
w

mapUSA <- map('state',  fill = TRUE,  plot = FALSE)
nms <- sapply(strsplit(mapUSA$names,  ':'),  function(x)x[1])
USApolygons <- map2SpatialPolygons(mapUSA,  IDs = nms,  CRS('+proj=longlat'))

as.character(w$Var1)
idx <- match(unique(nms),w$Var1)
dat2 <- data.frame(value = w$freq[idx], state = unique(nms))
row.names(dat2) <- unique(nms)

USAsp <- SpatialPolygonsDataFrame(USApolygons,  data = dat2)
spplot(USAsp['value'])

##### CRAP ####

##### QUESTON 1 ANALYSIS #####

ggplot(mdat1, aes(x = age), bins=0.5) + geom_histogram(binwidth = 0.5)

ggplot(mdat1, aes(x = discuss.coworker),colour='red') + geom_histogram(binwidth = 50,stat = "count")
ggplot(mdat1, aes(x = discuss.supervisor),colour='red') + geom_histogram(binwidth = 50,stat = "count")

r <- table(mdat1$discuss.coworker)
r

PlotBars <- function(Var,Name){qplot(factor(Var),
                                     data=mdat1,geom="bar",fill=factor(Var),main=Name)}
PlotBars(mdat1$discuss.supervisor,"Would.you.feel.comfortable.discussing.a.mental.health.disorder.with.your.supervisor.")

gender <- mdat1$gender
comfort<- mdat1$discuss.supervisor

mdat1 %>%select(gender,comfort)%>%group_by(gender,comfort)%>%summarise(c=n()) %>%
  ggplot(aes(x=mdat1$gender,y=c,fill=mdat1$current.mental.disorder))+
  geom_bar(stat="identity",position="dodge")+
  mytheme+
  labs(title="Gender vs Current Mental Health Disorder",y="count")+
  scale_fill_manual(values=colors)


ggplot(mdat1, aes(x =discuss.supervisor, fill = gender, group = gender, )) +
  geom_bar(position = 'dodge') + labs(title="Comfort level in Discussing mental health with Supervisor Vs Age")

?label
summary(mdat1$age)

summary(mdat1$discuss.supervisor)

mdat1 <- mdat1 %>% filter(discuss.supervisor == "Yes" & discuss.supervisor == "No" & discuss.supervisor == "Maybe")
dim(mdat1)

head(mdat1)

table(mdat1$Age)
mdat1$discuss.supervisor

plot(mdat1$discuss.supervisor,mdat1$Age)


summary(gender)


dim(mdat1)

ggplot(mdat1, aes(x =discuss.supervisor, fill = age, group = age)) +
  geom_bar(position = 'dodge')

ggplot(mdat1, aes(x =discuss.supervisor, fill = gender, group = gender)) +
  geom_bar(position = 'dodge')

ggplot(mdat1, aes(x = discuss.supervisor, fill = gender, group = gender)) +
  geom_bar(position = 'dodge') +
  facet_wrap(~gender)

