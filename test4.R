library(dplyr)
library(stringr)
library(ggplot2)
library(plotly)
library(wesanderson)
set.seed(1000)
mental.health <- read.csv("mental-heath.csv", header = TRUE, stringsAsFactors = TRUE)
new.names <- c("self.employed", "num.employees", "tech.company", "tech.role", "mental.health.coverage", "mental.health.options", "mental.health.formally.discussed", "mental.health.resources", "anonymity.protected", "medical.leave", "mental.health.negative", "physical.health.negative", "mental.health.comfort.coworker", "mental.health.comfort.supervisor", "mental.health.taken.seriously", "coworker.negative.consequences", "private.med.coverage", "resources", "reveal.diagnosis.clients.or.business", "revealed.negative.consequences.CB", "reveal.diagnosis.coworkers", "revealed.negative.consequences.CW", "productivity.effected", "percentage", "previous.employer", "prevemp.mental.health.coverage", "prevemp.mental.health.options", "prevemp.mental.health.formally.discussed", "prevemp.mental.health.resources", "prevemp.anonymity.protected", "prevemp.mental.health.negative",
               "prevemp.physical.health.negative", "prevemp.mental.health.coworker", "prevemp.mental.health.comfort.supervisor", "prevemp.mental.health.taken.seriously", "prevemp.coworker.negative.consequences", "mention.phsyical.issue.interview", "why.whynot.physical", "mention.mental.health.interview", "why.whynot.mental", "career.hurt", "viewed.negatively.by.coworkers", "share.with.family", "observed.poor.handling", "observations.lead.less.likely.to.reveal", "family.history", "ever.had.mental.disorder", "currently.have.mental.disorder", "if.yes.what", "if.maybe.what", "medical.prof.diagnosis", "what.conditions", "sought.prof.treatment", "treatment.affects.work", "no.treatment.affects.work", "age", "gender", "country.live", "US.state", "country.work", "state.work", "work.position", "remotely"  )
colnames(mental.health) <- new.names
mental.health$why.whynot.physical <- as.character(mental.health$why.whynot.physical)
mental.health$why.whynot.mental <- as.character(mental.health$why.whynot.mental)
mental.health$gender <- as.character(mental.health$gender)
mental.health$work.position <- as.character(mental.health$work.position)

mental.health$self.employed <- as.factor(mental.health$self.employed)
levels(mental.health$self.employed) <- c("No", "Yes")
mental.health$tech.role <- as.factor(mental.health$tech.role)
levels(mental.health$tech.role) <- c("No", "Yes")

mental.health[mental.health$gender == "Male", "gender"] <- "M"
mental.health[mental.health$gender == "male", "gender"] <- "M"
mental.health[mental.health$gender == "MALE", "gender"] <- "M"
mental.health[mental.health$gender == "Man", "gender"] <- "M"
mental.health[mental.health$gender == "man", "gender"] <- "M"
mental.health[mental.health$gender == "m", "gender"] <- "M"
mental.health[mental.health$gender == "man ", "gender"] <- "M"
mental.health[mental.health$gender == "Dude", "gender"] <- "M"
mental.health[mental.health$gender == "mail", "gender"] <- "M"
mental.health[mental.health$gender == "M|", "gender"] <- "M"
mental.health[mental.health$gender == "Cis male", "gender"] <- "M"
mental.health[mental.health$gender == "Male (cis)", "gender"] <- "M"
mental.health[mental.health$gender == "Cis Male", "gender"] <- "M"
mental.health[mental.health$gender == "cis male", "gender"] <- "M"
mental.health[mental.health$gender == "cisdude", "gender"] <- "M"
mental.health[mental.health$gender == "cis man", "gender"] <- "M"
mental.health[mental.health$gender == "Male.", "gender"] <- "M"
mental.health[mental.health$gender == "Male ", "gender"] <- "M"
mental.health[mental.health$gender == "male ", "gender"] <- "M"
mental.health[mental.health$gender == "Malr", "gender"] <- "M"
mental.health[841,"gender"] <- "M"
mental.health[mental.health$gender == "Female", "gender"] <- "F"
mental.health[mental.health$gender == "Female ", "gender"] <- "F"
mental.health[mental.health$gender == " Female", "gender"] <- "F"
mental.health[mental.health$gender == "female", "gender"] <- "F"
mental.health[mental.health$gender == "female ", "gender"] <- "F"
mental.health[mental.health$gender == "Woman", "gender"] <- "F"
mental.health[mental.health$gender == "woman", "gender"] <- "F"
mental.health[mental.health$gender == "f", "gender"] <- "F"
mental.health[mental.health$gender == "Cis female", "gender"] <- "F"
mental.health[mental.health$gender == "Cis female ", "gender"] <- "F"
mental.health[mental.health$gender == "Cisgender Female", "gender"] <- "F"
mental.health[mental.health$gender == "Cis-woman", "gender"] <- "F"
mental.health[mental.health$gender == "fem", "gender"] <- "F"
mental.health[1091, "gender"] <- "F"
mental.health[17, "gender"] <- "F"
mental.health[mental.health$gender == "Agender", "gender"] <- "GQ"
mental.health[mental.health$gender == "Androgynous", "gender"] <- "GQ"
mental.health[mental.health$gender == "Bigender", "gender"] <- "GQ"
mental.health[mental.health$gender == "Female or Multi-Gender Femme", "gender"] <- "GQ"
mental.health[mental.health$gender == "female-bodied; no feelings about gender", "gender"] <- "GQ"
mental.health[mental.health$gender == "Fluid", "gender"] <- "GQ"
mental.health[mental.health$gender == "fm", "gender"] <- "GQ"
mental.health[mental.health$gender == "GenderFluid", "gender"] <- "GQ"
mental.health[mental.health$gender == "GenderFluid (born female)", "gender"] <- "GQ"
mental.health[mental.health$gender == "Genderflux demi-girl", "gender"] <- "GQ"
mental.health[mental.health$gender == "genderqueer", "gender"] <- "GQ"
mental.health[mental.health$gender == "Genderqueer", "gender"] <- "GQ"
mental.health[mental.health$gender == "fm", "gender"] <- "GQ"
mental.health[mental.health$gender == "genderqueer woman", "gender"] <- "GQ"
mental.health[mental.health$gender == "human", "gender"] <- "GQ"
mental.health[mental.health$gender == "Human", "gender"] <- "GQ"
mental.health[mental.health$gender == "Unicorn", "gender"] <- "GQ"
mental.health[mental.health$gender == "Male/genderqueer", "gender"] <- "GQ"
mental.health[mental.health$gender == "nb masculine", "gender"] <- "GQ"
mental.health[mental.health$gender == "non-binary", "gender"] <- "GQ"
mental.health[mental.health$gender == "Nonbinary", "gender"] <- "GQ"
mental.health[mental.health$gender == "AFAB", "gender"] <- "GQ"
mental.health[mental.health$gender == "Male (trans, FtM)", "gender"] <- "TG"
mental.health[mental.health$gender == "Transgender woman", "gender"] <- "TG"
index <- which(mental.health$gender != "M" & mental.health$gender != "F" & mental.health$gender != "GQ" & mental.health$gender != "TG" & mental.health$gender != "")
mental.health[index, "gender"]
last.genders <- c("F", "TG", "GQ", "GQ", "F", "GQ", "GQ", "GQ", "M", "Refused", "GQ", "GQ", "GQ", "TG", NA)
mental.health[index, "gender"] <- last.genders
table(mental.health$gender)
mental.health$gender <- as.factor(mental.health$gender)

PlotBars <- function(Var,Name){
  qplot(factor(Var),
        data=mental.health,geom="bar",fill=factor(Var),main=Name)
} 
PlotBars(mental.health$mental.health.coverage,"Does the workplace include mental health coverage?")

tekkies <- mental.health %>% filter(!is.na(tech.role)) 
nrow(tekkies)
tekkies <- tekkies[tekkies$tech.role == "Yes", ]

#comfort sharing with coworkers
tekkies.grouped <- tekkies %>% filter(!is.na(sought.prof.treatment))  %>% group_by(sought.prof.treatment, treatment.affects.work)
forPlotting <- tekkies.grouped %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))
forPlotting
gg <- ggplot(forPlotting, aes(x = sought.prof.treatment, y = freq, fill = treatment.affects.work)) + 
  geom_bar(stat = "identity") +
  ggtitle("Tech role workers treatment and work affected") +
  xlab("Being treated effectively") +
  ylab("work affected or not") +
  guides(fill=guide_legend(title=NULL)) +
  theme_bw()
ggplotly(gg)


plot(mental.health$currently.have.mental.disorder)
plot(mental.health$family.history)
mod <- glm(currently.have.mental.disorder ~ family.history, data = mental.health, family = "binomial" )
summary(mod)
mental.health1 <- mental.health %>% filter(currently.have.mental.disorder!='')
mental.health2 <- mental.health1 %>% filter(!is.na(family.history))
levels(mental.health1$currently.have.mental.disorder)

mod1 <- glm(mental.health.comfort.coworker ~ prevemp.anonymity.protected, data = mental.health, family = "binomial" )
summary(mod1)

mod2 <- glm(mental.health.comfort.supervisor ~ prevemp.anonymity.protected, data = mental.health, family = "binomial" )
summary(mod2)



hist(mental.health$age)
plot(mental.health$gender)

chisq.test(table(mental.health$sought.prof.treatment, mental.health$treatment.affects.work))

