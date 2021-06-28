###Global Actors Experiments
###Harold Walbert
###hwalbert@gmu.edu


###Need to install the required package and then load and attach the package
#install.packages("RNetLogo")
library(dissPkg)
library(RNetLogo)

#Select the location of your NetLogo program
NLStart("C:/Program Files/NetLogo 6.0.3/app", nl.jarname = "netlogo-6.0.3.jar")
#Load the NetLogo model...will have to manually select the model becuase the above line of code seems to change the working directory
# NLLoadModel("/GlobalAgentModel_JASSS Version For Diss.nlogo")
NLLoadModel(choose.files(multi = F, caption = "Select the Global Agent Model downloaded from the GitHub diss_repo"))


# NLCommand("nw:load-graphml (C:/Users/Harold/Desktop/CoWR/CoWR1960defense.graphml)")
# NLCommand("set Year 1960")
###This is where the experiments for the Global Actors Paper are created
###Experiments are run on four years: 1945, 1960, 1980 & 2007
###The parameter sweep is run through the tribute-rate amount at levels of .25, .5 & .75
###This value is to set the number of repetitions to run the experiment
NumberOfRepetitions <- 100
numberOfTicks <- 100
NLCommand("set output-csv? FALSE")

#########1945############
NLCommand("set SelectedYear 1945")
NLCommand("set tribute-rate .25")
Winners1945_25 <- data.frame(row.names = c(1:6))
for (i in 1:NumberOfRepetitions)
{
  NLCommand("setupYearGraph")
  NLDoCommand(numberOfTicks, "runOneYear")
  people <- NLGetAgentSet(c("StateNme","wealth"),"turtles", as.data.frame=TRUE)
  people <- arrange(people, desc(wealth))
  Winners <- as.data.frame(head(people$StateNme))
  Winners1945_25 <- as.data.frame(bind_cols(Winners1945_25, Winners))
}

Winners1945_25 <- as.data.frame(t(Winners1945_25))

###Leave year as 1945 but change tribute-rate to .5
NLCommand("set tribute-rate .5")
Winners1945_5 <- data.frame(row.names = c(1:6))
for (i in 1:NumberOfRepetitions)
{
  NLCommand("setupYearGraph")
  NLDoCommand(numberOfTicks, "runOneYear")
  people <- NLGetAgentSet(c("StateNme","wealth"),"turtles", as.data.frame=TRUE)
  people <- arrange(people, desc(wealth))
  Winners <- as.data.frame(head(people$StateNme))
  Winners1945_5 <- as.data.frame(bind_cols(Winners1945_5, Winners))
}

Winners1945_5 <- as.data.frame(t(Winners1945_5))

NLCommand("set tribute-rate .75")
Winners1945_75 <- data.frame(row.names = c(1:6))
for (i in 1:NumberOfRepetitions)
{
  NLCommand("setupYearGraph")
  NLDoCommand(numberOfTicks, "runOneYear")
  people <- NLGetAgentSet(c("StateNme","wealth"),"turtles", as.data.frame=TRUE)
  people <- arrange(people, desc(wealth))
  Winners <- as.data.frame(head(people$StateNme))
  Winners1945_75 <- as.data.frame(bind_cols(Winners1945_75, Winners))
}

Winners1945_75 <- as.data.frame(t(Winners1945_75))

#########1960############

#########Set the year to 1960
NLCommand("set SelectedYear 1960")
NLCommand("set tribute-rate .25")
Winners1960_25 <- data.frame(row.names = c(1:6))
for (i in 1:NumberOfRepetitions)
{
  NLCommand("setupYearGraph")
  NLDoCommand(numberOfTicks, "runOneYear")
  people <- NLGetAgentSet(c("StateNme","wealth"),"turtles", as.data.frame=TRUE)
  people <- arrange(people, desc(wealth))
  Winners <- as.data.frame(head(people$StateNme))
  Winners1960_25 <- as.data.frame(bind_cols(Winners1960_25, Winners))
}

Winners1960_25 <- as.data.frame(t(Winners1960_25))


NLCommand("set tribute-rate .5")
Winners1960_5 <- data.frame(row.names = c(1:6))
for (i in 1:NumberOfRepetitions)
{
  NLCommand("setupYearGraph")
  NLDoCommand(numberOfTicks, "runOneYear")
  people <- NLGetAgentSet(c("StateNme","wealth"),"turtles", as.data.frame=TRUE)
  people <- arrange(people, desc(wealth))
  Winners <- as.data.frame(head(people$StateNme))
  Winners1960_5 <- as.data.frame(bind_cols(Winners1960_5, Winners))
}

Winners1960_5 <- as.data.frame(t(Winners1960_5))


NLCommand("set tribute-rate .75")
Winners1960_75 <- data.frame(row.names = c(1:6))
for (i in 1:NumberOfRepetitions)
{
  NLCommand("setupYearGraph")
  NLDoCommand(numberOfTicks, "runOneYear")
  people <- NLGetAgentSet(c("StateNme","wealth"),"turtles", as.data.frame=TRUE)
  people <- arrange(people, desc(wealth))
  Winners <- as.data.frame(head(people$StateNme))
  Winners1960_75 <- as.data.frame(bind_cols(Winners1960_75, Winners))
}

Winners1960_75 <- as.data.frame(t(Winners1960_75))

#########1980############

#########Set the year to 1980
NLCommand("set SelectedYear 1980")
NLCommand("set tribute-rate .25")
Winners1980_25 <- data.frame(row.names = c(1:6))
for (i in 1:NumberOfRepetitions)
{
  NLCommand("setupYearGraph")
  NLDoCommand(numberOfTicks, "runOneYear")
  people <- NLGetAgentSet(c("StateNme","wealth"),"turtles", as.data.frame=TRUE)
  people <- arrange(people, desc(wealth))
  Winners <- as.data.frame(head(people$StateNme))
  Winners1980_25 <- as.data.frame(bind_cols(Winners1980_25, Winners))
}

Winners1980_25 <- as.data.frame(t(Winners1980_25))

NLCommand("set tribute-rate .5")
Winners1980_5 <- data.frame(row.names = c(1:6))
for (i in 1:NumberOfRepetitions)
{
  NLCommand("setupYearGraph")
  NLDoCommand(numberOfTicks, "runOneYear")
  people <- NLGetAgentSet(c("StateNme","wealth"),"turtles", as.data.frame=TRUE)
  people <- arrange(people, desc(wealth))
  Winners <- as.data.frame(head(people$StateNme))
  Winners1980_5 <- as.data.frame(bind_cols(Winners1980_5, Winners))
}

Winners1980_5 <- as.data.frame(t(Winners1980_5))

NLCommand("set tribute-rate .75")
Winners1980_75 <- data.frame(row.names = c(1:6))
for (i in 1:NumberOfRepetitions)
{
  NLCommand("setupYearGraph")
  NLDoCommand(numberOfTicks, "runOneYear")
  people <- NLGetAgentSet(c("StateNme","wealth"),"turtles", as.data.frame=TRUE)
  people <- arrange(people, desc(wealth))
  Winners <- as.data.frame(head(people$StateNme))
  Winners1980_75 <- as.data.frame(bind_cols(Winners1980_75, Winners))
}

Winners1980_75 <- as.data.frame(t(Winners1980_75))


#########2007############

#########Set the year to 2007
NLCommand("set SelectedYear 2007")
NLCommand("set tribute-rate .25")
Winners2007_25 <- data.frame(row.names = c(1:6))
for (i in 1:NumberOfRepetitions)
{
  NLCommand("setupYearGraph")
  NLDoCommand(numberOfTicks, "runOneYear")
  people <- NLGetAgentSet(c("StateNme","wealth"),"turtles", as.data.frame=TRUE)
  people <- arrange(people, desc(wealth))
  Winners <- as.data.frame(head(people$StateNme))
  Winners2007_25 <- as.data.frame(bind_cols(Winners2007_25, Winners))
}

Winners2007_25 <- as.data.frame(t(Winners2007_25))


NLCommand("set tribute-rate .5")
Winners2007_5 <- data.frame(row.names = c(1:6))
for (i in 1:NumberOfRepetitions)
{
  NLCommand("setupYearGraph")
  NLDoCommand(numberOfTicks, "runOneYear")
  people <- NLGetAgentSet(c("StateNme","wealth"),"turtles", as.data.frame=TRUE)
  people <- arrange(people, desc(wealth))
  Winners <- as.data.frame(head(people$StateNme))
  Winners2007_5 <- as.data.frame(bind_cols(Winners2007_5, Winners))
}

Winners2007_5 <- as.data.frame(t(Winners2007_5))


NLCommand("set tribute-rate .75")
Winners2007_75 <- data.frame(row.names = c(1:6))
for (i in 1:NumberOfRepetitions)
{
  NLCommand("setupYearGraph")
  NLDoCommand(numberOfTicks, "runOneYear")
  people <- NLGetAgentSet(c("StateNme","wealth"),"turtles", as.data.frame=TRUE)
  people <- arrange(people, desc(wealth))
  Winners <- as.data.frame(head(people$StateNme))
  Winners2007_75 <- as.data.frame(bind_cols(Winners2007_75, Winners))
}

Winners2007_75 <- as.data.frame(t(Winners2007_75))

PowerCountries1945_25 <- summarise(group_by(Winners1945_25, V1, V2), Count = n()) %>% arrange(desc(Count)) %>% mutate(Year = 1945, Tribute = .25)
PowerCountries1960_25 <- summarise(group_by(Winners1960_25, V1, V2), Count = n()) %>% arrange(desc(Count)) %>% mutate(Year = 1960, Tribute = .25)
PowerCountries1980_25 <- summarise(group_by(Winners1980_25, V1, V2), Count = n()) %>% arrange(desc(Count)) %>% mutate(Year = 1980, Tribute = .25)
PowerCountries2007_25 <- summarise(group_by(Winners2007_25, V1, V2), Count = n()) %>% arrange(desc(Count)) %>% mutate(Year = 2007, Tribute = .25)

PowerCountries1945_5 <- summarise(group_by(Winners1945_5, V1, V2), Count = n()) %>% arrange(desc(Count)) %>% mutate(Year = 1945, Tribute = .5)
PowerCountries1960_5 <- summarise(group_by(Winners1960_5, V1, V2), Count = n()) %>% arrange(desc(Count)) %>% mutate(Year = 1960, Tribute = .5)
PowerCountries1980_5 <- summarise(group_by(Winners1980_5, V1, V2), Count = n()) %>% arrange(desc(Count)) %>% mutate(Year = 1980, Tribute = .5)
PowerCountries2007_5 <- summarise(group_by(Winners2007_5, V1, V2), Count = n()) %>% arrange(desc(Count)) %>% mutate(Year = 2007, Tribute = .5)

PowerCountries1945_75 <- summarise(group_by(Winners1945_75, V1, V2), Count = n()) %>% arrange(desc(Count)) %>% mutate(Year = 1945, Tribute = .75)
PowerCountries1960_75 <- summarise(group_by(Winners1960_75, V1, V2), Count = n()) %>% arrange(desc(Count)) %>% mutate(Year = 1960, Tribute = .75)
PowerCountries1980_75 <- summarise(group_by(Winners1980_75, V1, V2), Count = n()) %>% arrange(desc(Count)) %>% mutate(Year = 1980, Tribute = .75)
PowerCountries2007_75 <- summarise(group_by(Winners2007_75, V1, V2), Count = n()) %>% arrange(desc(Count)) %>% mutate(Year = 2007, Tribute = .75)

allResults <- bind_rows(
  PowerCountries1945_25, PowerCountries1945_5, PowerCountries1945_75,
  PowerCountries1960_25, PowerCountries1960_5, PowerCountries1960_75,
  PowerCountries1980_25, PowerCountries1980_5, PowerCountries1980_75,
  PowerCountries2007_25, PowerCountries2007_5, PowerCountries2007_75
)

save.image("~/DissertationCode/Data/GlobalAgents Experiment Results/Global Agents Experiment Results.RData")
write.csv(x = allResults, file = "~/DissertationCode/Data/GlobalAgents Experiment Results/Global Agents Experiment Results Powerful Countries.csv", row.names = F)

summarize(group_by(allResults, V1, Year), Count = sum(Count)) %>% arrange(desc(Count))
