#Agents & Ideology####
#Harold Walbert
#haroldwalbert@gmail.com

#Getting Started####
#Load Libraries
library(readxl)
library(tidyverse)
library(cluster)
library(scales)
library(car)
library(rgl)
library(scatterplot3d)
library(plot3D)
library(ggplot2)
library(janitor)
library(DT)
library(kableExtra)
#Make sure you set your working directory to where you ahve the data saved
setwd("C:/Users/Owner/Documents/GitHub/Ideological-Agents")

#Get the data...one is the full dataset and one is the ID to Name crosswalk
CountryData <- readxl::read_xls(path = "Data for Analysis and Model/swank-party-data-update.xls", sheet = "Country Data")
#This is old data...using the new swank data!!!
#CountryData <- read.csv("PARTY19502011.csv")
CountryIDXWalk <- read.csv("Data for Analysis and Model/CountryIDXwalk.csv")

#Clean and Prep data####
CCD <- CountryData
CCD[CCD == '-999'] <- NA

#Replace coid with Country Name
CCD <- left_join(CCD, CountryIDXWalk, by = "coid")

#What types of data do we have?
#coid is the country & year is year, elect1 is election year dummy,they should be coded as a factor
CCD$Country <- as.factor(CCD$Country)
CCD$year <- as.factor(CCD$year)
CCD$elect1 <- as.factor(CCD$elect1)
#Dont care about election month or day & will remove duplicative coid column
CCD$elmon <- NULL; CCD$elday <- NULL; CCD$coid <- NULL

#Keep only rows with complete data
#When this was not done it resulted in the NAs causing the graph to have obvious errors due
#to the presence of the NAs
#Remove Rows with NA
CCD <- na.omit(CCD)


#Some tables of CCD
CountryYearTable <- data.frame(table(as.numeric(as.character(CCD$year)), CCD$Country))
CountryYearTable <- (reshape2::dcast(data = CountryYearTable, formula = Var1~Var2, fun.aggregate = sum))
names(CountryYearTable)[1] <- "Year"
# CountryYearTable[] <- lapply(X = CountryYearTable, FUN = )
CountryYearTable[CountryYearTable == 0] <- "NA"
CountryYearTable[CountryYearTable == 1] <- "A"
#kbl(CountryYearTable, row.names = F, caption = "Country/Year Data Availability: A=Available, NA=Not Available") %>% kable_classic(full_width = T)


#Data Analysis####

#We just made sure the data was put in the appropriate classes
#We can now use this cleaned dataset to create a distance matrix, see:
#Gower, J. C. (1971) A general coefficient of similarity and some of its properties, Biometrics 27, 857-874.
#Kaufman, L. and Rousseeuw, P.J. (1990) Finding Groups in Data: An Introduction to Cluster Analysis. Wiley, New York.

#This creates a distance matrix of the remaining columns,
#This daisy() function can create a distance matrix when there are different classes in the dataset
CCD_diff <- daisy(CCD, metric = "gower")
#We have to reduce the dimensionality. We will do that by using Prinical Component Analysis
PC_CCD <- prcomp(CCD_diff, scale = TRUE)

#Now that we have reduced dimensionality with PCA we can cluster
CCDclusters <- CCD
for (i in 1:20) {
  KM_CCD <- kmeans(CCD_diff, centers = i)
  CCDclusters[,(length(CCD) + i)] <- as.factor(KM_CCD$cluster)
  names(CCDclusters[,(31 + i)]) <- paste("NoCl", i, sep = "_")
}
#Put all the data in one place:
FinalData <- bind_cols(CCDclusters, data.frame(PC_CCD$rotation[,1:3]))

#Scale the data
FinalData$EF <- scales::rescale(x=FinalData$PC1, c(-8, 8))
FinalData$PF <- scales::rescale(x=FinalData$PC2, c(-8, 8))
FinalData$DT <- scales::rescale(x=FinalData$PC3, c(-8, 8))

#colnames(FinalData, c("V33", "V34"), c("clust2","clust3"))

cat("The Standard deviation of scaled PC1 (EF) is", sd(FinalData$EF), "\n")
cat("The Standard deviation of scaled PC2 (PF) is", sd(FinalData$PF), "\n")
cat("The Standard deviation of scaled PC3 (DT) is", sd(FinalData$DT), "\n")

#Analysis & Visualization####
#View some plots
#We see that the first three PCs account for ~80% of the variance
Summary_PC_CCD <- summary(PC_CCD)
Summary_PC_CCD$importance[1:3,1:6]
# kbl(Summary_PC_CCD$importance[1:3,1:6], row.names = T, caption = "Principal Components Summary Statistics") %>% kable_classic(full_width = F)


plot(PC_CCD, main = "Variances of Principal Components")

#We are interested in the first three components since they explain most of what is going on in the data
#Lets look at the first two
par(mfrow= c(1,3))
plot(PC_CCD$rotation[,1:2], pch = 20,
     main = "Empirical Ideology Dimensions",
     sub = "Rotations of Principal Components 1 & 2"
     )
#Lets look at the second two
plot(PC_CCD$rotation[,2:3], pch = 20,
     sub = "Rotations of Principal Components 2 & 3"
)

plot(PC_CCD$rotation[,c(3,1)], pch = 20,
     sub = "Rotations of Principal Components 3 & 1"
)

dev.off()
#Kinda cool:
smoothScatter(PC_CCD$rotation[,1:2], xlab = "PC1", ylab = "PC2")

#Great article here:https://www.r-bloggers.com/scatterplot-matrices-in-r/
#Used it and tweaked it to get the following:
panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt)
}

#Then use function above to plot to see all three dimentions and their correlations:
scatterplotMatrix(~PC_CCD$rotation[,1] + PC_CCD$rotation[,2] + PC_CCD$rotation[,3],
                  upper.panel=panel.cor, pch = 20,
                  var.labels=c("PC1_Rotation","PC2_Rotation", "PC3_Rotation")
                  )
title(sub = "Distribtuions & Correlations of Dimensions of Political Ideology")



#Now lets take a look at all three dimensions
scatter3D(FinalData$EF, FinalData$PF, FinalData$DT, colkey = F, phi = 45,
          theta = 45, main = "3D Representation of Ideology, All Countries & All Years", point.col = "black", col = "black")

scatter3D(FinalData$EF, FinalData$PF, FinalData$DT, colkey = F, main = "3D Representation of Ideology, All Countries & All Years", point.col = "black", col = "black")

scatter3D(FinalData$EF, FinalData$PF, FinalData$DT, colkey = F, phi = 45,
          theta = 45, main = "3D Representation of Ideology, All Countries & All Years", point.col = "black", col = "black")



#This function allows you to plot in 3d either the x values or rotation values of a PCA
#the default is to view the rotation for three clusters using the rotated values
plotIdeologyClusters <- function(centers = 3, type = "rotation"){
  KM_CCD <- kmeans(CCD_diff, centers = centers)
  if (type == "x") {plot3d(PC_CCD$x[,1:3], col = KM_CCD$cluster); scatterplot3d(PC_CCD$x[,1:3], color = KM_CCD$cluster, pch = 20)}
  if (type == "rotation") {plot3d(PC_CCD$rotation[,1:3], col = KM_CCD$cluster); scatterplot3d(PC_CCD$rotation[,1:3], color = KM_CCD$cluster, pch = 20)}
  title(main = "Dimensions of Political Ideology", sub = paste("Colors show", centers, "clusters"))
  KM_CCD <<- KM_CCD
}

#Now that we have the clusters for each observation we use 3 clusters as example with rotation
plotIdeologyClusters(centers = 3)

#This is just interesting. Will not go into detail on this
#Plot by country
#Static 2D
plot(PC_CCD$rotation[,1:3], col = as.factor(as.integer(CCD$Country)))
#Interactive 3D
plot3d(PC_CCD$rotation[,1:3], col = as.factor(as.integer(CCD$Country)))


#Show specific countries ideological distributions:
PlotCountryIdeology <- function(CountryToFilter = "United States"){
  CountryFinalData <- filter(FinalData, Country %in% CountryToFilter)
  scatterplot3d(CountryFinalData[,52:54], highlight.3d = TRUE,
                main = paste("3D Ideological Distribution:", CountryToFilter), cex.lab=0.75)

  #scatterplot3d(CountryFinalData[,52:54], color = CountryFinalData$V35, pch = 20)


  CountryFinalData <- data.frame(CountryFinalData$year, CountryFinalData$Country, CountryFinalData[,33:37], CountryFinalData[,52:54])

  CountryFinalData <<- CountryFinalData
}
PlotCountryIdeology()
# 
# #For adjusting margins
# par(mai=c(0.1,0.1,0.1,0.1))
# points3D(FinalData$EF, FinalData$PF, FinalData$DT,
#           phi = 30,
#           theta = 40,
#           bty ="g",
#           colvar = as.integer(FinalData$Country),
#           # col = colors()[seq(from = 1, to = 420, by = 21)]
#           col = sample(rainbow(n = 21), size = 21, replace = F),
#          cex = .6,
#          pch = 19,
#          colkey = as.list(FinalData$Country)
#           )
# 
# text3D(FinalData$EF, FinalData$PF, FinalData$DT,
#        colvar = as.integer(substr(FinalData$year,3,4)),
#        labels = substr(FinalData$year,3,4),
#        colkey = F, cex = .65,
#        phi = 30,
#        theta = 40,
#        bty = "g")
# 
# FinalData$year2 <- as.numeric(as.character(FinalData$year))
# PlotCountryIdeology2 <- function(theCountry = "United States", theYear = NULL){
#   CountryData <- subset(FinalData,Country %in% theCountry)
#   if(!is.null(theYear)){
#     par(mfrow = c(1,2), mai=c(0.25,0.25,0.25,0.25))
#     CountryData <- subset(FinalData, year2 %in% theYear)
#     scatter3D(CountryData$EF, CountryData$PF, CountryData$DT, colkey = F, type = "h", pch = 19,phi = 45,
#               theta = 45, main = unique(CountryData$year2))
#     text3D(CountryData$EF, CountryData$PF, CountryData$DT,
#            # colvar = as.numeric(CountryData$Country),
#            labels = CountryData$Country,
#            colkey = F, cex = .65, bty = "u",
#            phi = 45, theta = 45, add = T)
# 
#     scatter3D(CountryData$EF, CountryData$PF, CountryData$DT, colkey = F, type = "h", pch = 19,            phi = 15,
#               theta = 0, main = unique(CountryData$year2))
#     text3D(CountryData$EF, CountryData$PF, CountryData$DT,
#            #colvar = as.numeric(CountryData$Country),
#            labels = CountryData$Country,
#            colkey = F, cex = .65, bty = "u", add = T)
# 
# 
#   }else{
#   par(mfrow = c(1,2), mai=c(0.25,0.25,0.25,0.25))
#   text3D(CountryData$EF, CountryData$PF, CountryData$DT,
#          colvar = as.numeric(CountryData$year),
#          labels = substr(CountryData$year,3,4),
#          colkey = F, cex = .65, bty = "u",
#          phi = 45, theta = 45,
#          main = unique(theCountry)
#          )
# 
#   text3D(CountryData$EF, CountryData$PF, CountryData$DT,
#          colvar = as.numeric(CountryData$year),
#          labels = substr(CountryData$year,3,4),
#          colkey = F, cex = .65, bty = "u",
#          phi = 15, theta = 0,
#          main = unique(theCountry)
#          )
# 
#   # scatter3D(CountryData$EF, CountryData$PF, CountryData$DT, phi = 45, theta = 45, type = "h",
#   #           ticktype = "detailed", lwd = .5, colkey = F, bty = "u")
#   # text3D(CountryData$EF, CountryData$PF, CountryData$DT,
#   #        labels = substr(CountryData$year2,3,4), cex = .5,
#   #        add = T)
#   #
#   # scatter3D(CountryData$EF, CountryData$PF, CountryData$DT, phi = 15, theta = 0, type = "h",
#   #           ticktype = "detailed", lwd = .5, colkey = F, bty = "u")
#   # text3D(CountryData$EF, CountryData$PF, CountryData$DT,
#   #        labels = substr(CountryData$year2,3,4), cex = .5,
#   #        add = T)
#   }
#   CountryData <<- CountryData
# }
# # PlotCountryIdeology2()
# # PlotCountryIdeology2(theYear = 2002)
# for(i in max(FinalData$year2):min(FinalData$year2)){
#   print(i)
#   jpeg(paste0("C:/Users/Owner/Documents/GitHub/Dissertation/tables and graphs", "/Year", i, ".jpg"))
#   PlotCountryIdeology2(theYear = i)
#   dev.off()
# }
# 
# for(i in unique(as.character(FinalData$Country))){
#   print(i)
#   jpeg(paste0("C:/Users/Owner/Documents/GitHub/Dissertation/tables and graphs", "/Country", i, ".jpg"))
#   PlotCountryIdeology2(theCountry = i)
#   dev.off()
# }
# 
# #Save results
# plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE)
# plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
# file.copy(from=plots.png.paths, to="C:/Users/Owner/Documents/GitHub/Dissertation/tables and graphs")
# 
# library(plot3Drgl)
# plotrgl()
#Understanding and creating empirically based distributions
#What about the first 3 principal components? Are they distributed normally?
#Lets check using the shapiro test for normality
shapiro.test(PC_CCD$rotation[,1])
shapiro.test(PC_CCD$rotation[,2])
shapiro.test(PC_CCD$rotation[,3])
#We see that we can reject the null hypothesis of normality
ks.test(PC_CCD$rotation[,1], "pnorm", mean=mean(PC_CCD$rotation[,1]), sd=sd(PC_CCD$rotation[,1]))

#now test other distributions
#fitdistr(abs(PC_CCD$rotation[,1]), "weibull")
ks.test(PC_CCD$x[,1], "pweibull", scale=.0300335846, shape=3.5980409221)






#
# Data prep for netlogo####
# # #Save image of data for RMarkDown
# # save.image(file = "PoliticalSpectrumClustering.RData")
# # # ###Load the Rational Voter NetLogo Model
# NLLoadModel('C:/Users/Harold/Documents/CSS/Computational Econ CSS695/RationalVoterModel_v.2.nlogo')
# #Use 3D Netlogo
# setwd('C:/Program Files (x86)/NetLogo 5.2.1')
# ###This code starts the NetLogo GUI
# nl.path <- getwd()
# NLStart(nl.path, is3d = TRUE)
# NLLoadModel('C:/Users/Harold/Documents/CSS/Computational Econ CSS695/3d_Ideological_Agents.nlogo3d')



#So lets just sample from the empirical data
#This function gets us a number of observations:
getVoterPopulation <- function(numAgents){
  VoterPopulation <- sample_n(FinalData, numAgents, replace = TRUE)
  #scatterplot3d(VoterPopulation[,52:54], highlight.3d = TRUE)

  #Need to rescale for netlogo
  VoterPopulation$EF <- scales::rescale(x=VoterPopulation$PC1, c(-8, 8))
  VoterPopulation$PF <- scales::rescale(x=VoterPopulation$PC2, c(-8, 8))
  VoterPopulation$DT <- scales::rescale(x=VoterPopulation$PC3, c(-8, 8))
  VoterPopulation$breed <- "Voter"

  VoterPopulation <<- VoterPopulation

}

setupAgents <-function(numAgents = 1000, graphType = "SmallWorld"){
#random graph
if(graphType == "SmallWorld") {g <- sample_smallworld(1, numAgents, 5, 0.05)}
if(graphType == "Random") {g <- erdos.renyi.game(numAgents, 1/100)}
if(graphType == "ScaleFree") {g <- barabasi.game(numAgents, power = 1)}

getVoterPopulation(numAgents = numAgents)
V(g)$EF=as.numeric(VoterPopulation$EF[1:numAgents])
V(g)$PF=as.numeric(VoterPopulation$PF[1:numAgents])
V(g)$DT=as.numeric(VoterPopulation$DT[1:numAgents])
V(g)$breed=as.character(VoterPopulation$breed[1:numAgents])
#V(g)$candidate = as.character(VoterPopulation$candidate[1:numAgents])
plot(g, edge.arrow.size=0.5, vertex.label = NA, vertex.size = 2)
g <<- g
write.graph(g, paste(graphType,"GraphForNetLogoModel.graphml", sep = "_"), format=c("graphml"))

}
#
# plot(degree.distribution(g), type = "l")
# hist(degree.distribution(g))
# plot(g, edge.arrow.size=0.5, vertex.label = NA, vertex.size = 2)
# summary(g)
# graph.density(g)
#
# plot(degree.distribution(g), xlab="degree",ylab="frequency", log="xy", pch=3, col=3, type="l")
#
# #add properties to the agents in the network
# V(Year1945Graph)$milex_percap=as.numeric(NMC1945$milex_percap[match(V(Year1945Graph)$name,NMC1945$ccode)])
# V(g)$EF=as.numeric(FinalData$PC1[1:vcount(g)])
#
#
#
#
dev.off()
par(mfrow=c(2,2))
PlotCountryIdeology("Australia")
PlotCountryIdeology("Austria")
PlotCountryIdeology("Belgium")
PlotCountryIdeology("Canada")
# PlotCountryIdeology("Denmark")
# PlotCountryIdeology("Finland")
# PlotCountryIdeology("France")
# PlotCountryIdeology("West Germany")
# PlotCountryIdeology("Ireland")
# PlotCountryIdeology("Italy")
# PlotCountryIdeology("Japan")
# PlotCountryIdeology("Netherlands")
# PlotCountryIdeology("New Zealand")
# PlotCountryIdeology("Norway")
# PlotCountryIdeology("Sweden")
# PlotCountryIdeology("Switzerland")
# PlotCountryIdeology("United Kingdom")
# PlotCountryIdeology("United States")
# PlotCountryIdeology("Greece")
# PlotCountryIdeology("Portugal")
# PlotCountryIdeology("Spain")
#
#
#
#
# #Experiment analysis####
#POINT this code to a table coming from the behavior space output of the Candidate Strategy Experiments

discountSweepResults <- read.csv(
  file = "Experiment Results/3d_Ideological_Agents_v1_w.experiments Candidate Strategy Experiments-table.csv",
  skip = 6,
  colClasses = "character"
  )
glimpse(discountSweepResults)
discountSweepResults$discountL <- as.numeric(discountSweepResults$discountL)
discountSweepResults$discountR <- as.numeric(discountSweepResults$discountR)
discountSweepResults$X..count.Voters.with..voting....TRUE.....count.Voters....100 <- as.numeric(discountSweepResults$X..count.Voters.with..voting....TRUE.....count.Voters....100)
discountSweepResults$X.step. <- as.numeric(discountSweepResults$X.step.)
table(discountSweepResults$NetworkType, discountSweepResults$Winner)

electionResults <- subset(discountSweepResults, X.step. == 20)

ERscaleFree <- subset(electionResults, NetworkType == "ScaleFreeNetwork")
ERrandom <- subset(electionResults, NetworkType == "RandomNetwork")
ERsmallWorld <- subset(electionResults, NetworkType == "SmallWorldNetwork")

table(ERscaleFree$Winner, ERscaleFree$discountL)
table(ERrandom$Winner, ERrandom$discountL)
table(ERsmallWorld$Winner, ERsmallWorld$discountL)

kbl(tabyl(dat = ERscaleFree, Winner, discountL)%>%
      adorn_percentages("col") %>%
      adorn_pct_formatting(digits = 1)%>%adorn_ns(),
    rownames = F, caption = "Winner of Election Parameter Sweep - Scale Free Network") %>% kable_classic(full_width = F)

kbl(tabyl(dat = ERrandom, Winner, discountL)%>%
      adorn_percentages("col") %>%
      adorn_pct_formatting(digits = 1)%>%adorn_ns(),
    rownames = F, caption = "Winner of Election Parameter Sweep - Random Network") %>% kable_classic(full_width = F)

kbl(tabyl(dat = ERsmallWorld, Winner, discountL)%>%
      adorn_percentages("col") %>%
      adorn_pct_formatting(digits = 1)%>%adorn_ns(),
    rownames = F, caption = "Winner of Election Parameter Sweep - Small World Network") %>% kable_classic(full_width = F)



table(ERscaleFree$Flip., ERscaleFree$discountL)
table(ERrandom$Flip., ERrandom$discountL)
table(ERsmallWorld$Flip., ERsmallWorld$discountL)

#The dynamic candidate was able to

#Cost Sweeps####
costSweepResults <- read.csv(
  file = "Experiment Results/3d_Ideological_Agents_v1_w.experiments Voting Cost Experiments-table.csv",
  skip = 6,
  colClasses = "character"
)

glimpse(costSweepResults)
costSweepResults$discountL <- as.numeric(costSweepResults$discountL)
costSweepResults$discountR <- as.numeric(costSweepResults$discountR)
#VPR = voter participation rate
costSweepResults$VPR <- round(as.numeric(costSweepResults$X..count.Voters.with..voting....TRUE.....count.Voters....100, 0))
costSweepResults$X.step. <- as.numeric(costSweepResults$X.step.)
table(costSweepResults$NetworkType, costSweepResults$Winner)

CSelectionResults <- subset(costSweepResults, X.step. == 20)

table(CSelectionResults$Winner, CSelectionResults$Cost)
tabyl(dat = CSelectionResults, Winner, Cost)%>%
adorn_percentages("row") %>%
adorn_pct_formatting(digits = 1)%>%adorn_ns()


table(CSelectionResults$Winner, CSelectionResults$Flip.)
table(CSelectionResults$Flip., CSelectionResults$Cost)
table(round(CSelectionResults$VPR,-1), CSelectionResults$Cost)


kbl(tabyl(dat = CSelectionResults, Cost, Winner)%>%
                  adorn_percentages("row") %>%
                  adorn_pct_formatting(digits = 1)%>%adorn_ns(),
          rownames = F, caption = "") %>% kable_classic(full_width = F)

# kbl(tabyl(dat = CSelectionResults, Cost, Flip.)%>%
#               adorn_percentages("row") %>%
#               adorn_pct_formatting(digits = 1)%>%adorn_ns(),
#           rownames = F) %>% kable_classic(full_width = F)

kbl(tabyl(dat = CSelectionResults, Cost, VPR)%>%
              adorn_percentages("col") %>%
              adorn_pct_formatting(digits = 1)%>%adorn_ns(),
          rownames = F, caption = "Cost of Voting and Voting Participation Rate (VPR)") %>% kable_classic(full_width = F)






