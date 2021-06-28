
###Harold Walbert
###Global Actors Code

###Load igraph
library(igraph)
library(dplyr)
library(cluster)

#Read in a Prep Data####
##First the Social Network Data (Formal Defense Agreements)
DefenseData <- read.csv("Data for Analysis and Model/alliance_v4.1_by_directed_yearly.csv")
###Now do calculations specific to the Defense Data
##Calculate the number of years allied and append to new column
DefenseData <- mutate(DefenseData, YearsAllied = year - dyad_st_year)
##Second the National Materials Capabilities Data
NMCData <- read.csv("Data for Analysis and Model/NMC_5_0.csv")
##Also need the COW Country Codes Dataset
COWCountryCodes <- read.csv("Data for Analysis and Model/COW country codes.csv")
COWCountryCodes$StateAbb <- NULL
##Remove the first column to make "ccode" the first column
NMCData$stateabb <- NULL

##Replace the "-9" observations in the dataset with blanks
NMCData[NMCData =="-9"] <- NA
##Calculate military expenditures per capita
NMCData <- mutate(NMCData, milex_percap = milex / tpop)
##Multiply military expenditures per capita by the composite index of national capabilities
NMCData <- mutate(NMCData, power = milex_percap * cinc)
##Take the log of irst and replace the "-Inf" (from trying to take the log of 0) with 0
NMCData <- mutate(NMCData, irstlog = log(irst))
NMCData$irstlog[NMCData$irstlog == "-Inf"] <- 0

#Filter Both Datasets for Desired years####
##Select Years for analysis and subset relations for for defense agreements
Year1945 <- filter(DefenseData, year ==  1945 & defense == 1)
Year1960 <- filter(DefenseData, year ==  1960 & defense == 1)
Year1980 <- filter(DefenseData, year ==  1980 & defense == 1)
Year2007 <- filter(DefenseData, year ==  2007 & defense == 1)
##Select years for anlysis from NMC Data
NMC1945 <- filter(NMCData, year == 1945)
NMC1960 <- filter(NMCData, year == 1960)
NMC1980 <- filter(NMCData, year == 1980)
NMC2007 <- filter(NMCData, year == 2007)
##Resize CINC to make the largest value in each year get a value of 1
NMC1945 <- mutate(NMC1945, cincResized = cinc / (max(NMC1945$cinc, na.rm=TRUE)))
NMC1960 <- mutate(NMC1960, cincResized = cinc / (max(NMC1960$cinc, na.rm=TRUE)))
NMC1980 <- mutate(NMC1980, cincResized = cinc / (max(NMC1980$cinc, na.rm=TRUE)))
NMC2007 <- mutate(NMC2007, cincResized = cinc / (max(NMC2007$cinc, na.rm=TRUE)))

##Get the diference between what is in the edge list of nodes and the vertex property list of nodes
##The only years that had this issue were 1960 and 2007
diff1960 <- setdiff(Year1960$ccode1, NMC1960$ccode)
add1960 <- data.frame(ccode=diff1960)
NMC1960 <- bind_rows(NMC1960, add1960)
diff2007 <- setdiff(Year2007$ccode1, NMC2007$ccode)
add2007 <- data.frame(ccode=diff2007)
NMC2007 <- bind_rows(NMC2007, add2007)

##Calculate the AllianceLevel by dividing the number of years in alliance by max number of years in YearsAllied column
Year1945 <- mutate(Year1945, AllianceLevel = YearsAllied / max(Year1945$YearsAllied))
Year1960 <- mutate(Year1960, AllianceLevel = YearsAllied / max(Year1960$YearsAllied))
Year1980 <- mutate(Year1980, AllianceLevel = YearsAllied / max(Year1980$YearsAllied))
Year2007 <- mutate(Year2007, AllianceLevel = YearsAllied / max(Year2007$YearsAllied))

##Throw out excess columns
Year1945 <- select(Year1945, ccode1, ccode2, AllianceLevel, YearsAllied )
Year1960 <- select(Year1960, ccode1, ccode2, AllianceLevel, YearsAllied )
Year1980 <- select(Year1980, ccode1, ccode2, AllianceLevel, YearsAllied )
Year2007 <- select(Year2007, ccode1, ccode2, AllianceLevel, YearsAllied )

###Make Graph Object 
Year1945Graph <- graph.data.frame(Year1945, directed=TRUE, vertices=NMC1945)
Year1960Graph <- graph.data.frame(Year1960, directed=TRUE, vertices=NMC1960)
Year1980Graph <- graph.data.frame(Year1980, directed=TRUE, vertices=NMC1980)
Year2007Graph <- graph.data.frame(Year2007, directed=TRUE, vertices=NMC2007)

#Add attributes to Graph Object####
##Add property for military expenditures per capita
V(Year1945Graph)$milex_percap=as.numeric(NMC1945$milex_percap[match(V(Year1945Graph)$name,NMC1945$ccode)])
V(Year1960Graph)$milex_percap=as.numeric(NMC1960$milex_percap[match(V(Year1960Graph)$name,NMC1960$ccode)])
V(Year1980Graph)$milex_percap=as.numeric(NMC1980$milex_percap[match(V(Year1980Graph)$name,NMC1980$ccode)])
V(Year2007Graph)$milex_percap=as.numeric(NMC2007$milex_percap[match(V(Year2007Graph)$name,NMC2007$ccode)])
##Add Property for the name of the country
V(Year1945Graph)$StateNme=as.character(COWCountryCodes$StateNme[match(V(Year1945Graph)$name,COWCountryCodes$CCode)])
V(Year1960Graph)$StateNme=as.character(COWCountryCodes$StateNme[match(V(Year1960Graph)$name,COWCountryCodes$CCode)])
V(Year1980Graph)$StateNme=as.character(COWCountryCodes$StateNme[match(V(Year1980Graph)$name,COWCountryCodes$CCode)])
V(Year2007Graph)$StateNme=as.character(COWCountryCodes$StateNme[match(V(Year2007Graph)$name,COWCountryCodes$CCode)])
##Add property for power (Calculated as: power = milex_percap * cinc)
V(Year1945Graph)$power=as.numeric(NMC1945$power[match(V(Year1945Graph)$name,NMC1945$ccode)])
V(Year1960Graph)$power=as.numeric(NMC1960$power[match(V(Year1960Graph)$name,NMC1960$ccode)])
V(Year1980Graph)$power=as.numeric(NMC1980$power[match(V(Year1980Graph)$name,NMC1980$ccode)])
V(Year2007Graph)$power=as.numeric(NMC2007$power[match(V(Year2007Graph)$name,NMC2007$ccode)])
##Add property using iron and steel production as proxy for wealth
V(Year1945Graph)$wealth=as.numeric(NMC1945$irst[match(V(Year1945Graph)$name,NMC1945$ccode)])
V(Year1960Graph)$wealth=as.numeric(NMC1960$irst[match(V(Year1960Graph)$name,NMC1960$ccode)])
V(Year1980Graph)$wealth=as.numeric(NMC1980$irst[match(V(Year1980Graph)$name,NMC1980$ccode)])
V(Year2007Graph)$wealth=as.numeric(NMC2007$irst[match(V(Year2007Graph)$name,NMC2007$ccode)])
##Add property for the cinc
V(Year1945Graph)$cinc=as.numeric(NMC1945$cinc[match(V(Year1945Graph)$name,NMC1945$ccode)])
V(Year1960Graph)$cinc=as.numeric(NMC1960$cinc[match(V(Year1960Graph)$name,NMC1960$ccode)])
V(Year1980Graph)$cinc=as.numeric(NMC1980$cinc[match(V(Year1980Graph)$name,NMC1980$ccode)])
V(Year2007Graph)$cinc=as.numeric(NMC2007$cinc[match(V(Year2007Graph)$name,NMC2007$ccode)])
##Add property for the cincResized
V(Year1945Graph)$cincResized=as.numeric(NMC1945$cincResized[match(V(Year1945Graph)$name,NMC1945$ccode)])
V(Year1960Graph)$cincResized=as.numeric(NMC1960$cincResized[match(V(Year1960Graph)$name,NMC1960$ccode)])
V(Year1980Graph)$cincResized=as.numeric(NMC1980$cincResized[match(V(Year1980Graph)$name,NMC1980$ccode)])
V(Year2007Graph)$cincResized=as.numeric(NMC2007$cincResized[match(V(Year2007Graph)$name,NMC2007$ccode)])

#Create the graph files####
#NetLogo will use these to create initial conditions
write.graph(Year1945Graph, 'C:/Users/Owner/Documents/DissertationCode/Data/Processed Graph Data for Model/1945.graphml', format=c("graphml"))
write.graph(Year1960Graph, 'C:/Users/Owner/Documents/DissertationCode/Data/Processed Graph Data for Model/1960.graphml', format=c("graphml"))
write.graph(Year1980Graph, 'C:/Users/Owner/Documents/DissertationCode/Data/Processed Graph Data for Model/1980.graphml', format=c("graphml"))
write.graph(Year2007Graph, 'C:/Users/Owner/Documents/DissertationCode/Data/Processed Graph Data for Model/2007.graphml', format=c("graphml"))

#Num Disputes by Year####
library(dissPkg)
numDispByYear <- summarize(group_by(MIDA_4.01, StYear), Count = n()) %>% filter(StYear %in% c(1945, 1960, 1980, 2007))
avgNumDisputes <- mean(numDispByYear$Count)

