# diss_repo
Repository for data and code related to the Global Agent Tribute Model and the Election and Voting Model

•	Candidate and Voter Election Behavior – Supporting Chapter 4 of Dissertation

  o	Data for Analysis and Model
    -	CountryIDXWalk.csv – Crosswalk between Country ID (coid) and Country Name in the Swank party dataset
    
    -	Random_GraphForNetLogoModel.graphml – graph data for instantiating 3D Voting Agents Netlogo model
    
    -	ScaleFree_GraphForNetLogoModel.graphml – graph data for instantiating 3D Voting Agents Netlogo model
    
    -	SmallWorld_GraphForNetLogoModel.graphml – graph data for instantiating 3D Voting Agents Netlogo model
    
    -	Swank-party-data-update.xls – Comparative Parties Dataset (Swank, D. (2018). Comparative Political Parties Dataset: Electoral, Legislative, and Government Strength of Political Parties by Ideological Group in 21 Capitalist Democracies, 1950-2015. Electronic Database, Department of Political Science, Marquette University, http://www.marquette.edu/polisci/faculty_swank.shtml)
    
  o	3D Voting Agents.nlogo3d – A 3D Netlogo model of Candidate and Agents interacting in 3D space
  
  o	Agents & Ideology.R – Data preparation and analysis supporting the Netlogo 3D voting model

•	CoW R Package

  o	dissPkg.zip – R Package connecting multiple dataset from the Correlates of War Dataset
  
  o	dissPkg_0.1.0.pdf – Documentation of the functions and data in the dissPkg library

•	International Networks and Dynamics – Supporting Chapter 2 & 3 of Dissertation
  
  o	Data for Analysis and Model
  
    -	1945.graphml – graph dataset of the formal defense alliances network between countries in 1945, used to instantiate the Global Agent Netlogo Model
    
    -	1960.graphml – graph dataset of the formal defense alliances network between countries in 1960, used to instantiate the Global Agent Netlogo Model
    
    -	1980.graphml – graph dataset of the formal defense alliances network between countries in 1980, used to instantiate the Global Agent Netlogo Model
    
    -	2007.graphml – graph dataset of the formal defense alliances network between countries in 2007, used to instantiate the Global Agent Netlogo Model
    
    -	COW country codes.csv – Correlates of War crosswalk between Country Abbreviation, Country Code, and Country name (https://correlatesofwar.org/data-sets/cow-country-codes)
    
    -	NMC_5_0.csv – Correlates of War National Material Capabilities dataset (https://correlatesofwar.org/data-sets/national-material-capabilities)
    
    -	alliance_v4.1_by_directed_yearly.csv – Correlates of War Alliances dataset (https://correlatesofwar.org/data-sets/formal-alliances)

o	Global Actors Experiments.R – This code connects R to Netlogo using the RNetLogo Package, it conducts experiments on the Netlogo model around distributions of power and supports the analysis in Chapter 3 of the dissertation

o	Global Actors R Code.R – Code cleaning and preparing the formal defense alliance networks for use in the Netlogo simulation model

o	GlobalAgentModel_JASSS Version For Diss.nlogo – Global Agent Simulation model used in the JASSS article (https://www.jasss.org/21/3/4.html), updated for this dissertation
