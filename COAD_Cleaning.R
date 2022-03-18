library(plotrix)
library(estimatr)
library(haven)
library(dplyr)
library(foreign)
library(ri2)
library(readxl)

setwd("/Users/aricaschuett/Documents/Fall_2021/DemandsBackup/CommunityPolicing/")

#read in COAD data
COAD <- read_xlsx("COAD Public (3).xlsx")


# change Yes/No variables into 0/1 Y=1 N=0
# 1 indicates GREATER independence from police force 
# 0 indicates LESS independence from Police
# Are former law enforcement members permitted to serve on the board/commission?
COAD<- mutate(COAD,
              board_formerle = ifelse(board_formerle == "Yes", 0, 1),            ## Former officers permitted to serve
              doj_crd = ifelse(doj_crd == "Yes", 1, 0),                          ## DOJ investigation
              corrections = ifelse(corrections == "Yes", 0, 1),                  ## Oversee corrections *
              court_intervention = ifelse(court_intervention ==
                                            "Yes, at the federal level" |
                                            court_intervention ==
                                            "Yes, at the state level", 1, 0),    # Settlement agreement or concent decree at fed or state level
              auditor_model= ifelse(oversight_model == "Auditor/monitor-focused. The office primarily conducts audits of completed department investigations and/or compliance with department policy and procedures OR actively monitors open department investigations and/or compliance with department policies and procedures.", 1, 0),
              investigation_model = ifelse(oversight_model == "Investigation-focused. The office primarily conducts investigations of the overseen law enforcement agency separately and independently of the overseen agency's internal affairs unit." , 1, 0),
              review_model = ifelse(oversight_model== "Review-focused. The office primarily reviews completed complaint investigations conducted by the overseen law enforcement agency's internal affairs unit for thoroughness, completeness, or accuracy." , 1, 0),
              board = ifelse(board== "Yes", 1, 0),                               ## volunteer board or comission
              staff_formerle = ifelse(staff_formerle== "Yes", 0, 1),             ## officers serve on staff
              board_currentle = ifelse(board_currentle== "Yes", 0, 1),           ## current officers on board
              board_stipends = ifelse(board_stipends== "Yes", 1, 0),             #** Board Stipend
              board_training = ifelse(board_training== "Yes", 1, 0),             ## Board training
              complaints_anon = ifelse(complaints_anon== "Yes", 1, 0),           ## recieve, review, investigate complaints
              complaints_affidavit = ifelse(complaints_affidavit== "Yes", 0, 1), ##affidavid required for review
              recs_policy = ifelse(recs_policy== "Yes", 1, 0),                   ## Can issue recomendations: policy or procedures
              recs_discipline = ifelse(recs_discipline== "Yes", 1, 0),           ## Can issue recomendations: dicipline
              recs_findings = ifelse(recs_findings== "Yes", 1, 0),               ## can issue recomendations: from investigations
              recs_training = ifelse(recs_training== "Yes", 1, 0),               ## can issue recomendations: officer training
              implement_policy = ifelse(implement_policy== "Yes", 1, 0),         # can impliment/inact: dept. policies
              implement_discipline= ifelse(implement_discipline== "Yes", 1, 0),  ## can impliment/inact: dicipline
              implement_training = ifelse(implement_training== "Yes", 1, 0),     # can impliment/inact: training
              reports_annual = ifelse(reports_annual== "Yes", 1, 0),             ## can release annual reports to public
              reports_special = ifelse(reports_special== "Yes", 1, 0),           # Can release reports on policies, procedures, specific cases
              reports_complaints = ifelse(reports_complaints== "Yes", 1, 0),     ## Can issue reports on complaints/ statisics
              reports_analyses = ifelse(reports_analyses== "Yes", 1, 0),         ## analyses of department trends in dicipline or practices
              evaluation = ifelse(evaluation== "No", 0, 1),                      #* Is agency evaluated by external stakeholder (this could produce more or less independence from department depending on who stakeholder is)
              mediation = ifelse(mediation== "No", 0, 1),                        #* Mediation Program
              access_iaopen = ifelse(access_iaopen== "Yes", 1, 0),               #* access to internail affairs files and investigations-- Sometimes was coded 0 with no
              access_iaclosed = ifelse(access_iaclosed== "Yes", 1, 0),           #* access to closed internal affiars files and investigations
              access_iafrontend = ifelse(access_iafrontend== "Yes", 1, 0),       ## access front-end to internal affiars databases
              access_iabackend = ifelse(access_iabackend== "Yes", 1, 0),         ## access back-end to internal affairs databases
              access_ews = ifelse(access_ews== "Yes", 1, 0),                     ## access to early warning system records
              access_cad = ifelse(access_cad== "Yes", 1, 0),                     ## computer aided dispatch records
              access_personnel = ifelse(access_personnel== "Yes", 1, 0),         ## access to personnel files
              access_video = ifelse(access_video== "Yes", 1, 0),                 ## access to body-worn camera videos
              access_stops = ifelse(access_stops== "Yes", 1, 0),                 ## access to stops information
              access_uof = ifelse(access_uof== "Yes", 1, 0),                     ## access to use of force records
              access_all = ifelse(access_all== "Yes", 1, 0),                     ## access to all databases
              access_coop = ifelse(access_coop== "No", 1, 0),                    ## Does access depend on cooperation of LEA
              access_subpoena = ifelse(access_subpoena== "No", 0, 1),              ## Ability to issue subpoenas
              critical_incident = ifelse(critical_incident== "Yes", 1, 0),       ## Authority to be on the scene for critical incidents?
              recs_response = ifelse(recs_response== "Yes", 1, 0))               ## is department required to resond to recomendations




## Independence Index
COAD_II <- data.frame(COAD[36:72])
# Replace NAs with 0
COAD_II[is.na(COAD_II)] = 0
COAD_II_Score<- mutate(COAD_II,
              Independence_Score = rowSums(COAD_II), na.rm=TRUE)

# New matrix
COAD_abrv <- cbind(COAD$name, COAD$city, COAD$State, COAD$jurisdiction_pop, COAD_II_Score$Independence_Score)




#colnames(COAD_abrv) <- c("name", "city", "state", "population", "Score")
COAD_abrv <- as.data.frame(COAD_abrv)

ggplot(COAD_abrv, aes(x = V3, y = V5)) +
  geom_point()


my_graph <- ggplot(COAD_abrv, aes(x = V4, y = V5)) +
  geom_point() +
  geom_smooth(formula = y ~ x,method = "loess",se = TRUE, aes(group = "" ))
my_graph

#test <-lm(V4~V5, COAD_abrv)
#summary(test)


## Longitude and latitude data with 
df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_us_cities.csv')
df$name <- trimws(df$name)

#read in Demands
Chapters <- read.csv("/Users/aricaschuett/Documents/Spring 2022/Experiments/Chapters.csv", header=T, na.strings=c("","NA"))

Chapters$name <- Chapters$City
#join 
#Chapters = Chapters %>% full_join(df, by="name")
#Chapters <- merge(x=df, y=Chapters, by= "name", all= TRUE)


###### left join in R using merge() function 
df2 = merge(x=Chapters,y=df,by="name",all.x=TRUE)

df2 <- data.frame(df2$name, df2$lat, df2$lon, df2$pop)
colnames(df2) <- c("name", "lat", "lon", "pop")
df2 <- df2[complete.cases(df2),]


#make this into an index of specificity or some other metric
df2$q <- with(df2, cut(pop, quantile(pop)))
levels(df2$q) <- paste(c("1st", "2nd", "3rd", "4th", "5th"), "Quantile")
df2$q <- as.ordered(df2$q)

g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showland = TRUE,
  landcolor = toRGB("gray85"),
  subunitwidth = 1,
  countrywidth = 1,
  subunitcolor = toRGB("white"),
  countrycolor = toRGB("white")
)

fig <- plot_geo(df2, locationmode = 'USA-states', sizes = c(1, 250))
fig <- fig %>% add_markers(
  x = ~lon, y = ~lat, size = ~pop, color = ~q, hoverinfo = "text",
  text = ~paste(df2$name, "<br />", df2$pop/1e6, " million")
)
fig <- fig %>% layout(title = '2014 US city populations<br>(Click legend to toggle)', geo = g )

fig












