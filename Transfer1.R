library(tidyverse)
library(igraph)
library(ggplot2)
library(plotly)

#Read in intial data
df<-read.csv("Top 250 Football Transfers 2000-2018.csv")
nodes<- unique(rbind(df[,4:5] %>% rename(Club=Team_from,League=League_from),df[,6:7] %>% rename(Club=Team_to,League=League_to)))
#vertices<-

#Visualse Trnasfer Value vs Expected Value
plot_ly(df[abs(df$Transfer_fee-df$Market_value)>5000000,],x=~Market_value,y=~Transfer_fee, hovertext=~Name,color=~League_from) %>%
  add_markers() #%>%
  layout(xaxis = list(type = "log"),
        yaxis = list(type = "log"))
plot(graph.data.frame(nodes,directed = TRUE))

#We then foudn much better data on github at https://github.com/ewenme/transfers
#Inspection
PL_13<-read.csv("data/2013/english_premier_league.csv")
PL_13$club_name %>% glimpse()

#Create datframes for all the leagues
EPL<-data.frame() #English Premier League
Eredivisie<-data.frame() #Eredivisie
Bundisliga<-data.frame() #Bundisliga
Ligue1<-data.frame() #Ligue 1
SerieA<-data.frame() #Serie A
Championship<-data.frame() #English Championship
RPL<-data.frame() #|Russian Premier League
LaLiga<-data.frame()
LigaNOS<-data.frame()

#Loops through all the folders
for (i in 1992:2019){
  EPL<-rbind(EPL,read.csv(paste0("data/",as.character(i),"/english_premier_league.csv")))
  Eredivisie<-rbind(Eredivisie,read.csv(paste0("data/",as.character(i),"/dutch_eredivisie.csv")))
  Bundisliga<-rbind(Bundisliga,read.csv(paste0("data/",as.character(i),"/german_bundesliga_1.csv")))
  Ligue1<-rbind(Ligue1,read.csv(paste0("data/",as.character(i),"/french_ligue_1.csv")))
  SerieA<-rbind(SerieA,read.csv(paste0("data/",as.character(i),"/italian_serie_a.csv")))
  Championship<-rbind(Championship,read.csv(paste0("data/",as.character(i),"/english_championship.csv")))
  RPL<-rbind(RPL,read.csv(paste0("data/",as.character(i),"/russian_premier_liga.csv")))
  LaLiga<-rbind(LaLiga,read.csv(paste0("data/",as.character(i),"/spanish_primera_division.csv")))
  LigaNOS<-rbind(LigaNOS,read.csv(paste0("data/",as.character(i),"/portugese_liga_nos.csv")))
  }
All<-list(EPL=EPL,Eredivisie=Eredivisie,Bundisliga=Bundisliga,Ligue1=Ligue1,SerieA=SerieA,Championship=Championship,RPL=RPL,LaLiga=LaLiga,LigaNOS=LigaNOS)

#Looping through the frames is a pain....
# for (i in length(All)){
#     Expenditure<- All[i] %>% 
#                   as.data.frame() %>% 
#                   select(club_name,season,fee_cleaned) %>% 
#                   group_by(club_name,season) %>% 
#                   summarise(Spend=sum(fee_cleaned))
#     assign(paste0(names(All)[i],"_Expenditure"),Expenditure)
# }

#For now we shall focus on the Premier League
EPL_Expenditure<- EPL%>% 
                  as.data.frame() %>% 
                  select(club_name,season,fee_cleaned) %>% 
                  group_by(club_name,season) %>% 
                  summarise(Spend=sum(fee_cleaned))

#Expenditure Visualised-Becomes clear there is data missing
EPL_Expenditure %>% plot_ly(x=~season,y=~Spend,color=~club_name,type="scatter",mode="lines")
EPL_Expenditure %>% plot_ly(x=~season,y=~Spend,color=~club_name,type="bar") %>% layout(barmode="stack")

#Signifying Loans
EPL$Loan<-ifelse(grepl("End of loan",EPL$fee)==TRUE,"End of Loan",ifelse(grepl("loan",EPL$fee)==TRUE,"Loan",as.character(EPL$fee)))
EPL_Loans<-EPL[EPL$Loan=="Loan",]
SerieA$Loan<-ifelse(grepl("End of loan",SerieA$fee)==TRUE,"End of Loan",ifelse(grepl("loan",SerieA$fee)==TRUE,"Loan",as.character(SerieA$fee)))
SerieA_Loans<-SerieA[SerieA$Loan=="Loan",]


#plotting most common Transfer Paths
EPL %>% filter(!grepl("Loan",Loan)) %>%
        filter(!grepl(paste(c("Without Club","Retired"),collapse = "|"),club_involved_name)) %>% 
        mutate(Path=ifelse(transfer_movement=="out",paste(club_name,"to",club_involved_name, sep=" "),paste(club_involved_name,"to",club_name, sep=" "))) %>% 
        filter(!grepl(paste(c("U18","U23","U19","U21"),collapse = "|"),Path)) %>%
        count(Path) %>% 
        #arrange(desc(n)) %>%
        filter(n>=9) %>% 
        plot_ly(x=~reorder(Path,desc(n)),y=~n) %>% 
        add_bars() %>% 
        layout(xaxis=list(title="Most Common Transfers"),
               yaxis=list(title="Number of Transfers"))

SerieA %>% filter(!grepl("Loan",Loan)) %>%
  filter(!grepl(paste(c("Without Club","Retired"),collapse = "|"),club_involved_name)) %>% 
  mutate(Path=ifelse(transfer_movement=="out",paste(club_name,"to",club_involved_name, sep=" "),paste(club_involved_name,"to",club_name, sep=" "))) %>% 
  filter(!grepl(paste(c("U18","U23","U19","U21"),collapse = "|"),Path)) %>%
  count(Path) %>% 
  #arrange(desc(n)) %>%
  filter(n>=15) %>% 
  plot_ly(x=~reorder(Path,desc(n)),y=~n) %>% 
  add_bars() %>% 
  layout(xaxis=list(title="Most Common Transfers"),
         yaxis=list(title="Number of Transfers"))

#Plotting Most Popular Loans
EPL_Loans %>% filter(!grepl(paste(c("Without Club","Retired"),collapse = "|"),club_involved_name)) %>% 
  mutate(Path=ifelse(transfer_movement=="out",paste(club_name,"to",club_involved_name, sep=" "),paste(club_involved_name,"to",club_name, sep=" "))) %>%
  count(Path) %>% 
  filter(n>=10) %>% 
  plot_ly(x=~reorder(Path,desc(n)),y=~n) %>% 
  add_bars() %>% 
  layout(xaxis=list(title="Most Common Loans"),
         yaxis=list(title="Number of Loans"))

SerieA_Loans %>% filter(!grepl(paste(c("Without Club","Retired"),collapse = "|"),club_involved_name)) %>% 
  mutate(Path=ifelse(transfer_movement=="out",paste(club_name,"to",club_involved_name, sep=" "),paste(club_involved_name,"to",club_name, sep=" "))) %>%
  count(Path) %>% 
  filter(n>=12) %>% 
  plot_ly(x=~reorder(Path,desc(n)),y=~n) %>% 
  add_bars() %>% 
  layout(xaxis=list(title="Most Common Loans"),
         yaxis=list(title="Number of Loans"))
