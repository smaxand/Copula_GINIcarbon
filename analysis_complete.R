#R-file for paper submission, CC-BY-NC-ND license
#title: The nonlinear dependence of income inequality and carbon emissions: potentials for a sustainable future
#Authors: Franziska Dorn Simone Maxand Thomas Kneib

rm(list=ls(all=TRUE))
library(dplyr)
library(readr)
library(foreign)
library(tidyr)
library(trustOptim)
library(GJRM)
library(gamlss)
library(copula)
library(evd)
library(foreign)
library(magic)
library(mgcv)
library(trust)
library(VGAM)
library(VineCopula)
library(readr)
library(xtable)
library(matrixStats)
library(BayesX)
library(maptools)
library(spatial)
library(readxl)
library(readstata13)
library(plyr)
library(car)

setwd("") #paste filepath here 


###############################################################################################################
##merge
#SWIID
##############################################################
SWIID <- readRDS("SWIID/swiid8_3/SWIID8_3_new_format.RDS")
names(SWIID)[1] <- "Country"
names(SWIID)[2] <- "Year"
SWIID$Country<-tolower(SWIID$Country)


# WDI
##############################################################
WDI <- readRDS("WDI/WDIDATA_08_20.RDS")
names(WDI)[1] <- "Country"
names(WDI)[2] <- "Year"

WDI$Country<- ifelse(WDI$Country=="Egypt, Arab Rep.","egypt", WDI$Country)
WDI$Country<- ifelse(WDI$Country=="Cote d'Ivoire","ivory coast", WDI$Country)
WDI$Country<- ifelse(WDI$Country=="Bahamas, The","bahamas", WDI$Country)
WDI$Country<- ifelse(WDI$Country=="Gambia, The","gambia", WDI$Country)
WDI$Country<- ifelse(WDI$Country=="Hong Kong SAR, China","hong kong", WDI$Country)
WDI$Country<- ifelse(WDI$Country=="Iran, Islamic Rep.","iran", WDI$Country)
WDI$Country<- ifelse(WDI$Country=="Macao SAR, China","macao", WDI$Country)
WDI$Country<- ifelse(WDI$Country=="Macedonia, FYR","macedonia", WDI$Country)
WDI$Country<- ifelse(WDI$Country=="Micronesia, Fed_new. Sts.","micronesia", WDI$Country)
WDI$Country<- ifelse(WDI$Country=="Lao PDR","laos", WDI$Country)
WDI$Country<- ifelse(WDI$Country=="Korea, Dem. People????Ts Rep.","Korea north", WDI$Country)
WDI$Country<- ifelse(WDI$Country=="Kyrgyz Republic","kyrgyzstan", WDI$Country)
WDI$Country<- ifelse(WDI$Country=="Yemen, Rep.","Yemen", WDI$Country)
WDI$Country<- ifelse(WDI$Country=="Venezuela, RB","Venezuela", WDI$Country)
WDI$Country<- ifelse(WDI$Country=="United States","United States of america", WDI$Country)
WDI$Country<- ifelse(WDI$Country=="Slovak Republic","slovakia", WDI$Country)
WDI$Country<- ifelse(WDI$Country=="Russian Federation","russia", WDI$Country)
WDI$Country<- ifelse(WDI$Country=="Korea, Rep.","korea south", WDI$Country)
WDI$Country<- ifelse(WDI$Country=="Brunei Darussalam","brunei", WDI$Country)
WDI$Country<- ifelse(WDI$Country=="St. Kitts and Nevis","Saint Kitts and Nevis", WDI$Country)
WDI$Country<- ifelse(WDI$Country=="St. Vincent and the Grenadines","Saint Vincent and the Grenadines", WDI$Country)
WDI$Country<- ifelse(WDI$Country=="St. Lucia","Saint Lucia", WDI$Country)
WDI$Country<- ifelse(WDI$Country=="Cabo Verde","cape verde", WDI$Country)
WDI$Country<- ifelse(WDI$Country=="Micronesia, Fed. Sts.","micronesia", WDI$Country)

WDI$Country<-tolower(WDI$Country)


########################################
#Polity
Polity<- readRDS("Polity/polity_data_23_08_20.RDS")

names(Polity)[1] <- "Country"
names(Polity)[2] <- "Year"

Polity$Country<- ifelse(Polity$Country=="Cote D'Ivoire","ivory coast", Polity$Country)
Polity$Country<- ifelse(Polity$Country=="Congo Brazzaville","congo, rep.", Polity$Country)
Polity$Country<- ifelse(Polity$Country=="Congo Kinshasa","congo, dem. rep.", Polity$Country)
Polity$Country<- ifelse(Polity$Country=="Bosnia","bosnia and herzegovina", Polity$Country)
Polity$Country<- ifelse(Polity$Country=="Syria","Syrian arab republic", Polity$Country)
Polity$Country<- ifelse(Polity$Country=="UAE","united arab emirates", Polity$Country)
Polity$Country<- ifelse(Polity$Country=="Myanmar (Burma)","myanmar", Polity$Country)
Polity$Country<- ifelse(Polity$Country=="United States","united states of america", Polity$Country)
Polity$Country<- ifelse(Polity$Country=="Slovak Republic","slovakia", Polity$Country)

Polity$Country<-tolower(Polity$Country)



###########################################################
#Global Carbon Atlas
GCA<- readRDS("Global_Carbon_Atlas/GCA_09_20.RDS")

#rename countries to fit other data
GCA$Country<- ifelse(GCA$Country=="Syria","Syrian arab republic", GCA$Country)
GCA$Country<- ifelse(GCA$Country=="C?te d'Ivoire","Ivory coast", GCA$Country)
GCA$Country<- ifelse(GCA$Country=="Congo","Congo, rep.", GCA$Country)
GCA$Country<- ifelse(GCA$Country=="Democratic Republic of the Congo","Congo, dem. rep.", GCA$Country)
GCA$Country<- ifelse(GCA$Country=="Russian Federation","Russia", GCA$Country)
GCA$Country<- ifelse(GCA$Country=="South Korea","Korea south", GCA$Country)


GCA$Country<-tolower(GCA$Country)

GCA2<- readRDS("Global_Carbon_Atlas/GCA_consum_pc_01_21.RDS")

#rename countries to fit other data
GCA2$Country<- ifelse(GCA2$Country=="Syria","Syrian arab republic", GCA2$Country)
GCA2$Country<- ifelse(GCA2$Country=="C?te d'Ivoire","Ivory coast", GCA2$Country)
GCA2$Country<- ifelse(GCA2$Country=="Congo","Congo, rep.", GCA2$Country)
GCA2$Country<- ifelse(GCA2$Country=="Democratic Republic of the Congo","Congo, dem. rep.", GCA2$Country)
GCA2$Country<- ifelse(GCA2$Country=="Russian Federation","Russia", GCA2$Country)
GCA2$Country<- ifelse(GCA2$Country=="South Korea","Korea south", GCA2$Country)


GCA2$Country<-tolower(GCA2$Country)


###Merge SWIID and WDI
#### common id NFA and SWIID
commonID<-intersect(SWIID$Country,WDI$Country)
#commonID
notcommons<-SWIID[!SWIID$Country %in% commonID,]
table(notcommons$Country)
notcommonc<-WDI[!WDI$Country %in% commonID,]
table(notcommonc$Country)

merge_WDI_SWIID <- merge( SWIID, WDI, by= c("Country","Year"))

#Merge with polity
commonID<-intersect(merge_WDI_SWIID$Country,Polity$Country)
#commonID
notcommonsc<-merge_WDI_SWIID[!merge_WDI_SWIID$Country %in% commonID,]
table(notcommonsc$Country)
notcommonw<-Polity[!Polity$Country %in% commonID,]
table(notcommonw$Country)

merge_WDI_SWIID_Polity <- merge(merge_WDI_SWIID, Polity,  by= c("Country","Year"))


commonID<-intersect(merge_WDI_SWIID_Polity$Country,GCA2$Country)
notcommonsc<-merge_WDI_SWIID_Polity[!merge_WDI_SWIID_Polity$Country %in% commonID,]
table(notcommonsc$Country)
notcommonw<-GCA2[!GCA2$Country %in% commonID,]
table(notcommonw$Country)

merge_WDI_SWIID_Polity_GCA <- merge(merge_WDI_SWIID_Polity, GCA2,  by= c("Country","Year"))

###################################################################################################################
##Adjusting variables
d<- merge_WDI_SWIID_Polity_GCA

#recode polity -88, -77, -66 into missing

d$polity<- car::recode(d$polity, "-88=NA; -77= NA; -66=NA")

d$lGDP<- log(d$NY.GDP.PCAP.KD)
d$GDP<- d$NY.GDP.PCAP.KD
d$lGDP_pp<- log(d$NY.GDP.PCAP.PP.KD)
d$lGDP_pp_cd<- log(d$NY.GDP.PCAP.PP.CD)
d$lGINI <- log(d$gini_disp)
d$GINI<- d$gini_disp
d$Agri <- d$NV.AGR.TOTL.ZS
d$Serv <- d$NV.SRV.TOTL.ZS
d$Manu <- d$NV.IND.MANF.ZS
d$Urban <- d$SP.URB.TOTL.IN.ZS
d$lGDP2 <- d$lGDP^2
d$lGDP_pp2<- d$lGDP_pp^2
d$lGDP_pp_cd2<-d$lGDP_pp_cd^2
d$lGINI2 <- d$lGINI^2
d$carbonWDI<- d$EN.ATM.CO2E.PC
d$fossil <- d$EG.USE.COMM.FO.ZS
d$aelec<- d$EG.ELC.ACCS.ZS


# Country names into numeric
x <- as.factor(d$Country)
levels(x) <- 1:length(levels(x))
d$cid <- as.numeric(x)






#############################################################################################
###Country groups
data<- d
data1<-data.frame(data$Country,data$Year, data$lGINI, data$lGINI2, data$Urban, data$lGDP, data$lGDP2, data$Agri,data$Manu, data$Serv)

income_class <- read_excel("income_class.xls",  skip = 4)
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

countries_char <- as.character(unique(data1$data.Country))

countries_cap<-sapply(countries_char, simpleCap)
inc_class<-rep(0,length(unique(data1$data.Country)))
country_notrecog<-list()
for(k in 1:length(unique(data1$data.Country))){
  inc_class[k]<-na.omit(income_class[income_class[,3]==countries_cap[k],7])
  if(identical(inc_class[[k]], character(0))) 
    country_notrecog<-c(country_notrecog,countries_cap[k])
}
country_notrecog[1]<-"Antigua and Barbuda"
country_notrecog[2]<-"Bahamas, The"
country_notrecog[3]<-"Bosnia and Herzegovina"
country_notrecog[4]<-"Brunei Darussalam"
country_notrecog[5]<-"Cabo Verde"
country_notrecog[6]<-"Egypt, Arab Rep."
country_notrecog[7]<-"Gambia, The"
country_notrecog[8]<-"Guinea-Bissau"
country_notrecog[9]<-"Iran, Islamic Rep."
country_notrecog[10]<-"C?te d'Ivoire"
country_notrecog[11]<-"Korea, Rep."
country_notrecog[12]<-"Kyrgyz Republic"
country_notrecog[13]<-"Lao PDR"
country_notrecog[14]<-"North Macedonia"    
country_notrecog[15]<-"Micronesia, Fed. Sts."
country_notrecog[16]<-"Russian Federation"
country_notrecog[17]<-"St. Kitts And Nevis"
country_notrecog[18]<-"St. Lucia"
country_notrecog[19]<-"St. Vincent and the Grenadines"
country_notrecog[20]<-"S?o Tom? and Principe"
country_notrecog[21]<-"Slovak Republic"
country_notrecog[22]<-"Eswatini"
country_notrecog[23]<-"Timor-Leste"
country_notrecog[24]<- "Trinidad and Tobago"
country_notrecog[25]<- "Turks and Caicos Islands"
country_notrecog[26]<-"United States"
country_notrecog[27]<-"Venezuela, RB"
country_notrecog[28]<-"Yemen, Rep."

i<-1
for(k in 1:length(inc_class)){
  if(identical(inc_class[[k]], character(0))) {
    inc_class[k]<-na.omit(income_class[income_class[,3]==country_notrecog[i],7])
    i<-i+1}}

income_class$Economy<- ifelse(income_class$Economy=="Antigua and Barbuda","antigua and barbuda", income_class$Economy)
income_class$Economy<- ifelse(income_class$Economy=="Bahamas, The","bahamas", income_class$Economy)
income_class$Economy<- ifelse(income_class$Economy=="Bosnia and Herzegovina","bosnia and herzegovina", income_class$Economy)
income_class$Economy<- ifelse(income_class$Economy=="Brunei Darussalam","brunei", income_class$Economy)
income_class$Economy<- ifelse(income_class$Economy=="Cabo Verde","cape verde", income_class$Economy)
income_class$Economy<- ifelse(income_class$Economy=="Egypt, Arab Rep.","egypt", income_class$Economy)
income_class$Economy<- ifelse(income_class$Economy=="Gambia, The","gambia", income_class$Economy)
income_class$Economy<- ifelse(income_class$Economy=="Guinea-Bissau","guinea-bissau", income_class$Economy)
income_class$Economy<- ifelse(income_class$Economy=="Iran, Islamic Rep.","iran", income_class$Economy)
income_class$Economy<- ifelse(income_class$Economy=="C?te d'Ivoire","ivory coast", income_class$Economy)
income_class$Economy<- ifelse(income_class$Economy=="Korea, Rep.","korea south", income_class$Economy)
income_class$Economy<- ifelse(income_class$Economy=="Kyrgyz Republic","kyrgyzstan", income_class$Economy)
income_class$Economy<- ifelse(income_class$Economy=="Lao PDR","laos", income_class$Economy)
income_class$Economy<- ifelse(income_class$Economy=="North Macedonia","macedonia", income_class$Economy)
income_class$Economy<- ifelse(income_class$Economy=="Micronesia, Fed. Sts.","micronesia", income_class$Economy)
income_class$Economy<- ifelse(income_class$Economy=="St. Kitts And Nevis","saint kitts and nevis", income_class$Economy)
income_class$Economy<- ifelse(income_class$Economy=="St. Lucia","saint lucia", income_class$Economy)
income_class$Economy<- ifelse(income_class$Economy=="St. Vincent and the Grenadines","saint vincent and the grenadines", income_class$Economy)
income_class$Economy<- ifelse(income_class$Economy=="S?o Tom? and Principe","sao tome and principe", income_class$Economy)
income_class$Economy<- ifelse(income_class$Economy=="Slovak Republic","slovakia", income_class$Economy)
income_class$Economy<- ifelse(income_class$Economy=="Eswatini","swaziland", income_class$Economy)
income_class$Economy<- ifelse(income_class$Economy=="Timor-Leste","timor-leste", income_class$Economy)
income_class$Economy<- ifelse(income_class$Economy=="Trinidad and Tobago","trinidad and tobago", income_class$Economy)
income_class$Economy<- ifelse(income_class$Economy=="Turks and Caicos Islands","turks and caicos islands", income_class$Economy)
income_class$Economy<- ifelse(income_class$Economy=="United States","united states of america", income_class$Economy)
income_class$Economy<- ifelse(income_class$Economy=="Venezuela, RB","venezuela", income_class$Economy)
income_class$Economy<- ifelse(income_class$Economy=="Yemen, Rep.","yemen", income_class$Economy)
income_class$Economy<- ifelse(income_class$Economy=="Russian Federation","russia", income_class$Economy)





income_class$Country<- income_class$Economy

income_class$Country<-tolower(income_class$Country)


merge <- left_join(data, income_class, by=c("Country"="Country"),match='all')


saveRDS(merge,"data_co2_gini_02_21.RDS")

#############################
#summary statistics
rm(list=ls(all=TRUE))
library(GJRM)
library(copula)
library(xtable)


setwd("") #paste filepath here 

d1<- readRDS("data_co2_gini_01_21.RDS")
#Deleate Panama due to negative values
d1<- subset(d1, Country!="panama")
d1$lcarbon_cons_pc<- log(d1$carbon_cons_pc)

d_high<- subset(d1,`Income group`=='High income')
d_mid<- subset(d1,`Income group`=='Upper middle income')
d_low<- subset(d1,`Income group`=='Lower middle income' | `Income group`=='Low income' )


carbon_high <- cbind(length(na.omit(d_high[,"carbon_cons_pc"])), mean(na.omit(d_high[,"carbon_cons_pc"])), sd(na.omit(d_high[,"carbon_cons_pc"])),min(na.omit(d_high[,"carbon_cons_pc"])), max(na.omit(d_high[,"carbon_cons_pc"])))
gini_high <- cbind(length(na.omit(d_high[,"GINI"])), mean(na.omit(d_high[,"GINI"])), sd(na.omit(d_high[,"GINI"])),min(na.omit(d_high[,"GINI"])), max(na.omit(d_high[,"GINI"])))

carbon_mid <- cbind(length(na.omit(d_mid[,"carbon_cons_pc"])), mean(na.omit(d_mid[,"carbon_cons_pc"])), sd(na.omit(d_mid[,"carbon_cons_pc"])),min(na.omit(d_mid[,"carbon_cons_pc"])), max(na.omit(d_mid[,"carbon_cons_pc"])))
gini_mid <- cbind(length(na.omit(d_mid[,"GINI"])), mean(na.omit(d_mid[,"GINI"])), sd(na.omit(d_mid[,"GINI"])),min(na.omit(d_mid[,"GINI"])), max(na.omit(d_mid[,"GINI"])))

carbon_low <- cbind(length(na.omit(d_low[,"carbon_cons_pc"])), mean(na.omit(d_low[,"carbon_cons_pc"])), sd(na.omit(d_low[,"carbon_cons_pc"])),min(na.omit(d_low[,"carbon_cons_pc"])), max(na.omit(d_low[,"carbon_cons_pc"])))
gini_low  <- cbind(length(na.omit(d_low[,"GINI"])), mean(na.omit(d_low[,"GINI"])), sd(na.omit(d_low[,"GINI"])),min(na.omit(d_low[,"GINI"])), max(na.omit(d_low[,"GINI"])))


table <- rbind(carbon_high, gini_high, carbon_mid,  gini_mid, carbon_low, gini_low)


##################################################################################################################
##################################################################################################################
##################################################################################################################
#Kendall's tau

rm(list=ls(all=TRUE))
library(GJRM)
library(copula)
library(xtable)


setwd("") #paste filepath here 
d1<- readRDS("data_co2_gini_02_21.RDS")


dh<- subset(d1,`Income group`=='High income') 

myvars<- c("GINI","GDP", "Manu", "Serv", "Agri",  "Urban",  "fossil", "polity", "Year", "carbon_cons_pc","Country")
dh <- dh[myvars]
dh <- dh[complete.cases(dh), ]
#Deleate Panama due to negative values
dh<- subset(dh, Country!="panama")
dh$lcarbon_cons<- log(dh$carbon_cons_pc)


############################################################################################################################
#Model 
############################################################################################################################


eq.mu1 <- GINI~ s(GDP)+ Manu+ Serv+ Agri+  Urban+  fossil+ polity+ s(Year)
eq.si1 <- ~ s(GDP)+ Manu+ Serv+ Agri+ Urban+fossil+ polity+  s(Year)

eq.mu2 <- carbon_cons_pc~ s(GDP)+Manu+ Serv+ Agri+ Urban+fossil+ polity+ s(Year)
eq.si2 <- ~s(GDP)+ Manu+ Serv+ Agri+ Urban+ fossil+ polity+ s(Year)

eq.cop <- ~ s(GDP)+Manu+ Serv+ Agri+ Urban+ fossil+ polity+ s(Year)

form.biv <- list(eq.mu1, eq.mu2, eq.si1, eq.si2, eq.cop ) 

biv.modh <- gjrm(form.biv, data = dh, margins = c("LN", "LN"), Model = "B", BivD = "N", gamlssfit = TRUE)

# kendalls tau averages
#complete cases!

ktha <- jc.probs(x=biv.modh, y1=25.7, y2= 0.5, newdata= dh, type = "joint", intervals = FALSE)


#gdp
dgdp<- dh
dgdp$GDP<- dgdp$GDP + 1000

kthagdp <- jc.probs(x=biv.modh, y1=25.7, y2=0.5, newdata= dgdp, type = "joint", intervals = FALSE)
kthgdp<- mean(kthagdp$tau)- mean(ktha$tau)

# Manu
dManu<- dh
dManu$Manu<- dManu$Manu + 1

kthaManu <- jc.probs(x=biv.modh, y1=25.7, y2=0.5, newdata= dManu, type = "joint", intervals = FALSE)
kthManu<- mean(kthaManu$tau)- mean(ktha$tau)

# Serv
dServ<- dh
dServ$Serv<- dServ$Serv + 1

kthaServ <- jc.probs(x=biv.modh, y1=25.7, y2=0.5, newdata= dServ, type = "joint", intervals = FALSE)
kthServ<- mean(kthaServ$tau)- mean(ktha$tau)

# Agri
dAgri<- dh
dAgri$Agri<- dAgri$Agri + 1

kthaAgri <- jc.probs(x=biv.modh, y1=25.7, y2=0.5, newdata= dAgri, type = "joint", intervals = FALSE)
kthAgri<- mean(kthaAgri$tau)- mean(ktha$tau)

# Urban
dUrban<- dh
dUrban$Urban<- dhUrban$Urban + 1

kthaUrban <- jc.probs(x=biv.modh, y1=25.7, y2=0.5, newdata= dUrban, type = "joint", intervals = FALSE)
kthUrban<- mean(kthaUrban$tau)- mean(ktha$tau)


# Fossil
dhfossil<- dh
dhfossil$fossil<- dhfossil$fossil + 1

kthafossil <- jc.probs(x=biv.modh, y1=25.7, y2=0.5, newdata= dhfossil, type = "joint", intervals = FALSE)
kthfossil<- mean(kthafossil$tau)- mean(ktha$tau)


# Polity
dhpolity<- dh
dhpolity$polity<- dhpolity$polity + 1

kthapolity <- jc.probs(x=biv.modh, y1=25.7, y2=0.5, newdata= dhpolity, type = "joint", intervals = FALSE)
kthpolity<- mean(kthapolity$tau)- mean(ktha$tau)


# Year
dhYear<- dh
dhYear$Year<- dhYear$Year + 1

kthhaYear <- jc.probs(x=biv.modh, y1=25.7, y2=0.5, newdata= dhYear, type = "joint", intervals = FALSE)
kthYear<- mean(kthhaYear$tau)- mean(ktha$tau)





#####Middle income countries
dm<- subset(d1,`Income group`=='Upper middle income')

myvars<- c("GINI","GDP", "Manu", "Serv", "Agri",  "Urban",  "fossil", "polity", "Year", "carbon_cons_pc","Country")
dm <- dm[myvars]
dm <- dm[complete.cases(dm), ]
#Deleate Panama due to negative values
dm<- subset(dm, Country!="panama")
dm$lcarbon_cons<- log(dm$carbon_cons_pc)


biv.modm <- gjrm(form.biv, data = dm, margins = c("N", "LN"), Model = "B", BivD = "F", gamlssfit = TRUE)

# kendalls tau averages
#complete cases!

ktma <- jc.probs(x=biv.modm, y1=25.7, y2= 0.5, newdata= dm, type = "joint", intervals = FALSE)


#gdp
dgdp<- dm
dgdp$GDP<- dgdp$GDP + 1000

ktmagdp <- jc.probs(x=biv.modm, y1=25.7, y2=0.5, newdata= dgdp, type = "joint", intervals = FALSE)
ktmgdp<- mean(ktmagdp$tau)- mean(ktma$tau)

# Manu
dManu<- dm
dManu$Manu<- dManu$Manu + 1

ktmaManu <- jc.probs(x=biv.modm, y1=25.7, y2=0.5, newdata= dManu, type = "joint", intervals = FALSE)
ktmManu<- mean(ktmaManu$tau)- mean(ktma$tau)

# Serv
dServ<- dm
dServ$Serv<- dServ$Serv + 1

ktmaServ <- jc.probs(x=biv.modm, y1=25.7, y2=0.5, newdata= dServ, type = "joint", intervals = FALSE)
ktmServ<- mean(ktmaServ$tau)- mean(ktma$tau)

# Agri
dAgri<- dm
dAgri$Agri<- dAgri$Agri + 1

ktmaAgri <- jc.probs(x=biv.modm, y1=25.7, y2=0.5, newdata= dAgri, type = "joint", intervals = FALSE)
ktmAgri<- mean(ktmaAgri$tau)- mean(ktma$tau)

# Urban
dUrban<- dm
dUrban$Urban<- dUrban$Urban + 1

ktmaUrban <- jc.probs(x=biv.modm, y1=25.7, y2=0.5, newdata= dUrban, type = "joint", intervals = FALSE)
ktmUrban<- mean(ktmaUrban$tau)- mean(ktma$tau)


# Fossil
dmfossil<- dm
dmfossil$fossil<- dmfossil$fossil + 1

ktmafossil <- jc.probs(x=biv.modm, y1=25.7, y2=0.5, newdata= dmfossil, type = "joint", intervals = FALSE)
ktmfossil<- mean(ktmafossil$tau)- mean(ktma$tau)


# Polity
dmpolity<- dm
dmpolity$polity<- dmpolity$polity + 1

ktmapolity <- jc.probs(x=biv.modm, y1=25.7, y2=0.5, newdata= dmpolity, type = "joint", intervals = FALSE)
ktmpolity<- mean(ktmapolity$tau)- mean(ktma$tau)


# Year
dmYear<- dm
dmYear$Year<- dmYear$Year + 1

ktmaYear <- jc.probs(x=biv.modm, y1=25.7, y2=0.5, newdata= dmYear, type = "joint", intervals = FALSE)
ktmYear<- mean(ktmaYear$tau)- mean(ktma$tau)




#####low income countries

di<- subset(d1,`Income group`=='Lower middle income'| `Income group`=='Low income')
myvars<- c("GINI","GDP", "Manu", "Serv", "Agri",  "Urban",  "fossil", "polity", "Year", "carbon_cons_pc","Country")
di <- di[myvars]
di <- di[complete.cases(di), ]
#Deleate Panama due to negative values
di<- subset(di, Country!="panama")
di$lcarbon_cons<- log(di$carbon_cons_pc)


biv.modi <- gjrm(form.biv, data = di, margins = c("N", "LN"), Model = "B", BivD = "F", gamlssfit = TRUE)

# kendalls tau averages
#complete cases!

ktia <- jc.probs(x=biv.modi, y1=25.7, y2= 0.5, newdata= di, type = "joint", intervals = FALSE)


#gdp
dgdp<- di
dgdp$GDP<- dgdp$GDP + 1000

ktiagdp <- jc.probs(x=biv.modi, y1=25.7, y2=0.5, newdata= dgdp, type = "joint", intervals = FALSE)
ktigdp<- mean(ktiagdp$tau)- mean(ktia$tau)

# Manu
dManu<- di
dManu$Manu<- dManu$Manu + 1

ktiaManu <- jc.probs(x=biv.modi, y1=25.7, y2=0.5, newdata= dManu, type = "joint", intervals = FALSE)
ktiManu<- mean(ktiaManu$tau)- mean(ktia$tau)

# Serv
dServ<- di
dServ$Serv<- dServ$Serv + 1

ktiaServ <- jc.probs(x=biv.modm, y1=25.7, y2=0.5, newdata= dServ, type = "joint", intervals = FALSE)
ktiServ<- mean(ktiaServ$tau)- mean(ktia$tau)

# Agri
dAgri<- di
dAgri$Agri<- dAgri$Agri + 1

ktiaAgri <- jc.probs(x=biv.modi, y1=25.7, y2=0.5, newdata= dAgri, type = "joint", intervals = FALSE)
ktiAgri<- mean(ktiaAgri$tau)- mean(ktia$tau)

# Urban
dUrban<- di
dUrban$Urban<- dUrban$Urban + 1

ktiaUrban <- jc.probs(x=biv.modi, y1=25.7, y2=0.5, newdata= dUrban, type = "joint", intervals = FALSE)
ktiUrban<- mean(ktiaUrban$tau)- mean(ktia$tau)


# Fossil
difossil<- di
difossil$fossil<- difossil$fossil + 1

ktiafossil <- jc.probs(x=biv.modi, y1=25.7, y2=0.5, newdata= difossil, type = "joint", intervals = FALSE)
ktifossil<- mean(ktiafossil$tau)- mean(ktia$tau)


# Polity
dipolity<- di
dipolity$polity<- dipolity$polity + 1

ktiapolity <- jc.probs(x=biv.modi, y1=25.7, y2=0.5, newdata= dipolity, type = "joint", intervals = FALSE)
ktipolity<- mean(ktiapolity$tau)- mean(ktia$tau)


# Year
diYear<- di
diYear$Year<- diYear$Year + 1

ktiaYear <- jc.probs(x=biv.modi, y1=25.7, y2=0.5, newdata= diYear, type = "joint", intervals = FALSE)
ktiYear<- mean(ktiaYear$tau)- mean(ktia$tau)








#Table marginal effects

ameh<- rbind(kthgdp, kthManu, kthServ, kthAgri, kthUrban, kthfossil, kthpolity, kthYear)
amem<- rbind(ktmgdp, ktmManu, ktmServ, ktmAgri, ktmUrban, ktmfossil, ktmpolity, ktmYear)
amei<- rbind(ktigdp, ktiManu, ktiServ, ktiAgri, ktiUrban, ktifossil, ktipolity, ktiYear)



ame<- cbind(ameh, amem, amei)
rownames(ame)<- c( "GDP", "Manu", "Serv", "Agri", "Urban", "Fossil", "Polity", "Year")
xtable(ame, digits = 3)










#################################################################################################################
#################################################################################################################
#################################################################################################################
#copula high income countries


rm(list=ls(all=TRUE))
library(GJRM)
library(copula)
library(xtable)


setwd("") #paste filepath here 
d1<- readRDS("data_co2_gini_02_21.RDS")

d<- subset(d1,`Income group`=='High income')

myvars<- c("GINI","GDP", "Manu", "Serv", "Agri",  "Urban",  "fossil", "polity", "Year", "carbon_cons_pc","Country")
d <- d[myvars]
d <- d[complete.cases(d), ]
#Deleate Panama due to negative values
d<- subset(d, Country!="panama")
d$lcarbon_cons<- log(d$carbon_cons_pc)
############################################################################################################################
#Model 
############################################################################################################################


eq.mu1 <- GINI~ s(GDP)+ Manu+ Serv+ Agri+  Urban+  fossil+ polity+ s(Year)
eq.si1 <- ~ s(GDP)+ Manu+ Serv+ Agri+ Urban+fossil+ polity+  s(Year)

eq.mu2 <- carbon_cons_pc~ s(GDP)+Manu+ Serv+ Agri+ Urban+fossil+ polity+ s(Year)
eq.si2 <- ~s(GDP)+ Manu+ Serv+ Agri+ Urban+ fossil+ polity+ s(Year)

eq.cop <- ~ s(GDP)+Manu+ Serv+ Agri+ Urban+ fossil+ polity+ s(Year)

form.biv <- list(eq.mu1, eq.mu2, eq.si1, eq.si2, eq.cop ) 

biv.mod <- gjrm(form.biv, data = d, margins = c("LN", "LN"), Model = "B", BivD = "N", gamlssfit = TRUE)
post.check(biv.mod)
summary(biv.mod)


#Histogram
par(mfrow = c(1, 1))
hist(biv.mod$tau,col=rgb(0,0,1,1/1.5), probability = T,breaks = 30, xlim= c(-1,1),
     xlab = "Kendall's tau", main = NULL)

legend("topright",  c("high income"),col = rgb(0,0,1,1/1.5), pch = 15, bty = "n")


copula.contour <- function(object, newdata, ls = 100, range.v1 = NULL, range.v2 = NULL, levels=NULL, ...){
  
  mar1 <- object$margins[1]
  mar2 <- object$margins[2]
  BivD <- object$BivD 
  
  eta1    <- predict(object, eq = 1, newdata = newdata)
  eta2    <- predict(object, eq = 2, newdata = newdata)
  sigma21 <- esp.tr(predict(object, eq = 3, newdata = newdata), mar1)$vrb
  sigma22 <- esp.tr(predict(object, eq = 4, newdata = newdata), mar2)$vrb
  theta   <- teta.tr(object$VC, predict(object, eq = 5, newdata = newdata))$teta
  
  if(is.null(range.v1)) y1.r <- range(object$y1) else y1.r <- range.v1
  if(is.null(range.v2)) y2.r <- range(object$y2) else y2.r <- range.v2 
  
  size <- ls
  
  
  x1 <- seq(from = y1.r[1], to = y1.r[2], length.out = size)         
  x2 <- seq(from = y2.r[1], to = y2.r[2], length.out = size)                   
  
  x11 <- rep(x1, each = size)
  x22 <- rep(x2, times = size)
  

  dHs1 <- distrHsAT(x11, c(eta1), c(sigma21), c(log(sigma21)), margin2 = mar1, min.dn = 1e-40, min.pr = 1e-40, max.pr = 1) 
  pdf1 <- dHs1$pdf2
  p1   <- dHs1$p2  
  
  dHs2 <-distrHsAT(x22, c(eta2), c(sigma22), c(log(sigma22)), margin2 = mar2, min.dn = 1e-40, min.pr = 1e-40, max.pr = 1) 
  pdf2 <- dHs2$pdf2
  p2   <- dHs2$p2
  
  
  md <- copgHsAT(p1, p2, c(theta), BivD, Ln = TRUE, par2 = NULL, min.dn = 1e-40, min.pr = 1e-40, max.pr = 1)$c.copula2.be1be2*pdf1*pdf2
  z  <- matrix(data = md, nrow = size, byrow = TRUE)
 
  contour(x1, x2, z, levels=levels, ...)
  
}


myplot <- function(country="germany", year=2015, polity=NULL, fossil=NULL, Serv=NULL, thresholds = c(25.7, 0.5), range.v1 = c(10, 50), range.v2 = c(0, 30), 
                   levels=seq(0, 0.01, by=0.0005)){
  newd<- subset(d, Country==country & Year == year)
  if(!is.null(polity)){
    newd$polity <- polity
  }
  if(!is.null(fossil)){
    newd$fossil <- fossil
  }
  if(!is.null(Serv)){
    newd$Serv <- Serv
  }
  joint <- jc.probs(x=biv.mod, y1=thresholds[1], y2=thresholds[2], newdata= newd, type = "joint", intervals = FALSE)
  tau<- round(joint$tau, 3)
  thGINI<- round(joint$p1,3)
  thCarbon<- round(joint$p2,3)
  thGC<- round(joint$p12,3)
  text <- paste(country,"& ", newd$Year, "& ",newd$polity,"& ", round(newd$fossil,2), "& ", round(newd$Serv,2), sep="")
  copula.contour(biv.mod, newd, ls = 100, range.v1 = range.v1, range.v2 = range.v2, levels=levels, xlab= "GINI", ylab= "Carbon", 
                 main = text)
  legend("bottomright", legend=paste("Kendall's ", expression(tau), "=", tau, sep=""), bty = "n")
  return(list(tau=tau, main=text, thGINI= thGINI, thCarbon=thCarbon,thGC= thGC ))
}
(tau <- myplot())


dorg<- d
dpol<- d
dpol$polity<- dpol$polity + 1

thetaorg   <- teta.tr(biv.mod$VC, predict(biv.mod, eq = 5, newdata = dorg))$teta
thetapol   <- teta.tr(biv.mod$VC, predict(biv.mod, eq = 5, newdata = dpol))$teta
mean(thetaorg ) - mean( thetapol)



m1<-myplot(year=1997, country="france")
m2<-myplot(year=2008, country="france")
m3<-myplot(year=2009, country="france")
m4<-myplot(year=2015, country="france")
m5<-myplot(year=2015, country="france", fossil=80)
m5a<-myplot(year=2015, country="france", fossil=10)
m5b<-myplot(year=2015, country="france", polity=10)
m5c<-myplot(year=2015, country="france", polity=3)
m5d<-myplot(year=2015, country="france", Serv=20)
m5e<-myplot(year=2015, country="france", Serv=20, fossil=10) #degrowth scenario

m6<-myplot(year=1997, country="germany")
m7<-myplot(year=2008, country="germany")
m8<-myplot(year=2009, country="germany")
m9<-myplot(year=2015, country="germany")
m10<-myplot(year=2015, country="germany", fossil=50)
m10a<-myplot(year=2015, country="germany", fossil=10)
m10b<-myplot(year=2015, country="germany", polity=9)
m10c<-myplot(year=2015, country="germany", polity=3)
m10d<-myplot(year=2015, country="germany", Serv=20)
m10e<-myplot(year=2015, country="germany", Serv=20, fossil=10) #degrowth scenario




m11<-myplot(year=1997, country="australia")
m12<-myplot(year=2008, country="australia")
m13<-myplot(year=2009, country="australia")
m14<-myplot(year=2015, country="australia")
m14a<-myplot(year=2015, country="australia", fossil=50)
m14b<-myplot(year=2015, country="australia", fossil=10)
m14c<-myplot(year=2015, country="australia", polity=9)
m14d<-myplot(year=2015, country="australia", polity=3)
m14e<-myplot(year=2015, country="australia", Serv=20)
m14f<-myplot(year=2015, country="australia", Serv=20, fossil=10) #degrowth scenario


m15<-myplot(year=1997, country="united states of america")
m16<-myplot(year=2008, country="united states of america")
m17<-myplot(year=2009, country="united states of america")
m18<-myplot(year=2015, country="united states of america")
m18a<-myplot(year=2015, country="united states of america", fossil=50)
m18b<-myplot(year=2015, country="united states of america", fossil=10)
m18c<-myplot(year=2015, country="united states of america", polity=9)
m18d<-myplot(year=2015, country="united states of america", polity=3)
m18e<-myplot(year=2015, country="united states of america", Serv=20)
#m18f<-myplot(year=2015, country="united states of america", Serv=90)
m18f<-myplot(year=2015, country="united states of america", Serv=20, fossil = 10)

m19<-myplot(year=1997, country="united kingdom")
m20<-myplot(year=2008, country="united kingdom")
m21<-myplot(year=2009, country="united kingdom")
m23<-myplot(year=2015, country="united kingdom")
m23a<-myplot(year=2015, country="united kingdom", fossil=50)
m23b<-myplot(year=2015, country="united kingdom", fossil=10)
m23c<-myplot(year=2015, country="united kingdom", polity=9)
m23d<-myplot(year=2015, country="united kingdom", polity=3)
m23e<-myplot(year=2015, country="united kingdom", Serv=20)
m23f<-myplot(year=2015, country="united kingdom", Serv=20, fossil=10) #degrowth scenario


m24<-myplot(year=1997, country="switzerland")
m25<-myplot(year=2008, country="switzerland")
m26<-myplot(year=2009, country="switzerland")
m27<-myplot(year=2015, country="switzerland")


m28<-myplot(year=1997, country="norway")
m29<-myplot(year=2008, country="norway")
m30<-myplot(year=2009, country="norway")
m40<-myplot(year=2015, country="norway")


m41<-myplot(year=1997, country="sweden")
m42<-myplot(year=2008, country="sweden")
m43<-myplot(year=2009, country="sweden")
m44<-myplot(year=2015, country="sweden")

m45<-myplot(year=1997, country="chile")
m46<-myplot(year=2008, country="chile")
m47<-myplot(year=2009, country="chile")
m48<-myplot(year=2015, country="chile")

m49<-myplot(year=1997, country="canada")
m50<-myplot(year=2008, country="canada")
m51<-myplot(year=2009, country="canada")
m52<-myplot(year=2015, country="canada")

m53<-myplot(year=1997, country="canada")
m54<-myplot(year=2008, country="canada")
m55<-myplot(year=2009, country="canada")
m56<-myplot(year=2015, country="canada")

m57<-myplot(year=2010, country="oman")
m58<-myplot(year=2014, country="bahrain")
m59<-myplot(year=2013, country="qatar")
m60<-myplot(year=2008, country="united arab emirates")

m61<- myplot(year=2013, country="saudi arabia")

main<- rbind(m1$main, m2$main, m3$main, m4$main, m5$main,m5a$main, m5b$main, m5c$main,m5d$main, m5e$main, m6$main, m7$main, m8$main, m9$main, m10$main,
             m10a$main, m10b$main, m10c$main, m10d$main, m10e$main,
             m11$main, m12$main, m13$main, m14$main,m14a$main, m14b$main, m14c$main, m14d$main,  m14e$main,  m14f$main,
             m15$main, m16$main, m17$main, m18$main,m18a$main, m18b$main, m18c$main, m18d$main, m18e$main, m18f$main, m19$main, m20$main,
             m21$main, m23$main, m23a$main, m23b$main, m23c$main, m23d$main, m23e$main, m23f$main) 

ktau<- rbind(m1$tau, m2$tau, m3$tau, m4$tau, m5$tau,m5a$tau, m5b$tau, m5c$tau,m5d$tau, m5e$tau, m6$tau, m7$tau, m8$tau, m9$tau, m10$tau,
             m10a$tau, m10b$tau, m10c$tau, m10d$tau, m10e$tau, 
             m11$tau, m12$tau, m13$tau, m14$tau,m14a$tau, m14b$tau, m14c$tau, m14d$tau,  m14e$tau,  m14f$tau,
             m15$tau, m16$tau, m17$tau, m18$tau,m18a$tau, m18b$tau, m18c$tau, m18d$tau, m18e$tau, m18f$tau, m19$tau, m20$tau,
             m21$tau, m23$tau, m23a$tau, m23b$tau, m23c$tau, m23d$tau, m23e$tau, m23f$tau)


thGINI<- rbind(m1$thGINI, m2$thGINI, m3$thGINI, m4$thGINI, m5$thGINI,m5a$thGINI, m5b$thGINI, m5c$thGINI,m5d$thGINI, m5e$thGINI, m6$thGINI, m7$thGINI, m8$thGINI, m9$thGINI, m10$thGINI,
               m10a$thGINI, m10b$thGINI, m10c$thGINI, m10d$thGINI, m10e$thGINI,
               m11$thGINI, m12$thGINI, m13$thGINI, m14$thGINI,m14a$thGINI, m14b$thGINI, m14c$thGINI, m14d$thGINI,  m14e$thGINI,  m14f$thGINI,
               m15$thGINI, m16$thGINI, m17$thGINI, m18$thGINI,m18a$thGINI, m18b$thGINI, m18c$thGINI, m18d$thGINI, m18e$thGINI, m18f$thGINI, m19$thGINI, m20$thGINI,
               m21$thGINI, m23$thGINI, m23a$thGINI, m23b$thGINI, m23c$thGINI, m23d$thGINI, m23e$thGINI, m23f$thGINI)

thCarbon<- rbind(m1$thCarbon, m2$thCarbon, m3$thCarbon, m4$thCarbon, m5$thCarbon,m5a$thCarbon, m5b$thCarbon, m5c$thCarbon,m5d$thCarbon, m5e$thCarbon, m6$thCarbon, m7$thCarbon, m8$thCarbon, m9$thCarbon, m10$thCarbon,
                 m10a$thCarbon, m10b$thCarbon, m10c$thCarbon, m10d$thCarbon, m10e$thCarbon, 
                 m11$thCarbon, m12$thCarbon, m13$thCarbon, m14$thCarbon,m14a$thCarbon, m14b$thCarbon, m14c$thCarbon, m14d$thCarbon,  m14e$thCarbon,  m14f$thCarbon,
                 m15$thCarbon, m16$thCarbon, m17$thCarbon, m18$thCarbon,m18a$thCarbon, m18b$thCarbon, m18c$thCarbon, m18d$thCarbon, m18e$thCarbon, m18f$thCarbon, m19$thCarbon, m20$thCarbon,
                 m21$thCarbon, m23$thCarbon, m23a$thCarbon, m23b$thCarbon, m23c$thCarbon, m23d$thCarbon, m23e$thCarbon, m23f$thCarbon) 


thGC<- rbind(m1$thGC, m2$thGC, m3$thGC, m4$thGC, m5$thGC,m5a$thGC, m5b$thGC, m5c$thGC,m5d$thGC, m5e$thGC, m6$thGC, m7$thGC, m8$thGC, m9$thGC, m10$thGC,
             m10a$thGC, m10b$thGC, m10c$thGC, m10d$thGC, m10e$thGC, 
             m11$thGC, m12$thGC, m13$thGC, m14$thGC,m14a$thGC, m14b$thGC, m14c$thGC, m14d$thGC,  m14e$thGC,  m14f$thGC,
             m15$thGC, m16$thGC, m17$thGC, m18$thGC,m18a$thGC, m18b$thGC, m18c$thGC, m18d$thGC, m18e$thGC, m18f$thGC, m19$thGC, m20$thGC,
             m21$thGC, m23$thGC, m23a$thGC, m23b$thGC, m23c$thGC, m23d$thGC, m23e$thGC, m23f$thGC)


tab_country<- cbind(main, ktau, thGINI, thCarbon, thGC)
xtable(tab_country)


##################################################################################################################
##################################################################################################################
##################################################################################################################
#Copula middle income countries

rm(list=ls(all=TRUE))
library(GJRM)
library(copula)
library(xtable)


setwd("") #paste filepath here 
d1<- readRDS("data_co2_gini_02_21.RDS")
d<- subset(d1,`Income group`=='Upper middle income')

myvars<- c("GINI","GDP", "Manu", "Serv", "Agri",  "Urban",  "fossil", "polity", "Year", "carbon_cons_pc","Country")

############################################################################################################################
#Model middle income countries
############################################################################################################################


eq.mu1 <- GINI~ s(GDP)+ Manu+ Serv+ Agri+  Urban+  fossil+ polity+ s(Year)
eq.si1 <- ~ s(GDP)+ Manu+ Serv+ Agri+ Urban+fossil+ polity+  s(Year)

eq.mu2 <- carbon_cons_pc~ s(GDP)+Manu+ Serv+ Agri+ Urban+fossil+ polity+ s(Year)
eq.si2 <- ~s(GDP)+ Manu+ Serv+ Agri+ Urban+ fossil+ polity+ s(Year)

eq.cop <- ~ s(GDP)+Manu+ Serv+ Agri+ Urban+ fossil+ polity+ s(Year)

form.biv <- list(eq.mu1, eq.mu2, eq.si1, eq.si2, eq.cop ) 

biv.mod <- gjrm(form.biv, data = d, margins = c("N", "LN"), Model = "B", BivD = "F", gamlssfit = TRUE)
post.check(biv.mod)
summary(biv.mod)




#Histogram
par(mfrow = c(1, 1))
hist(biv.mod$tau,col=rgb(0,0,1,1/1.5), probability = T,breaks = 30, xlim= c(-1,1),
     xlab = "Kendall's tau", main = NULL)

legend("topright",  c("middle income"),col = rgb(0,0,1,1/1.5), pch = 15, bty = "n")


copula.contour <- function(object, newdata, ls = 100, range.v1 = NULL, range.v2 = NULL, levels=NULL, ...){
  
  mar1 <- object$margins[1]
  mar2 <- object$margins[2]
  BivD <- object$BivD 
  
  eta1    <- predict(object, eq = 1, newdata = newdata)
  eta2    <- predict(object, eq = 2, newdata = newdata)
  sigma21 <- esp.tr(predict(object, eq = 3, newdata = newdata), mar1)$vrb
  sigma22 <- esp.tr(predict(object, eq = 4, newdata = newdata), mar2)$vrb
  #nu2     <- esp.tr(predict(object, eq = 5, newdata = newdata), mar1)$vrb
  theta   <- teta.tr(object$VC, predict(object, eq = 5, newdata = newdata))$teta
  
  if(is.null(range.v1)) y1.r <- range(object$y1) else y1.r <- range.v1
  if(is.null(range.v2)) y2.r <- range(object$y2) else y2.r <- range.v2 
  
  size <- ls
  
  
  x1 <- seq(from = y1.r[1], to = y1.r[2], length.out = size)         
  x2 <- seq(from = y2.r[1], to = y2.r[2], length.out = size)                   
  
  x11 <- rep(x1, each = size)
  x22 <- rep(x2, times = size)
  
  dHs1 <- distrHsAT(x11, c(eta1), c(sigma21), c(log(sigma21)), margin2 = mar1, min.dn = 1e-40, min.pr = 1e-40, max.pr = 1) 
  pdf1 <- dHs1$pdf2
  p1   <- dHs1$p2  
  
  dHs2 <-distrHsAT(x22, c(eta2), c(sigma22), c(log(sigma22)), margin2 = mar2, min.dn = 1e-40, min.pr = 1e-40, max.pr = 1) 
  pdf2 <- dHs2$pdf2
  p2   <- dHs2$p2
  

  
  md <- copgHsAT(p1, p2, c(theta), BivD, Ln = TRUE, par2 = NULL, min.dn = 1e-40, min.pr = 1e-40, max.pr = 1)$c.copula2.be1be2*pdf1*pdf2
  z  <- matrix(data = md, nrow = size, byrow = TRUE)
  
  contour(x1, x2, z, levels=levels, ...)
  
}



myplot <- function(country="argentina", year=2014, polity=NULL, fossil=NULL, Serv=NULL, thresholds = c(25.7, 0.5), range.v1 = c(5, 80), range.v2 = c(0, 30), 
                   levels=seq(0, 0.01, by=0.0005)){
  newd<- subset(d, Country==country & Year == year)
  if(!is.null(polity)){
    newd$polity <- polity
  }
  if(!is.null(fossil)){
    newd$fossil <- fossil
  }
  if(!is.null(Serv)){
    newd$Serv <- Serv
  }
  joint <- jc.probs(x=biv.mod, y1=thresholds[1], y2=thresholds[2], newdata= newd, type = "joint", intervals = FALSE)
  tau<- round(joint$tau, 3)
  thGINI<- round(joint$p1,3)
  thCarbon<- round(joint$p2,3)
  thGC<- round(joint$p12,3)
  text <- paste(country,"& ", newd$Year, "&",newd$polity,"&", round(newd$fossil,2),"&",round(newd$Serv,2), sep="")
  copula.contour(biv.mod, newd, ls = 100, range.v1 = range.v1, range.v2 = range.v2, levels=levels, xlab= "GINI", ylab= "Carbon", 
                 main = text)
  legend("topleft", legend=paste("Kendall's ", expression(tau), "=", tau, sep=""), bty = "n")
  return(list(tau=tau, main=text, thGINI= thGINI, thCarbon=thCarbon,thGC= thGC ))
}
(tau <- myplot())


m1<-myplot(year=1997, country="argentina")
m2<-myplot(year=2008, country="argentina")
m3<-myplot(year=2009, country="argentina")
m4<-myplot(year=2014, country="argentina")
m5<-myplot(year=2014, country="argentina", fossil=50)
m5a<-myplot(year=2014, country="argentina", fossil=10)
m5b<-myplot(year=2014, country="argentina", polity=10)
m5c<-myplot(year=2014, country="argentina", polity=3)
m5d<-myplot(year=2014, country="argentina", Serv=20)
m5e<-myplot(year=2014, country="argentina", Serv=20, fossil=10)

#m6<-myplot(year=1997, country="china")
m7<-myplot(year=2008, country="china")
m8<-myplot(year=2009, country="china")
m9<-myplot(year=2014, country="china")
m10<-myplot(year=2014, country="china", fossil=50)
m10a<-myplot(year=2014, country="china", fossil=10)
m10b<-myplot(year=2014, country="china", polity=10)
m10c<-myplot(year=2014, country="china", polity=3)
m10d<-myplot(year=2014, country="china", Serv=20)
m10e<-myplot(year=2014, country="china", Serv=20, fossil=10)


m11<-myplot(year=1997, country="south africa")
m12<-myplot(year=2008, country="south africa")
m13<-myplot(year=2009, country="south africa")
m14<-myplot(year=2014, country="south africa")
m14a<-myplot(year=2014, country="south africa", fossil=50)
m14b<-myplot(year=2014, country="south africa", fossil=10)
m14c<-myplot(year=2014, country="south africa", polity=10)
m14d<-myplot(year=2014, country="south africa", polity=3)
m14e<-myplot(year=2014, country="south africa", Serv=20)
m14f<-myplot(year=2014, country="south africa", Serv=20, fossil=10)

m15<-myplot(year=1997, country="brazil")
m16<-myplot(year=2008, country="brazil")
m17<-myplot(year=2009, country="brazil")
m18<-myplot(year=2014, country="brazil")
m18a<-myplot(year=2014, country="brazil", fossil=80)
m18b<-myplot(year=2014, country="brazil", fossil=10)
m18c<-myplot(year=2014, country="brazil", polity=10)
m18d<-myplot(year=2014, country="brazil", polity=3)
m18e<-myplot(year=2014, country="brazil", Serv=20)
m18f<-myplot(year=2014, country="brazil", Serv=20, fossil=10)


#m19<-myplot(year=1997, country="russia")
m20<-myplot(year=2008, country="russia")
m21<-myplot(year=2009, country="russia")
m23<-myplot(year=2014, country="russia")
m23a<-myplot(year=2014, country="russia", fossil=50)
m23b<-myplot(year=2014, country="russia", fossil=10)
m23c<-myplot(year=2014, country="russia", polity=10)
m23d<-myplot(year=2014, country="russia", polity=8)
m23e<-myplot(year=2014, country="russia", Serv=20)
m23f<-myplot(year=2014, country="russia", Serv=20, fossil=10)

main<- rbind(m1$main, m2$main, m3$main, m4$main, m5$main,m5a$main, m5b$main, m5c$main,m5d$main, m5e$main, m7$main, m8$main, m9$main, m10$main,
             m10a$main, m10b$main, m10c$main, m10d$main, m10e$main,
             m11$main, m12$main, m13$main, m14$main,m14a$main, m14b$main, m14c$main, m14d$main,  m14e$main,  m14f$main,
             m15$main, m16$main, m17$main, m18$main,m18a$main, m18b$main, m18c$main, m18d$main, m18e$main, m18f$main, m20$main,
             m21$main, m23$main, m23a$main, m23b$main, m23c$main, m23d$main, m23e$main, m23f$main) 


ktau<- rbind(m1$tau, m2$tau, m3$tau, m4$tau, m5$tau,m5a$tau, m5b$tau, m5c$tau,m5d$tau, m5e$tau, m7$tau, m8$tau, m9$tau, m10$tau,
             m10a$tau, m10b$tau, m10c$tau, m10d$tau, m10e$tau, 
             m11$tau, m12$tau, m13$tau, m14$tau,m14a$tau, m14b$tau, m14c$tau, m14d$tau,  m14e$tau,  m14f$tau,
             m15$tau, m16$tau, m17$tau, m18$tau,m18a$tau, m18b$tau, m18c$tau, m18d$tau, m18e$tau, m18f$tau, m20$tau,
             m21$tau, m23$tau, m23a$tau, m23b$tau, m23c$tau, m23d$tau, m23e$tau, m23f$tau)


thGINI<- rbind(m1$thGINI, m2$thGINI, m3$thGINI, m4$thGINI, m5$thGINI,m5a$thGINI, m5b$thGINI, m5c$thGINI,m5d$thGINI, m5e$thGINI, m7$thGINI, m8$thGINI, m9$thGINI, m10$thGINI,
               m10a$thGINI, m10b$thGINI, m10c$thGINI, m10d$thGINI, m10e$thGINI,
               m11$thGINI, m12$thGINI, m13$thGINI, m14$thGINI,m14a$thGINI, m14b$thGINI, m14c$thGINI, m14d$thGINI,  m14e$thGINI,  m14f$thGINI,
               m15$thGINI, m16$thGINI, m17$thGINI, m18$thGINI,m18a$thGINI, m18b$thGINI, m18c$thGINI, m18d$thGINI, m18e$thGINI, m18f$thGINI, m20$thGINI,
               m21$thGINI, m23$thGINI, m23a$thGINI, m23b$thGINI, m23c$thGINI, m23d$thGINI, m23e$thGINI, m23f$thGINI)

thCarbon<- rbind(m1$thCarbon, m2$thCarbon, m3$thCarbon, m4$thCarbon, m5$thCarbon,m5a$thCarbon, m5b$thCarbon, m5c$thCarbon,m5d$thCarbon, m5e$thCarbon, m7$thCarbon, m8$thCarbon, m9$thCarbon, m10$thCarbon,
                 m10a$thCarbon, m10b$thCarbon, m10c$thCarbon, m10d$thCarbon, m10e$thCarbon, 
                 m11$thCarbon, m12$thCarbon, m13$thCarbon, m14$thCarbon,m14a$thCarbon, m14b$thCarbon, m14c$thCarbon, m14d$thCarbon,  m14e$thCarbon,  m14f$thCarbon,
                 m15$thCarbon, m16$thCarbon, m17$thCarbon, m18$thCarbon,m18a$thCarbon, m18b$thCarbon, m18c$thCarbon, m18d$thCarbon, m18e$thCarbon, m18f$thCarbon, m20$thCarbon,
                 m21$thCarbon, m23$thCarbon, m23a$thCarbon, m23b$thCarbon, m23c$thCarbon, m23d$thCarbon, m23e$thCarbon, m23f$thCarbon) 


thGC<- rbind(m1$thGC, m2$thGC, m3$thGC, m4$thGC, m5$thGC,m5a$thGC, m5b$thGC, m5c$thGC,m5d$thGC, m5e$thGC, m7$thGC, m8$thGC, m9$thGC, m10$thGC,
             m10a$thGC, m10b$thGC, m10c$thGC, m10d$thGC, m10e$thGC, 
             m11$thGC, m12$thGC, m13$thGC, m14$thGC,m14a$thGC, m14b$thGC, m14c$thGC, m14d$thGC,  m14e$thGC,  m14f$thGC,
             m15$thGC, m16$thGC, m17$thGC, m18$thGC,m18a$thGC, m18b$thGC, m18c$thGC, m18d$thGC, m18e$thGC, m18f$thGC, m20$thGC,
             m21$thGC, m23$thGC, m23a$thGC, m23b$thGC, m23c$thGC, m23d$thGC, m23e$thGC, m23f$thGC)

tab_country<- cbind(main, ktau, thGINI, thCarbon, thGC)
xtable(tab_country)


#################################################################################################################################
#################################################################################################################################
#################################################################################################################################
#copula low income countries

rm(list=ls(all=TRUE))
library(GJRM)
library(copula)
library(xtable)


setwd("") #paste filepath here 
d1<- readRDS("data_co2_gini_02_21.RDS")
d<- subset(d1,`Income group`=='Lower middle income'| `Income group`=='Low income')

myvars<- c("GINI","GDP", "Manu", "Serv", "Agri",  "Urban",  "fossil", "polity", "Year", "carbon_cons_pc","Country")


############################################################################################################################
#Model low income countries
############################################################################################################################


eq.mu1 <- GINI~ s(GDP)+ Manu+ Serv+ Agri+  Urban+  fossil+ polity+ s(Year)
eq.si1 <- ~ s(GDP)+ Manu+ Serv+ Agri+ Urban+fossil+ polity+  s(Year)

eq.mu2 <- carbon_cons_pc~ s(GDP)+Manu+ Serv+ Agri+ Urban+fossil+ polity+ s(Year)
eq.si2 <- ~s(GDP)+ Manu+ Serv+ Agri+ Urban+ fossil+ polity+ s(Year)

eq.cop <- ~ s(GDP)+Manu+ Serv+ Agri+ Urban+ fossil+ polity+ s(Year)

form.biv <- list(eq.mu1, eq.mu2, eq.si1, eq.si2, eq.cop ) 

biv.mod <- gjrm(form.biv, data = d, margins = c("N", "LN"), Model = "B", BivD = "F", gamlssfit = TRUE)
post.check(biv.mod)
summary(biv.mod)




#Histogram
par(mfrow = c(1, 1))
hist(biv.mod$tau,col=rgb(0,0,1,1/1.5), probability = T,breaks = 30, xlim= c(-1,1),
     xlab = "Kendall's tau", main = NULL)

legend("topright",  c("low income"),col = rgb(0,0,1,1/1.5), pch = 15, bty = "n")


copula.contour <- function(object, newdata, ls = 100, range.v1 = NULL, range.v2 = NULL, levels=NULL, ...){
  
  mar1 <- object$margins[1]
  mar2 <- object$margins[2]
  BivD <- object$BivD 
  
  eta1    <- predict(object, eq = 1, newdata = newdata)
  eta2    <- predict(object, eq = 2, newdata = newdata)
  sigma21 <- esp.tr(predict(object, eq = 3, newdata = newdata), mar1)$vrb
  sigma22 <- esp.tr(predict(object, eq = 4, newdata = newdata), mar2)$vrb
  #nu2     <- esp.tr(predict(object, eq = 5, newdata = newdata), mar1)$vrb
  theta   <- teta.tr(object$VC, predict(object, eq = 5, newdata = newdata))$teta
  
  if(is.null(range.v1)) y1.r <- range(object$y1) else y1.r <- range.v1
  if(is.null(range.v2)) y2.r <- range(object$y2) else y2.r <- range.v2 
  
  size <- ls
  
  
  x1 <- seq(from = y1.r[1], to = y1.r[2], length.out = size)         
  x2 <- seq(from = y2.r[1], to = y2.r[2], length.out = size)                   
  
  x11 <- rep(x1, each = size)
  x22 <- rep(x2, times = size)
  

  dHs1 <- distrHsAT(x11, c(eta1), c(sigma21), c(log(sigma21)), margin2 = mar1, min.dn = 1e-40, min.pr = 1e-40, max.pr = 1) 
  pdf1 <- dHs1$pdf2
  p1   <- dHs1$p2  
  
  dHs2 <-distrHsAT(x22, c(eta2), c(sigma22), c(log(sigma22)), margin2 = mar2, min.dn = 1e-40, min.pr = 1e-40, max.pr = 1) 
  pdf2 <- dHs2$pdf2
  p2   <- dHs2$p2
  

  md <- copgHsAT(p1, p2, c(theta), BivD, Ln = TRUE, par2 = NULL, min.dn = 1e-40, min.pr = 1e-40, max.pr = 1)$c.copula2.be1be2*pdf1*pdf2
  z  <- matrix(data = md, nrow = size, byrow = TRUE)

  contour(x1, x2, z, levels=levels, ...)
  
}



myplot <- function(country="india", year=2004, polity=NULL, fossil=NULL, Serv=NULL, thresholds = c(25.7, 0.5), range.v1 = c(10, 60), range.v2 = c(0, 20), 
                   levels=seq(0, 0.01, by=0.0005)){
  newd<- subset(d, Country==country & Year == year)
  if(!is.null(polity)){
    newd$polity <- polity
  }
  if(!is.null(fossil)){
    newd$fossil <- fossil
  }
  if(!is.null(Serv)){
    newd$Serv <- Serv
  }
  joint <- jc.probs(x=biv.mod, y1=thresholds[1], y2=thresholds[2], newdata= newd, type = "joint", intervals = FALSE)
  tau<- round(joint$tau, 3)
  thGINI<- round(joint$p1,3)
  thCarbon<- round(joint$p2,3)
  thGC<- round(joint$p12,3)
  text <- paste(country,"& ", newd$Year, "& ",newd$polity,"& ", round(newd$fossil,2), "& ", round(newd$Serv,2), sep="")
  copula.contour(biv.mod, newd, ls = 100, range.v1 = range.v1, range.v2 = range.v2, levels=levels, xlab= "GINI", ylab= "Carbon", 
                 main = text)
  legend("bottomright", legend=paste("Kendall's ", expression(tau), "=", tau, sep=""), bty = "n")
  return(list(tau=tau, main=text, thGINI= thGINI, thCarbon=thCarbon,thGC= thGC ))
}

(tau <- myplot())


m1<-myplot(year=1997, country="india")
m2<-myplot(year=2008, country="india")
m3<-myplot(year=2009, country="india")
m4<-myplot(year=2012, country="india")
m5<-myplot(year=2012, country="india", fossil=50)
m5a<-myplot(year=2012, country="india", fossil=10)
m5b<-myplot(year=2012, country="india", polity=10)
m5c<-myplot(year=2012, country="india", polity=3)
m5d<-myplot(year=2012, country="india", Serv=20)
m5e<-myplot(year=2012, country="india", Serv=20, fossil=10) 



m6<-myplot(year=1997, country="egypt")
m7<-myplot(year=2008, country="egypt")
m8<-myplot(year=2009, country="egypt")
m9<-myplot(year=2014, country="egypt")
m10<-myplot(year=2014, country="egypt", fossil=50)
m10a<-myplot(year=2014, country="egypt", fossil=10)
m10b<-myplot(year=2014, country="egypt", polity=10)
m10c<-myplot(year=2014, country="egypt", polity=3)
m10d<-myplot(year=2014, country="egypt", Serv=20)
m10e<-myplot(year=2014, country="egypt", Serv=20, fossil=10) 

m11<-myplot(year=1997, country="bangladesh")
m12<-myplot(year=2008, country="bangladesh")
m13<-myplot(year=2009, country="bangladesh")
m14<-myplot(year=2014, country="bangladesh")
m14a<-myplot(year=2014, country="bangladesh", fossil=50)
m14b<-myplot(year=2014, country="bangladesh", fossil=10)
m14c<-myplot(year=2014, country="bangladesh", polity=10)
m14d<-myplot(year=2014, country="bangladesh", polity=3)
m14e<-myplot(year=2014, country="bangladesh", Serv=20)
m14f<-myplot(year=2014, country="bangladesh", Serv=20, fossil=10) 

m15<-myplot(year=1997, country="tanzania")
m16<-myplot(year=2008, country="tanzania")
m17<-myplot(year=2009, country="tanzania")
m18<-myplot(year=2014, country="tanzania")
m18a<-myplot(year=2014, country="tanzania", fossil=80)
m18b<-myplot(year=2014, country="tanzania", fossil=50)
m18c<-myplot(year=2014, country="tanzania", polity=10)
m18d<-myplot(year=2014, country="tanzania", polity=3)
m18e<-myplot(year=2014, country="tanzania", Serv=20)
m18f<-myplot(year=2014, country="tanzania", Serv=20, fossil=10) 

m19<-myplot(year=1997, country="bolivia")
m20<-myplot(year=2008, country="bolivia")
m21<-myplot(year=2009, country="bolivia")
m23<-myplot(year=2014, country="bolivia")
m23a<-myplot(year=2014, country="bolivia", fossil=50)
m23b<-myplot(year=2014, country="bolivia", fossil=10)
m23c<-myplot(year=2014, country="bolivia", polity=10)
m23d<-myplot(year=2014, country="bolivia", polity=3)
m23e<-myplot(year=2014, country="bolivia", Serv=20)
m23f<-myplot(year=2014, country="bolivia", Serv=20, fossil=10) 


main<- rbind(m1$main, m2$main, m3$main, m4$main, m5$main,m5a$main, m5b$main, m5c$main,m5d$main, m5e$main, m6$main, m7$main, m8$main, m9$main, m10$main,
             m10a$main, m10b$main, m10c$main, m10d$main, m10e$main,
             m11$main, m12$main, m13$main, m14$main,m14a$main, m14b$main, m14c$main, m14d$main,  m14e$main,  m14f$main,
             m15$main, m16$main, m17$main, m18$main,m18a$main, m18b$main, m18c$main, m18d$main, m18e$main, m18f$main, m19$main, m20$main,
             m21$main, m23$main, m23a$main, m23b$main, m23c$main, m23d$main, m23e$main, m23f$main) 

ktau<- rbind(m1$tau, m2$tau, m3$tau, m4$tau, m5$tau,m5a$tau, m5b$tau, m5c$tau,m5d$tau, m5e$tau, m6$tau, m7$tau, m8$tau, m9$tau, m10$tau,
             m10a$tau, m10b$tau, m10c$tau, m10d$tau, m10e$tau, 
             m11$tau, m12$tau, m13$tau, m14$tau,m14a$tau, m14b$tau, m14c$tau, m14d$tau,  m14e$tau,  m14f$tau,
             m15$tau, m16$tau, m17$tau, m18$tau,m18a$tau, m18b$tau, m18c$tau, m18d$tau, m18e$tau, m18f$tau, m19$tau, m20$tau,
             m21$tau, m23$tau, m23a$tau, m23b$tau, m23c$tau, m23d$tau, m23e$tau, m23f$tau)


thGINI<- rbind(m1$thGINI, m2$thGINI, m3$thGINI, m4$thGINI, m5$thGINI,m5a$thGINI, m5b$thGINI, m5c$thGINI,m5d$thGINI, m5e$thGINI, m6$thGINI, m7$thGINI, m8$thGINI, m9$thGINI, m10$thGINI,
               m10a$thGINI, m10b$thGINI, m10c$thGINI, m10d$thGINI, m10e$thGINI,
               m11$thGINI, m12$thGINI, m13$thGINI, m14$thGINI,m14a$thGINI, m14b$thGINI, m14c$thGINI, m14d$thGINI,  m14e$thGINI,  m14f$thGINI,
               m15$thGINI, m16$thGINI, m17$thGINI, m18$thGINI,m18a$thGINI, m18b$thGINI, m18c$thGINI, m18d$thGINI, m18e$thGINI, m18f$thGINI, m19$thGINI, m20$thGINI,
               m21$thGINI, m23$thGINI, m23a$thGINI, m23b$thGINI, m23c$thGINI, m23d$thGINI, m23e$thGINI, m23f$thGINI)

thCarbon<- rbind(m1$thCarbon, m2$thCarbon, m3$thCarbon, m4$thCarbon, m5$thCarbon,m5a$thCarbon, m5b$thCarbon, m5c$thCarbon,m5d$thCarbon, m5e$thCarbon, m6$thCarbon, m7$thCarbon, m8$thCarbon, m9$thCarbon, m10$thCarbon,
                 m10a$thCarbon, m10b$thCarbon, m10c$thCarbon, m10d$thCarbon, m10e$thCarbon, 
                 m11$thCarbon, m12$thCarbon, m13$thCarbon, m14$thCarbon,m14a$thCarbon, m14b$thCarbon, m14c$thCarbon, m14d$thCarbon,  m14e$thCarbon,  m14f$thCarbon,
                 m15$thCarbon, m16$thCarbon, m17$thCarbon, m18$thCarbon,m18a$thCarbon, m18b$thCarbon, m18c$thCarbon, m18d$thCarbon, m18e$thCarbon, m18f$thCarbon, m19$thCarbon, m20$thCarbon,
                 m21$thCarbon, m23$thCarbon, m23a$thCarbon, m23b$thCarbon, m23c$thCarbon, m23d$thCarbon, m23e$thCarbon, m23f$thCarbon) 


thGC<- rbind(m1$thGC, m2$thGC, m3$thGC, m4$thGC, m5$thGC,m5a$thGC, m5b$thGC, m5c$thGC,m5d$thGC, m5e$thGC, m6$thGC, m7$thGC, m8$thGC, m9$thGC, m10$thGC,
             m10a$thGC, m10b$thGC, m10c$thGC, m10d$thGC, m10e$thGC, 
             m11$thGC, m12$thGC, m13$thGC, m14$thGC,m14a$thGC, m14b$thGC, m14c$thGC, m14d$thGC,  m14e$thGC,  m14f$thGC,
             m15$thGC, m16$thGC, m17$thGC, m18$thGC,m18a$thGC, m18b$thGC, m18c$thGC, m18d$thGC, m18e$thGC, m18f$thGC, m19$thGC, m20$thGC,
             m21$thGC, m23$thGC, m23a$thGC, m23b$thGC, m23c$thGC, m23d$thGC, m23e$thGC, m23f$thGC)


tab_country<- cbind(main, ktau, thGINI, thCarbon, thGC)
xtable(tab_country)



