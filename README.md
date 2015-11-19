setwd("d://Users//goyaan//Desktop//R trials/Permian")
library(plyr)
library(ggplot2)

## Read Eagle Ford attributes and production data into memory
attributes<-read.csv("./PermianAttributes1.csv",stringsAsFactors = FALSE,colClasses = "character",na.strings = "")
production<-read.csv("./PermianProduction1.csv",stringsAsFactors = FALSE,colClasses = "character",na.strings = "")

#clean up column headers
{names(attributes)<-tolower(names(attributes))
names(attributes)<-gsub("\\.","",names(attributes))
names(production)<-tolower(names(production))
names(production)<-gsub("\\.","",names(production))}

#convert ipdate and production month to standard date formats for comparisons
attributes$ipdate<-as.Date(attributes$ipdate,"%e/%m/%Y")
production$month<-as.Date(production$month,"%m/%e/%Y")
attributes$api<-as.numeric(attributes$api)
production$api<-as.numeric(production$api)
attributes<-attributes[!is.na(attributes$ipdate),]

#merge acts amost like vlookup. looks up attributes for IP date and pastes that along respective API
production=merge(production,attributes[,c("api","ipdate")])

# Convert oil/gas/condensate production to numeric and replace NA with 0
{production$oilbbl<-as.numeric(production$oilbbl)
production$oilbbl[is.na(production$oilbbl)]<-0
production$gasmcf<-as.numeric(production$gasmcf)
production$gasmcf[is.na(production$gasmcf)]<-0
production$condensatebbl<-as.numeric(production$condensatebbl)
production$condensatebbl[is.na(production$condensatebbl)]<-0
production$waterbbl<-as.numeric(production$waterbbl)
production$waterbbl[is.na(production$waterbbl)]<-0}

#Convert well metric to numeric for plotting
{attributes$laterallengthft<-as.numeric(attributes$laterallengthft)
attributes$proppantlbs<-as.numeric(attributes$proppantlbs)
attributes$propperfoot<-attributes$proppantlbs/attributes$laterallengthft}

#initialise 180 day cumulative production for oil, gas and condensate with 0 before calculating actuals
{production$sixmonthcumoil=0
production$sixmonthcumgas=0
production$sixmonthcumcondensate=0
production$sixmonthcumwater=0}

#For all rows in production table check if the IP date and production month are equal
#Then, if 6 consecutive APIs are equal, sum them up for cumulative production

#To get cumulative oil production
for (i in 1:(nrow(production)-5)) {
  if (production[i,1]==production[i+1,1] && production[i,1]==production[i+2,1] &&
      production[i,1]==production[i+3,1] && production[i,1]==production[i+4,1] &&
      production[i,1]==production[i+5,1] && production$month[i]==production$ipdate[i]) {
      production$sixmonthcumoil[i] = production$oilbbl[i]+production$oilbbl[i+1]+production$oilbbl[i+2]+
                                     production$oilbbl[i+3]+production$oilbbl[i+4]+production$oilbbl[i+5]
      }
}

#To get cumulative gas production
for (i in 1:(nrow(production)-5)) {
  if (production[i,1]==production[i+1,1] && production[i,1]==production[i+2,1] && 
      production[i,1]==production[i+3,1] && production[i,1]==production[i+4,1] &&
      production[i,1]==production[i+5,1] && production$month[i]==production$ipdate[i]) {
      production$sixmonthcumgas[i] = production$gasmcf[i]+production$gasmcf[i+1]+production$gasmcf[i+2]+
                                     production$gasmcf[i+3]+production$gasmcf[i+4]+production$gasmcf[i+5]
  }
}

#To calculate condensate production
for (i in 1:(nrow(production)-5)) {
  if (production[i,1]==production[i+1,1] && production[i,1]==production[i+2,1] && 
      production[i,1]==production[i+3,1] && production[i,1]==production[i+4,1] &&
      production[i,1]==production[i+5,1] && production$month[i]==production$ipdate[i]) {
      production$sixmonthcumcondensate[i] = production$condensatebbl[i]+production$condensatebbl[i+1]+production$condensatebbl[i+2]+
                                            production$condensatebbl[i+3]+production$condensatebbl[i+4]+production$condensatebbl[i+5]
  }
}

#To calculate water production
for (i in 1:(nrow(production)-5)) {
  if (production[i,1]==production[i+1,1] && production[i,1]==production[i+2,1] && 
      production[i,1]==production[i+3,1] && production[i,1]==production[i+4,1] &&
      production[i,1]==production[i+5,1] && production$month[i]==production$ipdate[i]) {
      production$sixmonthcumwater[i] = production$waterbbl[i]+production$waterbbl[i+1]+production$waterbbl[i+2]+
                                       production$waterbbl[i+3]+production$waterbbl[i+4]+production$waterbbl[i+5]
  }
}

#Subset out cumulative oil production values is 0 or NA
#If major product is gas, then replace with gas production
productioncum<-production[production$sixmonthcumgas>0,]
productioncum<-productioncum[!is.na(productioncum$sixmonthcumgas),]

#Convert sub-play and operator to factor for plotting
attributes$subplay<-factor(attributes$subplay)
attributes$operator<-factor(attributes$operator)

#Create attributes table with cumulative production
attributescum=merge(attributes,productioncum[,c("api","sixmonthcumoil","sixmonthcumgas","sixmonthcumcondensate","sixmonthcumwater")])

#Convert all dates to years and store as factor for plotting
{attributescum$ipyear<-factor(as.numeric(format(as.Date(attributescum$ipdate,"%e/%m/%Y"),"%Y")))
attributescum$permityear<-factor(as.numeric(format(as.Date(attributescum$permitdate,"%e/%m/%Y"),"%Y")))
attributescum$drillingstartyear<-factor(as.numeric(format(as.Date(attributescum$drillingstartdate,"%e/%m/%Y"),"%Y")))
attributescum$drillingendyear<-factor(as.numeric(format(as.Date(attributescum$drillingenddate,"%e/%m/%Y"),"%Y")))
attributescum$completionyear<-factor(as.numeric(format(as.Date(attributescum$completiondate,"%e/%m/%Y"),"%Y")))
attributescum$firstproductionyear<-factor(as.numeric(format(as.Date(attributescum$firstproductiondate,"%e/%m/%Y"),"%Y")))}

#Convert all cumulative volumes to a per foot basis
{attributescum$cumoilperfoot<-attributescum$sixmonthcumoil/attributescum$laterallengthft
attributescum$cumgasperfoot<-attributescum$sixmonthcumgas/attributescum$laterallengthft
attributescum$cumcondensateperfoot<-attributescum$sixmonthcumcondensate/attributescum$laterallengthft
attributescum$cumwaterperfoot<-attributescum$sixmonthcumwater/attributescum$laterallengthft}

attributescum=attributescum[!is.na(attributescum$cumgasperfoot),]
#attributescum<-attributescum[attributescum$cumoilperfoot<30,]
  
#Subset the attributescum table for the companies to be analysed 
{companysubset<-c("EOG Resources",
                 "ConocoPhillips",
                 "Marathon Oil",
                 "Murphy Oil",
                 "Devon Energy",
                 "Encana Corporation",
                 "Chesapeake Energy",
                 "EP Energy",
                 "BHP Billiton")

attributescumplot<-subset(attributescum,attributescum$operator %in% companysubset)}

#Subset the attributescum table for the sub-plays to be analysed 
{subplaysubset<-c("Lean Gas Core",
                  "Lean Gas",
                  "Southern Wet Gas",
                  "Greene Dry gas Area",
                  "Southwest Rich Gas",
                  "Rich gas Core",
                  "WV Rich Gas")

attributescumplot<-subset(attributescum,attributescum$subplay %in% subplaysubset)}

a<-attributescumplot[,c("subplay","completionyear","sixmonthcumgas","ipgasmmcfd","operator")]

#To add mean and median of a metric on plot
{northeastgasmean<-ddply(a,c("subplay","completionyear"),summarize,mean=mean(sixmonthcumgas))
northeastgasmedian<-ddply(a,c("subplay","completionyear"),summarize,median=median(sixmonthcumgas))}

# to plot cumulative oil production per foot by sub-play and ip year + mean and median lines
{h<-ggplot(a,aes(sixmonthcumgas))

(h+geom_histogram(binwidth=100000) + facet_grid(.~subplay)
  +facet_grid(.~subplay)+geom_vline(data=northeastgasmean,aes(xintercept=mean),na.rm = TRUE,color="red")
  +facet_grid(.~subplay)+geom_vline(data=northeastgasmedian,aes(xintercept=median),na.rm = TRUE,color="blue")
)}

# To plot cumulative production vs proppant per foot
#attributescumplot<-attributescumplot[!is.na(attributescumplot$propperfoot),]
ip<-ggplot(a,aes(propperfoot,sixmonthcumoil))
(ip+geom_point()+facet_grid(completionyear~subplay)
  +geom_smooth(method="lm",se=FALSE)
  +geom_vline(aes(xintercept=2000),linetype="dashed")
  +labs(xlab("Proppant per lateral foot (lbs/ft)"))
  +labs(ylab("180 day cumulative oil production (bbl)")))
