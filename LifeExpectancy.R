install.packages("plyr")
install.packages("googleVis")
install.packages("maps")
install.packages("mapdata")
library('maps')
library('mapproj') 
library('rgeos')
library('maptools')
library('rworldmap')
library('RColorBrewer')
library('mapdata')
library('googleVis')
library('ggplot2')
library('reshape2')
library('sqldf')
library('rgdal')
options(repos = c(CRAN = "http://cran.rstudio.com"))
# Final Project IS608 Knowledge and Visual Analytics
#===============================================================================================================
#Loading Data and setting working Directory
#setwd("C:\\Users\\dundeva\\Desktop\\SPS\\IS608\\CUNY_IS608-master\\FinalProject")
setwd("C:\\Users\\dunde\\Documents\\SCRIPTING FILES\\R")
# loading Health Care expenditure data
#hcare <- read.csv('../FinalProject/HealthcareExpenditurecleaned.csv',header=TRUE,sep = ',',check.names=FALSE)
hcare <- read.csv('../R/healthcareExp.csv')
summary(hcare)


# loading life expectancy data
#lifexpct1 <- read.csv('../FinalProject/LifeExpectancycleaned.csv',header=TRUE,sep = ',',check.names=FALSE)
lifexpct1<- read.csv('../R/lifeExpec1.csv')
summary(lifexpct1)


# loading Regions 
#cregion <- read.csv('../FinalProject/Region.csv',header=TRUE,sep = ',',check.names=FALSE)
cregion<- read.csv('../R/lifeExpec2.csv')
#cregion<-lifeExpec2
summary(cregion)

#loading Shape File
library('rgdal')
setwd("C:\\Users\\dunde\\Documents\\SCRIPTING FILES\\R")
#dsn <- "ne_110m_admin_0_countries.shp"
#cb5 = readOGR(dsn)
#np_dist2<- readOGR("../SCRIPTING FILES/R/ShapeFile/ne_110m_admin_0_countries.shp")
library(raster)
np_dist2 <- shapefile("~/SCRIPTING FILES/R/ShapeFile/ne_110m_admin_0_countries.shp")
#===============================================================================================================
#===============================================================================================================
## Transforming data from Wide to long for Cost
# assigning new names to the columns of the data frame

hc<-reshape(hcare,varying=c("X2000"
                            ,"X2001","X2002","X2003","X2004","X2005","X2006"
                            ,"X2007","X2008","X2009","X2010","X2011","X2012","X2013","X2014",
                            "X2015","X2016","X2017","X2018"),
            v.names="cost",
            timevar="Year",
            times=c("2000"
                    ,"2001","2002","2003","2004","2005","2006"
                    ,"2007","2008","2009","2010","2011","2012","2013","2014",
                    "2015","2016","2017","2018"),
            direction="long"
)
hc
colnames(hc)
colnames(hc) <- c('CountryName','CountryCode','X2019','X2020','Year','cost','id')
#===============================================================================================================
## Transforming data from Wide to long for length of life

life<-reshape(lifexpct1,varying=c("X1995","X1996","X1997","X1998","X1999","X2000","X2001","X2002","X2003","X2004","X2005"
                                  ,"X2006","X2007","X2008","X2009","X2010","X2011","X2012"),
              v.names="Age",
              timevar="Year",
              times=c("1995","1996","1997","1998","1999","2000"
                      ,"2001","2002","2003","2004","2005","2006"
                      ,"2007","2008","2009","2010","2011","2012"),
              direction="long"
)
life
#colnames(life)
#===============================================================================================================
#merging lie expectancy and cost data sets

hl <- sqldf("SELECT hc.[CountryName],
                    hc.[CountryCode], 
                    hc.Year , 
                    hc.cost, 
                    life.Age 
       FROM hc LEFT JOIN life where hc.[CountryName]= life.[ï..CountryName] 
             and hc.[CountryCode]=life.[CountryCode] 
             and hc.Year=life.Year ")

#Adding Regions to data set
hl2<-sqldf("Select hl.[CountryName],
hl.[CountryCode], 
hl.Year , 
hl.cost, 
hl.Age,
cregion.Region
           from hl left join cregion where hl.[CountryCode]=cregion.[ï..CountryCode]
           and hl.[CountryName]=cregion.[CountryName]
           and cregion.Region !=''")

#impute avg cost and age for NA entries
hl2$cost[is.na(hl2$cost)] <- mean(hl2$cost, na.rm = TRUE)
hl2$Age[is.na(hl2$Age)] <- mean(hl2$Age, na.rm = TRUE)

colnames(cregion)
head(hl2)
summary(hl2)
str(hl2)



#Save Cleaned data
write.csv(hl2, 
          file='C:/Users/dunde/Documents/SCRIPTING FILES/R/mergcleaned.csv',row.names=FALSE)
#===============================================================================================================
#===============================================================================================================

install.packages('rworldmap',dependencies=TRUE) 
library(rworldmap)
#plotting static heat maps for Expenditure and Life Expectancy
sPDF <- joinCountryData2Map(hl2,joinCode="NAME" , nameJoinColumn ="CountryName")
par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
mapCountryData( sPDF,mapTitle='World Health Expenditure', nameColumnToPlot="cost")

sPDF <- joinCountryData2Map(hl2,joinCode="NAME" , nameJoinColumn ="CountryName")
par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
mapCountryData( sPDF,mapTitle='World Life Expectancy ', nameColumnToPlot="Age")

#===============================================================================================================
library(googleVis)
#Creating Geo Charts with googlevis package
GeoChart <- gvisGeoChart(hl2, "CountryName", "cost","Age",
                         options=list(width=610, height=400,
                                      colorAxis="{colors:['#99FF66','#2EB82E','#006600']}",
                                      backgroundColor="#F0F0F0"))
# Display chart
plot(GeoChart)
#===============================================================================================================
D <- transform(hl2, Year = as.numeric(Year))

# plotting a subset of the data to compare spending against Age
(hsb7 <- D[D$CountryCode %in% c('BRA','CHN', 'IND', 'RUS', 'USA','GBR','CAN','AUS'), ])
G <- gvisGeoChart(hsb7, "CountryName", "cost","Age", options=list(width=610, height=400,
                                                                  colorAxis="{colors:['#99FF66','#2EB82E','#006600']}",
                                                                  backgroundColor="#F0F0F0"),chartid="c1")
T <- gvisTable(hsb7, options=list(width=610, height=270), chartid="c2")
GT <- gvisMerge(G,T, horizontal=FALSE,chartid="gt") 
M <- gvisMotionChart(hsb7, "CountryCode", "Year",
                     options=list(width=610, height=670),chartid="c3")

GTM <- gvisMerge(GT, M, horizontal=TRUE,
                 tableOptions="bgcolor=\"#CCCCCC\" cellspacing=10",chartid="gtm")

# Display chart

plot(GTM)
#===============================================================================================================
## Bar and Line chart
Combo <- gvisComboChart(hsb7, xvar="CountryName",
                        yvar=c("cost","Age"),
                        options=list(seriesType="bars",
                                     series='{1: {type:"line"}}'))
plot(Combo)
#==============================================================================================================


