remove(list = ls())

library(googleVis)
library(ggplot2)
library(dygraphs)
library(rworldmap)

Regions <- read.csv("~/WorkingDirectory/Data/Regions_Attacks.csv")
Continents <- read.csv("~/WorkingDirectory/Data/Continents_Attacks.csv")

NorthAmerica <- read.csv("~/WorkingDirectory/Data/North America_Attacks.csv")
SouthAmerica <- read.csv("~/WorkingDirectory/Data/South America_Attacks.csv")
CentralAmerica <- read.csv("~/WorkingDirectory/Data/Central America and Caribbean_Attacks.csv")

EastAsia <- read.csv("~/WorkingDirectory/Data/East Asia_Attacks.csv")
SoutheastAsia <- read.csv("~/WorkingDirectory/Data/Southeast Asia_Attacks.csv")
CentralAsia <- read.csv("~/WorkingDirectory/Data/Central Asia_Attacks.csv")
SouthAsia <- read.csv("~/WorkingDirectory/Data/South Asia_Attacks.csv")

Australasia <- read.csv("~/WorkingDirectory/Data/Australasia and Oceania_Attacks.csv")

WesternEurope <- read.csv("~/WorkingDirectory/Data/Western Europe_Attacks.csv")
EasternEurope <- read.csv("~/WorkingDirectory/Data/Eastern Europe_Attacks.csv")

SubSaharanAfrica <- read.csv("~/WorkingDirectory/Data/Sub-Saharan Africa_Attacks.csv")
MiddleEastNorthAfrica <- read.csv("~/WorkingDirectory/Data/Middle East and North Africa_Attacks.csv")

World <- read.csv("~/WorkingDirectory/Data/World.csv")

Tree <- read.csv("~/WorkingDirectory/Data/Tree.csv")

Year <- read.csv("~/WorkingDirectory/Data/Year_Attacks.csv")

kill <- read.csv("~/WorkingDirectory/Data/kill.csv")

bubble <- read.csv("~/WorkingDirectory/Data/GoogleChart_Final.csv")

terror <- read.csv("~/WorkingDirectory/Data/terror.csv")

##################################################################################################

# Tree Structure

Org <- gvisOrgChart(Tree, idvar = "Region", parentvar = "Parent",
                    tipvar="Attacks",
                    options=list(width=600, height=250, 
                                 size='large', allowCollapse=TRUE))
plot(Org)
cat(Org$html$chart)

###################################################################################################

# Histograms

# Continents
Continents$Mean=mean(Continents$Attacks)
CC <- gvisComboChart(Continents, xvar='Continent',
                     yvar=c('Mean', 'Attacks'),
                     options=list(seriesType='bars',
                                  width=450, height=300,
                                  title='Continent Attacks',
                                  series='{0: {type:\"line\"}}'))

# Regions
Regions$Mean=mean(Regions$Attacks)
RR <- gvisComboChart(Regions, xvar='Region',
                     yvar=c('Mean', 'Attacks'),
                     options=list(seriesType='bars',
                                  width=450, height=300,
                                  title='Region Attacks',
                                  series='{0: {type:\"line\"}}'))


M1 <- gvisMerge(CC,RR, horizontal=TRUE)
plot(M1)

# Asia

EastAsia$Mean=mean(EastAsia$Attacks)
H1 <- gvisComboChart(EastAsia, xvar='Country',
                     yvar=c('Mean', 'Attacks'),
                     options=list(seriesType='bars',
                                  width=450, height=300,
                                  title='East Asia Attacks',
                                  series='{0: {type:\"line\"}}'))

SouthAsia$Mean=mean(SouthAsia$Attacks)
H2 <- gvisComboChart(SouthAsia, xvar='Country',
                     yvar=c('Mean', 'Attacks'),
                     options=list(seriesType='bars',
                                  width=450, height=300,
                                  title='South Asia Attacks',
                                  series='{0: {type:\"line\"}}'))

SoutheastAsia$Mean=mean(SoutheastAsia$Attacks)
H3 <- gvisComboChart(SoutheastAsia, xvar='Country',
                     yvar=c('Mean', 'Attacks'),
                     options=list(seriesType='bars',
                                  width=450, height=300,
                                  title='Southeast Asia Attacks',
                                  series='{0: {type:\"line\"}}'))

CentralAsia$Mean=mean(CentralAsia$Attacks)
H4 <- gvisComboChart(CentralAsia, xvar='Country',
                     yvar=c('Mean', 'Attacks'),
                     options=list(seriesType='bars',
                                  width=450, height=300,
                                  title='Central Asia Attacks',
                                  series='{0: {type:\"line\"}}'))

M2 <- gvisMerge(H1,H2, horizontal = TRUE)
M3 <- gvisMerge(H3,H4, horizontal = TRUE)
M4 <- gvisMerge(M2,M3, horizontal = FALSE)

plot(M4)

# Africa

MiddleEastNorthAfrica$Mean=mean(MiddleEastNorthAfrica$Attacks)
H5 <- gvisComboChart(MiddleEastNorthAfrica, xvar='Country',
                     yvar=c('Mean', 'Attacks'),
                     options=list(seriesType='bars',
                                  width=450, height=300,
                                  title='MiddleEast, North Africa Attacks',
                                  series='{0: {type:\"line\"}}'))



SubSaharanAfrica$Mean=mean(SubSaharanAfrica$Attacks)
H6 <- gvisComboChart(SubSaharanAfrica, xvar='Country',
                     yvar=c('Mean', 'Attacks'),
                     options=list(seriesType='bars',
                                  width=450, height=300,
                                  title='SubSaharan Africa Attacks',
                                  series='{0: {type:\"line\"}}'))


M5 <- gvisMerge(H5,H6, horizontal = TRUE)

plot(M5)

# North America

NorthAmerica$Mean=mean(NorthAmerica$Attacks)
H7 <- gvisComboChart(NorthAmerica, xvar='Country',
                     yvar=c('Mean', 'Attacks'),
                     options=list(seriesType='bars',
                                  width=450, height=300,
                                  title='North America Attacks',
                                  series='{0: {type:\"line\"}}'))


CentralAmerica$Mean=mean(CentralAmerica$Attacks)
H8 <- gvisComboChart(CentralAmerica, xvar='Country',
                     yvar=c('Mean', 'Attacks'),
                     options=list(seriesType='bars',
                                  width=450, height=300,
                                  title='Central AmericaAttacks',
                                  series='{0: {type:\"line\"}}'))

M6 <- gvisMerge(H7,H8, horizontal = TRUE)
plot(M6)

# South America

SouthAmerica$Mean=mean(SouthAmerica$Attacks)
H9 <- gvisComboChart(SouthAmerica, xvar='Country',
                     yvar=c('Mean', 'Attacks'),
                     options=list(seriesType='bars',
                                  width=450, height=300,
                                  title='South America Attacks',
                                  series='{0: {type:\"line\"}}'))
plot(H9)

# Europe

WesternEurope$Mean=mean(WesternEurope$Attacks)
H10 <- gvisComboChart(WesternEurope, xvar='Country',
                     yvar=c('Mean', 'Attacks'),
                     options=list(seriesType='bars',
                                  width=450, height=300,
                                  title='Western Europe Attacks',
                                  series='{0: {type:\"line\"}}'))


EasternEurope$Mean=mean(EasternEurope$Attacks)
H11 <- gvisComboChart(EasternEurope, xvar='Country',
                     yvar=c('Mean', 'Attacks'),
                     options=list(seriesType='bars',
                                  width=450, height=300,
                                  title='Eastern Europe Attacks',
                                  series='{0: {type:\"line\"}}'))

M7 <- gvisMerge(H10,H11, horizontal = TRUE)
plot(M7)


# Australia

Australasia$Mean=mean(Australasia$Attacks)
H12 <- gvisComboChart(Australasia, xvar='Country',
                     yvar=c('Mean', 'Attacks'),
                     options=list(seriesType='bars',
                                  width=450, height=300,
                                  title='Australasia Attacks',
                                  series='{0: {type:\"line\"}}'))

plot(H12)

cat(M1$html$chart, file = "hist1.html") #CC and RR
cat(M4$html$chart, file = "hist2.html") #Asia
cat(M5$html$chart, file = "hist3.html") #Africa
cat(M6$html$chart, file = "hist4.html") #N America
cat(M7$html$chart, file = "hist5.html") #S America
cat(H9$html$chart, file = "hist6.html") #Europe
cat(H12$html$chart, file = "hist7.html")#Australia

#####################################################################################################

# Year Wise Attacks

Year$Mean=mean(Year$Attacks)

H13 <- gvisComboChart(Year, xvar='Year',
                      yvar=c('Mean', 'Attacks'),
                      options=list(seriesType='bars',
                                   width= 1000, height=500,
                                   title='Attacks all over the world in 45 Years',
                                   series='{0: {type:\"line\"}}'))

plot(H13)

plot(Year$Year,Year$Attacks, type ="l", col ="red", lwd = 3, xlab = "Year", ylab ="Number ofAttacks", main ="Attacks all over the world in 45 years")

cat(H13$html$chart, file="year.html")

#######################################################################################################

# World plot

World$Mean=mean(World$Attacks)
H14 <- gvisComboChart(World, xvar='Country',
                      yvar=c('Mean', 'Attacks'),
                      options=list(seriesType='bars',
                                   width=1000, height=300,
                                   title='Attacks all over the world in 45 Years',
                                   series='{0: {type:\"line\"}}'))

plot(H14)
cat(H14$html$chart, file="world.html")

#####################################################################################################

# World map - All attack points

newmap <- getMap(resolution = "low")
plot(newmap, xlim= c(-300,300), ylim = c(35,71), asp = 1)
points(ModifiedVersion$longitude,ModifiedVersion$latitude, col="red", cex= .6)

#######################################################################################################

# Intensity Map - World

Geo <- gvisGeoChart(World, locationvar='Country', colorvar='Attacks')
#options=list(colors="['#109dc0']")
Tbl <- gvisTable(World, options=list(height=300, width=200))
I1 <- gvisMerge(Geo, Tbl, horizontal=TRUE)
plot(I1)


WorldGeo <- gvisGeoMap(World, "Country", "Attacks",
                 options=list(dataMode="regions", width=600, height=300))
plot(WorldGeo)

# America

Geo <- gvisGeoChart(NorthAmerica, locationvar='Country', colorvar='Attacks',
                    options=list(height=300, width=350, region ="021"))
Tbl <- gvisTable(NorthAmerica, options=list(height=300, width=200))
I2 <-gvisMerge(Geo, Tbl, horizontal=TRUE)

Geo <- gvisGeoChart(SouthAmerica, locationvar='Country', colorvar='Attacks',
                    options=list(height=300, width=350, region="005"))
Tbl <- gvisTable(SouthAmerica, options=list(height=300, width=200))
I3 <- gvisMerge(Geo, Tbl, horizontal=TRUE)

Geo <- gvisGeoChart(CentralAmerica, locationvar='Country', colorvar='Attacks',
                    options=list(height=300, width=350, region="019"))
Tbl <- gvisTable(CentralAmerica, options=list(height=300, width=200))
I4 <- gvisMerge(Geo, Tbl, horizontal=TRUE)

I30 <- gvisMerge(I2, I3, horizontal=TRUE,tableOptions="bgcolor=\"#AABBCC\"")
I30 <- gvisMerge(I30, I4, tableOptions="bgcolor=\"#AABBCC\"")
plot(I30)

# Asia

Geo <- gvisGeoChart(SouthAsia, locationvar='Country', colorvar='Attacks',
                    options=list(region="034",backgroundColor="lightblue"))
Tbl <- gvisTable(SouthAsia, options=list(height=300, width=200))
I5 <- gvisMerge(Geo, Tbl, horizontal=TRUE)

Geo <- gvisGeoChart(CentralAsia, locationvar='Country', colorvar='Attacks',
                    options=list(region="143"))
Tbl <- gvisTable(CentralAsia, options=list(height=300, width=200))
I7 <- gvisMerge(Geo, Tbl, horizontal=TRUE)

Geo <- gvisGeoChart(EastAsia, locationvar='Country', colorvar='Attacks',
                    options=list(region="030"))
Tbl <- gvisTable(EastAsia, options=list(height=300, width=200))
I8 <- gvisMerge(Geo, Tbl, horizontal=TRUE)

Geo <- gvisGeoChart(SoutheastAsia, locationvar='Country', colorvar='Attacks',
                    options=list(region="035"))
Tbl <- gvisTable(SoutheastAsia, options=list(height=300, width=200))
I9 <- gvisMerge(Geo, Tbl, horizontal=TRUE)
plot(I9)

I31 <- gvisMerge(gvisMerge(I5,I7),gvisMerge(I8,I9),
                 horizontal=TRUE, tableOptions="bgcolor=\"#AABBCC\"")
plot(I31)

# Europe

Geo <- gvisGeoChart(EasternEurope, locationvar='Country', colorvar='Attacks',
                    options=list(region="150"))
Tbl <- gvisTable(EasternEurope, options=list(height=300, width=200))
I10 <- gvisMerge(Geo, Tbl, horizontal=TRUE)

Geo <- gvisGeoChart(WesternEurope, locationvar='Country', colorvar='Attacks',
                    options=list(region="150"))
Tbl <- gvisTable(WesternEurope, options=list(height=300, width=200))
I11<- gvisMerge(Geo, Tbl, horizontal=TRUE)

I32 <- gvisMerge(I10,I11,
                 horizontal=TRUE, tableOptions="bgcolor=\"#AABBCC\"")
plot(I32)

# Africa

Geo <- gvisGeoChart(MiddleEastNorthAfrica, locationvar='Country', colorvar='Attacks',
                    options=list(region="002"))
Tbl <- gvisTable(MiddleEastNorthAfrica, options=list(height=300, width=200))
I12 <- gvisMerge(Geo, Tbl, horizontal=TRUE)

Geo <- gvisGeoChart(SubSaharanAfrica, locationvar='Country', colorvar='Attacks',
                    options=list(region="002"))
Tbl <- gvisTable(SubSaharanAfrica, options=list(height=300, width=200))
I13 <- gvisMerge(Geo, Tbl, horizontal=TRUE)

I33 <- gvisMerge(I12,I13,
                 horizontal=TRUE, tableOptions="bgcolor=\"#AABBCC\"")
plot(I33)

# Australia

Geo <- gvisGeoChart(Australasia, locationvar='Country', colorvar='Attacks',
                    options=list(region="009"))
Tbl <- gvisTable(Australasia, options=list(height=300, width=200))
I14 <- gvisMerge(Geo, Tbl, horizontal=TRUE)
plot(I14)


cat(I1$html$chart, file="chart.html") #World
cat(I30$html$chart, file= "chart1.html") #America
cat(I31$html$chart, file= "chart2.html") #Asia
cat(I32$html$chart, file= "chart3.html") #Europe
cat(I33$html$chart, file= "chart4.html") #Africa
cat(I14$html$chart, file= "chart5.html") #Australia


#####################################################################################################

# Bubble Motion Chart

M <- gvisMotionChart(bubble, "region", "year")
tbl <- gvisTable(bubble, options=list(height=220))
Mtbl <- gvisMerge(M, tbl,horizontal=TRUE, tableOptions="bgcolor=\"#AABBCC\"")
plot(Mtbl)

cat(Mtbl$html$chart, file="bubble.html") #Bubble with table

#####################################################################################################

# Pie Chart

slices<- c(234,126,1369,802,17,57,19,58)
colors<-c("violetred","violetred1","steelblue1","springgreen1","slateblue4","seagreen2","red3","yellow1")
lbls<- round(slices/sum(slices)*100,1)
lbls<-paste(lbls,"%",sep="")
pie(slices,labels=lbls,col=colors,main='Terrorist Attacks in USA since 1970')
legend("center",c("Armed Assault","Assasination","Bombonig/Explosion","Facility/Infrastructure Attack","Hijacking","Hostage Taking-Barricade Incident","Hostage Taking-Kidnapping","Unarmed Assault"),fill=colors)

#####################################################################################################

# Bubble Graph

bubble1 <- gvisBubbleChart(bubble, idvar="region", xvar="nattacks", yvar="ndeaths",
                           colorvar="year", sizevar="group",
                           options=list(hAxis='{minValue:75, maxValue:125}'))
plot(bubble1)

cat(bubble1$html$chart, file="bubble1.html")

#####################################################################################################

# Dygraph for year 

dygraph(Year, main = "Attacks by year", ylab = "attacks")

#####################################################################################################

# Tables

tbl <- gvisTable(Regions)
plot(tbl)

#####################################################################################################

# Calendar


`5years` <- read.csv("~/WorkingDirectory/Data/5years.csv")

`5years`$Dating <- as.Date(`5years`$Date, "%Y-%m-%d")


#as.POSIXct(dates, format = "%Y-%m-%d")


cl4 <- gvisCalendar(`5years`, datevar = "Dating", numvar="nwound",
                    options=list(colors="['#cbb69d','#603913']",
                                 title="Year Wise Kills",
                                 height=1000,
                                 calendar="{yearLabel: { fontName: 'Times-Roman',
                                 fontSize: 32, color: '#1A8763', bold: true},
                                 cellSize: 10,
                                 cellColor: { stroke: 'grey', strokeOpacity: 0.5 },
                                 focusedCellColor: {stroke:'black'}}")
                    )


plot(cl4)

cat(cl4$html$chart, file="cal.html")

#####################################################################################################



