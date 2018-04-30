# Load necessary libraries
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(fuzzyjoin))
suppressPackageStartupMessages(library(ggplot2))

# Load file
destfile<-"stormdata.csv.bz2"
fileURL<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
if(!file.exists(destfile)){
        download.file(fileURL,destfile,method=auto)
}

stormdata<-read.csv("stormdata.csv.bz2")
head(stormdata,5)

# Event categories
eventcat<-c("Astronomical Low Tide","Avalanche","Blizzard","Coastal Flood","Cold/Wind Chill","Debris Flow","Dense Fog","Dense Smoke","Drought","Dust Devil","Dust Storm","Excessive Heat","Extreme Cold/Wind Chill","Flash Flood","Flood","Frost/Freeze","Funnel Cloud","Freezing Fog","Hail","Heat","Heavy Rain","Heavy Snow","High Surf","High Wind","Hurricane (Typhoon)","Ice Storm","Lake-Effect Snow","Lakeshore Flood","Lightning","Marine Hail","Marine High Wind","Marine Strong Wind","Marine Thunderstorm Wind","Rip Current","Seiche","Sleet","Storm Surge/Tide","Strong Wind","Thunderstorm Wind","Tornado","Tropical Depression","Tropical Storm","Tsunami","Volcanic Ash","Waterspout","Wildfire","Winter Storm","Winter Weather")
eventcat<-as_tibble(eventcat)
names(eventcat)<-"EVTYPE"

# Subset
eventtype<-stormdata %>% select(EVTYPE,FATALITIES,INJURIES,PROPDMG,PROPDMGEXP,CROPDMG,CROPDMGEXP) %>% transform(EVTYPE=tolower(EVTYPE)) %>% arrange(EVTYPE)
eventtype.table<-as_tibble(eventtype)
eventtype<-eventtype.table %>% stringdist_left_join(eventcat,method="lcs",ignore_case=TRUE,max_dist=1)

# Convert damage
multiplier<-data.frame(exp=c("K","M","B"),amount=c(1000,1000000,1000000000))
eventtype$PROPDMG<-multiplier$amount[match(eventtype$PROPDMGEXP,multiplier$exp)]*eventtype[4]
eventtype$CROPDMG<-multiplier$amount[match(eventtype$CROPDMGEXP,multiplier$exp)]*eventtype[6]

eventtype<-select(eventtype, -ends_with("EXP"))

names(eventtype)<-c("EVTYPE","FATALITIES","INJURIES","PROPDMG","CROPDMG","EVCAT")

eventtype[,"n"]<-1
eventtype$n[is.na(eventtype$EVCAT)]<-0

#Summarize
eventtype<-eventtype %>% group_by(EVCAT,EVTYPE) %>% summarize(FATALITIES=sum(FATALITIES),INJURIES=sum(INJURIES),health=sum(FATALITIES,INJURIES,na.rm=TRUE),PROPDMG=sum(PROPDMG,na.rm=TRUE),CROPDMG=sum(CROPDMG,na.rm=TRUE),n=sum(n)) %>% arrange(desc(health))

#Remove unnecessary data frames
rm(eventtype.table,eventcat)

#Categorize events
#adjust avalanche
avalanche<-unique(eventtype$EVTYPE[grepl("avalan",eventtype$EVTYPE, ignore.case=TRUE)])
eventtype$EVCAT[which(eventtype$EVTYPE %in% avalanche & eventtype$n==0)]<-"Avalanche"
eventtype$n[which(eventtype$EVTYPE %in% avalanche & eventtype$n==0)]<-1
rm(avalanche)

#adjust astronomical low tide, add blow-out tide
low.tide<-unique(eventtype$EVTYPE[grepl("low tide",eventtype$EVTYPE, ignore.case=TRUE)|grepl("blow-out",eventtype$EVTYPE, ignore.case=TRUE)])
eventtype$EVCAT[which(eventtype$EVTYPE %in% low.tide & eventtype$n==0)]<-"Astronomical Low Tide"
eventtype$n[which(eventtype$EVTYPE %in% low.tide & eventtype$n==0)]<-1
rm(low.tide)

#adjust blizzard
blizzard<-unique(eventtype$EVTYPE[grepl("*blizzard*",eventtype$EVTYPE, ignore.case=TRUE)])
eventtype$EVCAT[which(eventtype$EVTYPE %in% blizzard & eventtype$n==0)]<-"Blizzard"
eventtype$n[which(eventtype$EVTYPE %in% blizzard & eventtype$n==0)]<-1
rm(blizzard)

#adjust coastal flooding
coastal.flood<-unique(eventtype$EVTYPE[grepl("coastal",eventtype$EVTYPE, ignore.case=TRUE)|grepl("beach",eventtype$EVTYPE, ignore.case=TRUE)])
coastal.storm<-coastal.flood[grepl("storm",coastal.flood)]
coastal.flood<-coastal.flood[!grepl("storm",coastal.flood)]
eventtype$EVCAT[which(eventtype$EVTYPE %in% coastal.flood & eventtype$n==0)]<-"Coastal Flood"
eventtype$EVCAT[which(eventtype$EVTYPE %in% coastal.storm & eventtype$n==0)]<-"Coastal Storm"
eventtype$n[which(eventtype$EVTYPE %in% coastal.flood & eventtype$n==0)]<-1
eventtype$n[which(eventtype$EVTYPE %in% coastal.storm & eventtype$n==0)]<-1
rm(coastal.flood,coastal.storm)

#cold/wind chill
wind.chill<-unique(eventtype$EVTYPE[grepl("wind chill",eventtype$EVTYPE, ignore.case=TRUE) | grepl("extreme cold",eventtype$EVTYPE, ignore.case=TRUE) | grepl("extremewind",eventtype$EVTYPE, ignore.case=TRUE) | grepl("windchill",eventtype$EVTYPE, ignore.case=TRUE) | grepl("record low",eventtype$EVTYPE)])
wind.chill<-wind.chill[!grepl("blizzard",wind.chill) & !grepl("tornado",wind.chill) & !grepl("fog",wind.chill) & !grepl("frost",wind.chill)]
eventtype$EVCAT[which(eventtype$EVTYPE %in% wind.chill & eventtype$n==0)]<-"Extreme Cold/Wind Chill"
eventtype$n[which(eventtype$EVTYPE %in% wind.chill & eventtype$n==0)]<-1
rm(wind.chill)

#No debris flow
debris.flow<-unique(eventtype$EVTYPE[grepl("*slide*",eventtype$EVTYPE, ignore.case=TRUE)])
eventtype$EVCAT[which(eventtype$EVTYPE %in% debris.flow & eventtype$n==0)]<-"Debris Flow"
eventtype$n[which(eventtype$EVTYPE %in% debris.flow & eventtype$n==0)]<-1
rm(debris.flow)

#Dense Fog, freezing fog
fog<-unique(eventtype$EVTYPE[grepl("fog",eventtype$EVTYPE, ignore.case=TRUE)])
freeze.fog<-fog[grepl("freezing",fog)|grepl("ice",fog)]
fog<-fog[!grepl("freezing",fog)&!grepl("ice",fog)]
eventtype$EVCAT[which(eventtype$EVTYPE %in% fog & eventtype$n==0)]<-"Dense Fog"
eventtype$EVCAT[which(eventtype$EVTYPE %in% freeze.fog & eventtype$n==0)]<-"Freezing Fog"
eventtype$n[which(eventtype$EVTYPE %in% fog & eventtype$n==0)]<-1
eventtype$n[which(eventtype$EVTYPE %in% freeze.fog & eventtype$n==0)]<-1
rm(fog,freeze.fog)

#Dense Smoke
smoke<-unique(eventtype$EVTYPE[grepl("smoke",eventtype$EVTYPE, ignore.case=TRUE)])
eventtype$EVCAT[which(eventtype$EVTYPE %in% smoke & eventtype$n==0)]<-"Dense Smoke"
eventtype$n[which(eventtype$EVTYPE %in% smoke & eventtype$n==0)]<-1
rm(smoke)

#drought, excessive heat/drought classified as excessive heat
drought<-unique(eventtype$EVTYPE[grepl("drought",eventtype$EVTYPE, ignore.case=TRUE) | grepl("abnormally dry",eventtype$EVTYPE, ignore.case=TRUE) | grepl("excessively dry",eventtype$EVTYPE, ignore.case=TRUE) | grepl("record dry",eventtype$EVTYPE, ignore.case=TRUE)])
drought<-drought[!grepl("excessive heat",drought)]
eventtype$EVCAT[which(eventtype$EVTYPE %in% drought & eventtype$n==0)]<-"Drought"
eventtype$n[which(eventtype$EVTYPE %in% drought & eventtype$n==0)]<-1
rm(drought)


#dust devil and dust storm
dust.devil<-unique(eventtype$EVTYPE[grepl("dust",eventtype$EVTYPE, ignore.case=TRUE)])
dust.storm<-dust.devil[grepl("storm",dust.devil)]
dust.devil<-dust.devil[!grepl("storm",dust.devil)]

eventtype$EVCAT[which(eventtype$EVTYPE %in% dust.devil & eventtype$n==0)]<-"Dust Devil"
eventtype$EVCAT[which(eventtype$EVTYPE %in% dust.storm & eventtype$n==0)]<-"Dust Storm"
eventtype$n[which(eventtype$EVTYPE %in% dust.devil & eventtype$n==0)]<-1
eventtype$n[which(eventtype$EVTYPE %in% dust.storm & eventtype$n==0)]<-1
rm(dust.devil,dust.storm)

#Excessive heat
#adjust excessive heat
heat<-unique(eventtype$EVTYPE[grepl("heat",eventtype$EVTYPE, ignore.case=TRUE) | grepl("abnormal warmth",eventtype$EVTYPE, ignore.case=TRUE) | grepl("record high",eventtype$EVTYPE)])
eventtype$EVCAT[which(eventtype$EVTYPE %in% heat & eventtype$n==0)]<-"Excessive Heat"
eventtype$n[which(eventtype$EVTYPE %in% heat & eventtype$n==0)]<-1
rm(heat)

#Flash flood, flood, lakeshore flood
flood<-unique(eventtype$EVTYPE[grepl("flood",eventtype$EVTYPE, ignore.case=TRUE) | grepl("fld",eventtype$EVTYPE, ignore.case=TRUE) | grepl("dam",eventtype$EVTYPE, ignore.case=TRUE) | grepl("flash",eventtype$EVTYPE)])
flash.flood<-flood[grepl("flash",flood)]
lake.flood<-flood[grepl("lake",flood)]
flood<-flood[!grepl("coastal",flood)&!grepl("flash",flood)& !grepl("red",flood)& !grepl("beach",flood)&!grepl("lake",flood)]
eventtype$EVCAT[which(eventtype$EVTYPE %in% flood & eventtype$n==0)]<-"Flood"
eventtype$EVCAT[which(eventtype$EVTYPE %in% flash.flood & eventtype$n==0)]<-"Flash Flood"
eventtype$EVCAT[which(eventtype$EVTYPE %in% lake.flood & eventtype$n==0)]<-"Lakeshore Flood"

eventtype$n[which(eventtype$EVTYPE %in% flood & eventtype$n==0)]<-1
eventtype$n[which(eventtype$EVTYPE %in% flash.flood & eventtype$n==0)]<-1
eventtype$n[which(eventtype$EVTYPE %in% lake.flood & eventtype$n==0)]<-1

rm(flood,flash.flood,lake.flood)

#frost/freeze
frost<-unique(eventtype$EVTYPE[grepl("frost",eventtype$EVTYPE, ignore.case=TRUE) | grepl("freez",eventtype$EVTYPE, ignore.case=TRUE)])
frost<-frost[!grepl("fog",frost) & !grepl("blizzard",frost) & !grepl("rain",frost) & !grepl("sleet",frost) & !grepl("record cold",frost)]
eventtype$EVCAT[which(eventtype$EVTYPE %in% frost & eventtype$n==0)]<-"Frost/Freeze"
eventtype$n[which(eventtype$EVTYPE %in% frost & eventtype$n==0)]<-1
rm(frost)

#Funnel Cloud
funnel<-unique(eventtype$EVTYPE[grepl("funnel",eventtype$EVTYPE)])
funnel<-funnel[!grepl("wind",funnel) & !grepl("water",funnel) & !grepl("cold",funnel) & !grepl("hail",funnel)]
eventtype$EVCAT[which(eventtype$EVTYPE %in% funnel & eventtype$n==0)]<-"Funnel Cloud"
eventtype$n[which(eventtype$EVTYPE %in% funnel & eventtype$n==0)]<-1
rm(funnel)

#Hail
hail<-unique(eventtype$EVTYPE[grepl("hail",eventtype$EVTYPE)])
marine.hail<-hail[grepl("marine",hail)]
hail<-hail[!grepl("flood",hail) & !grepl("marine",hail) & !grepl("tornado",hail)]
eventtype$EVCAT[which(eventtype$EVTYPE %in% hail & eventtype$n==0)]<-"Hail"
eventtype$EVCAT[which(eventtype$EVTYPE %in% marine.hail & eventtype$n==0)]<-"Marine Hail"

eventtype$n[which(eventtype$EVTYPE %in% hail & eventtype$n==0)]<-1
eventtype$n[which(eventtype$EVTYPE %in% marine.hail & eventtype$n==0)]<-1
rm(hail,marine.hail)

#heavy rain
rain<-unique(eventtype$EVTYPE[grepl("rain",eventtype$EVTYPE) | grepl("precip",eventtype$EVTYPE) | grepl("*shower*",eventtype$EVTYPE)])
rain<-rain[!grepl("flood",rain) & !grepl("blizzard",rain) & !grepl("freez",rain) & !grepl("lightning",rain) & !grepl("record low",rain) & !grepl("sleet",rain) & !grepl("thunderstorm",rain) & !grepl("tstm",rain) & !grepl("surf",rain) & !grepl("below",rain)]
eventtype$EVCAT[which(eventtype$EVTYPE %in% rain & eventtype$n==0)]<-"Heavy Rain"
eventtype$n[which(eventtype$EVTYPE %in% rain & eventtype$n==0)]<-1
rm(rain)

#heavy snow
snow<-unique(eventtype$EVTYPE[grepl("snow",eventtype$EVTYPE) | grepl("snowstorm",eventtype$EVTYPE)])
snow<-snow[grepl("heavy",snow) | grepl("excessive",snow) | grepl("record",snow) | grepl("snowstorm",snow)]
snow<-snow[!grepl("lake",snow) & !grepl("blizzard",snow)]
eventtype$EVCAT[which(eventtype$EVTYPE %in% snow & eventtype$n==0)]<-"Heavy Snow"
eventtype$n[which(eventtype$EVTYPE %in% snow & eventtype$n==0)]<-1
rm(snow)

#high tide
high.tide<-unique(eventtype$EVTYPE[grepl("high tide",eventtype$EVTYPE)])
eventtype$EVCAT[which(eventtype$EVTYPE %in% high.tide & eventtype$n==0)]<-"High Tide"
eventtype$n[which(eventtype$EVTYPE %in% high.tide & eventtype$n==0)]<-1
rm(high.tide)

#High surf
surf<-unique(eventtype$EVTYPE[grepl("surf",eventtype$EVTYPE)|grepl("swell",eventtype$EVTYPE)])
surf<-surf[!grepl("rip",surf)]
eventtype$EVCAT[which(eventtype$EVTYPE %in% surf & eventtype$n==0)]<-"High Surf"
eventtype$n[which(eventtype$EVTYPE %in% surf & eventtype$n==0)]<-1
rm(surf)

#adjust hurricane
hurricane<-unique(eventtype$EVTYPE[grepl("hurricane", eventtype$EVTYPE)])
hurricane<-hurricane[!grepl("swell",hurricane)]
eventtype$EVCAT[which(eventtype$EVTYPE %in% hurricane & eventtype$n==0)]<-"Hurricane"
eventtype$n[which(eventtype$EVTYPE %in% hurricane & eventtype$n==0)]<-1
rm(hurricane)


#ice
ice<-unique(eventtype$EVTYPE[grepl("ice storm",eventtype$EVTYPE)])
ice<-ice[!grepl("heavy snow",ice) & !grepl("flood",ice)]
eventtype$EVCAT[which(eventtype$EVTYPE %in% ice & eventtype$n==0)]<-"ice storm"
eventtype$n[which(eventtype$EVTYPE %in% ice & eventtype$n==0)]<-1
rm(ice)

#lake-effect snow
lake.snow<-unique(eventtype$EVTYPE[grepl("lake",eventtype$EVTYPE) & grepl("snow",eventtype$EVTYPE)])
eventtype$EVCAT[which(eventtype$EVTYPE %in% lake.snow & eventtype$n==0)]<-"Lake-Effect Snow"
eventtype$n[which(eventtype$EVTYPE %in% lake.snow & eventtype$n==0)]<-1
rm(lake.snow)

#lightning
lightning<-unique(eventtype$EVTYPE[grepl("light",eventtype$EVTYPE)|grepl("ning",eventtype$EVTYPE)])
lightning<-lightning[!grepl("freezing",lightning) & !grepl("snow",lightning) & !grepl("northern",lightning)]
eventtype$EVCAT[which(eventtype$EVTYPE %in% lightning & eventtype$n==0)]<-"Lightning"
eventtype$n[which(eventtype$EVTYPE %in% lightning & eventtype$n==0)]<-1
rm(lightning)

#marine
marine<-unique(eventtype$EVTYPE[grepl("marine",eventtype$EVTYPE)])
marine.other<-marine[grepl("accident",marine) | grepl("mishap",marine)]
marine.high.wind<-marine[grepl("high wind",marine)]
marine.strong.wind<-marine[grepl("strong wind",marine)]
marine.thunderstorm<-marine[grepl("thunderstorm",marine) | grepl("tstm",marine)]

eventtype$EVCAT[which(eventtype$EVTYPE %in% marine.other & eventtype$n==0)]<-"Marine Other"
eventtype$EVCAT[which(eventtype$EVTYPE %in% marine.high.wind & eventtype$n==0)]<-"Marine High Wind"
eventtype$EVCAT[which(eventtype$EVTYPE %in% marine.strong.wind & eventtype$n==0)]<-"Marine Strong Wind"
eventtype$EVCAT[which(eventtype$EVTYPE %in% marine.thunderstorm & eventtype$n==0)]<-"Marine Thunderstorm Wind"

eventtype$n[which(eventtype$EVTYPE %in% marine.other & eventtype$n==0)]<-1
eventtype$n[which(eventtype$EVTYPE %in% marine.high.wind & eventtype$n==0)]<-1
eventtype$n[which(eventtype$EVTYPE %in% marine.strong.wind & eventtype$n==0)]<-1
eventtype$n[which(eventtype$EVTYPE %in% marine.thunderstorm & eventtype$n==0)]<-1

rm(marine,marine.other,marine.high.wind,marine.strong.wind,marine.thunderstorm)


#rip current
rip.current<-unique(eventtype$EVTYPE[grepl("rip",eventtype$EVTYPE)])
eventtype$EVCAT[which(eventtype$EVTYPE %in% rip.current & eventtype$n==0)]<-"Rip Current"
eventtype$n[which(eventtype$EVTYPE %in% rip.current & eventtype$n==0)]<-1
rm(rip.current)

#seiche
seiche<-unique(eventtype$EVTYPE[grepl("seiche",eventtype$EVTYPE)])
eventtype$EVCAT[which(eventtype$EVTYPE %in% seiche & eventtype$n==0)]<-"Seiche"
eventtype$n[which(eventtype$EVTYPE %in% seiche & eventtype$n==0)]<-1
rm(seiche)

#sleet
sleet<-unique(eventtype$EVTYPE[grepl("sleet",eventtype$EVTYPE)])
sleet<-sleet[!grepl("ice storm",sleet)]
eventtype$EVCAT[which(eventtype$EVTYPE %in% sleet & eventtype$n==0)]<-"Sleet"
eventtype$n[which(eventtype$EVTYPE %in% sleet & eventtype$n==0)]<-1
rm(sleet)

#storm surge
surge<-unique(eventtype$EVTYPE[grepl("storm surge",eventtype$EVTYPE)])
eventtype$EVCAT[which(eventtype$EVTYPE %in% surge & eventtype$n==0)]<-"Storm Surge/Tide"
eventtype$n[which(eventtype$EVTYPE %in% surge & eventtype$n==0)]<-1
rm(surge)

#Thunderstorm
thunderstorm<-unique(eventtype$EVTYPE[grepl("thunderstorm",eventtype$EVTYPE)|grepl("tstm",eventtype$EVTYPE)|grepl("orm wind",eventtype$EVTYPE)|grepl("om winds",eventtype$EVTYPE)])
thunderstorm<-thunderstorm[!grepl("marine",thunderstorm) & !grepl("non",thunderstorm) & !grepl("hail",thunderstorm)]
eventtype$EVCAT[which(eventtype$EVTYPE %in% thunderstorm & eventtype$n==0)]<-"Thunderstorm Wind"
eventtype$n[which(eventtype$EVTYPE %in% thunderstorm & eventtype$n==0)]<-1
rm(thunderstorm)

#Tornado
tornado<-unique(eventtype$EVTYPE[grepl("*tornado*",eventtype$EVTYPE)])
tornado<-tornado[!grepl("water",tornado)]
eventtype$EVCAT[which(eventtype$EVTYPE %in% tornado & eventtype$n==0)]<-"Tornado"
eventtype$n[which(eventtype$EVTYPE %in% tornado & eventtype$n==0)]<-1
rm(tornado)

#tropical
tropical<-unique(eventtype$EVTYPE[grepl("tropical",eventtype$EVTYPE)])
tropical.depression<-tropical[grepl("depression",tropical)]
tropical.storm<-tropical[grepl("storm",tropical)]
eventtype$EVCAT[which(eventtype$EVTYPE %in% tropical.depression & eventtype$n==0)]<-"Tropical Depression"
eventtype$EVCAT[which(eventtype$EVTYPE %in% tropical.storm & eventtype$n==0)]<-"Tropical Storm"
eventtype$n[which(eventtype$EVTYPE %in% tropical.depression & eventtype$n==0)]<-1
eventtype$n[which(eventtype$EVTYPE %in% tropical.storm & eventtype$n==0)]<-1
rm(tropical.storm,tropical.depression,tropical)

#tsunami
tsunami<-unique(eventtype$EVTYPE[grepl("tsunami",eventtype$EVTYPE)])
eventtype$EVCAT[which(eventtype$EVTYPE %in% tsunami & eventtype$n==0)]<-"Tsunami"
eventtype$n[which(eventtype$EVTYPE %in% tsunami & eventtype$n==0)]<-1
rm(tsunami)

#volcanic eruption/ash
volcano<-unique(eventtype$EVTYPE[grepl("volcan",eventtype$EVTYPE)])
eventtype$EVCAT[which(eventtype$EVTYPE %in% volcano & eventtype$n==0)]<-"Volcanic Ash"
eventtype$n[which(eventtype$EVTYPE %in% volcano & eventtype$n==0)]<-1
rm(volcano)

#waterspout
waterspout<-unique(eventtype$EVTYPE[grepl("waterspout",eventtype$EVTYPE)])
waterspout<-waterspout[!grepl("dust",waterspout)]
eventtype$EVCAT[which(eventtype$EVTYPE %in% waterspout & eventtype$n==0)]<-"Waterspout"
eventtype$n[which(eventtype$EVTYPE %in% waterspout & eventtype$n==0)]<-1
rm(waterspout)

#wildfire
wildfire<-unique(eventtype$EVTYPE[grepl("fire",eventtype$EVTYPE)])
wildfire<-wildfire[!grepl("red",wildfire) & !grepl("lightning",wildfire)]
eventtype$EVCAT[which(eventtype$EVTYPE %in% wildfire & eventtype$n==0)]<-"Wildfire"
eventtype$n[which(eventtype$EVTYPE %in% wildfire & eventtype$n==0)]<-1
rm(wildfire)

#winter storm
winter.storm<-unique(eventtype$EVTYPE[grepl("winter storm",eventtype$EVTYPE)])
winter.storm<-winter.storm[!grepl("blizzard",winter.storm) & !grepl("heavy snow",winter.storm)]
eventtype$EVCAT[which(eventtype$EVTYPE %in% winter.storm & eventtype$n==0)]<-"Winter Storm"
eventtype$n[which(eventtype$EVTYPE %in% winter.storm & eventtype$n==0)]<-1
rm(winter.storm)

#winter weather
winter.weather<-unique(eventtype$EVTYPE[grepl("winter",eventtype$EVTYPE)| grepl("snow",eventtype$EVTYPE) | grepl("freezing",eventtype$EVTYPE) | grepl("ice",eventtype$EVTYPE)| grepl("icy",eventtype$EVTYPE) | grepl("glaze",eventtype$EVTYPE) | grepl("mix",eventtype$EVTYPE)])
winter.weather<-winter.weather[!grepl("blizzard",winter.weather) & !grepl("fog",winter.weather) & !grepl("sleet",winter.weather) & !grepl("heavy",winter.weather) & !grepl("flood",winter.weather) & !grepl("chill",winter.weather) & !grepl("storm",winter.weather) & !grepl("lake",winter.weather) & !grepl("excessive",winter.weather) & !grepl("record",winter.weather) & !grepl("storm",winter.weather) & !grepl("lack of",winter.weather)]

eventtype$EVCAT[which(eventtype$EVTYPE %in% winter.weather & eventtype$n==0)]<-"Winter Weather"
eventtype$n[which(eventtype$EVTYPE %in% winter.weather & eventtype$n==0)]<-1
rm(winter.weather)

#High wind and strong wind
wind<-unique(eventtype$EVTYPE[grepl("wind",eventtype$EVTYPE)| grepl("microburst",eventtype$EVTYPE) | grepl("downburst",eventtype$EVTYPE)])
high.wind<-wind[grepl("high wind",wind) | grepl("high  winds",wind)]
strong.wind<-wind[grepl("strong wind",wind) | grepl("microburst",wind) | grepl("downburst",wind)|grepl("mirco",wind)|grepl("strong",wind)|grepl("gust",wind)]
strong.wind<-strong.wind[!grepl("marine",strong.wind)]
wind<-wind[!(wind %in% high.wind) & !(wind %in% strong.wind)]
wind<-wind[!grepl("tstm",wind) & !grepl("chill",wind) & !grepl("rain",wind) & !grepl("surf",wind) & !grepl("snow",wind) & !grepl("marine",wind) & !grepl("thunder",wind) & !grepl("storm",wind) & !grepl("hail",wind)]
eventtype$EVCAT[which(eventtype$EVTYPE %in% high.wind & eventtype$n==0)]<-"High Wind"
eventtype$EVCAT[which(eventtype$EVTYPE %in% strong.wind & eventtype$n==0)]<-"Strong Wind"
eventtype$EVCAT[which(eventtype$EVTYPE %in% wind & eventtype$n==0)]<-"Wind"

eventtype$n[which(eventtype$EVTYPE %in% high.wind & eventtype$n==0)]<-1
eventtype$n[which(eventtype$EVTYPE %in% strong.wind & eventtype$n==0)]<-1
eventtype$n[which(eventtype$EVTYPE %in% wind & eventtype$n==0)]<-1
rm(wind,high.wind,strong.wind)

#high seas
seas<-unique(eventtype$EVTYPE[grepl("high seas",eventtype$EVTYPE, ignore.case=TRUE) | grepl("heavy seas",eventtype$EVTYPE) | grepl("High Seas",eventtype$EVTYPE) | grepl("rough seas", eventtype$EVTYPE)])
eventtype$EVTYPE[which(eventtype$EVTYPE %in% seas & eventtype$n==0)]<-"High Seas"
eventtype$n[which(eventtype$EVTYPE %in% seas & eventtype$n==0)]<-1
rm(seas)

#Cold, uncat
cold<-unique(eventtype$EVTYPE[grepl("cold",eventtype$EVTYPE,ignore.case = TRUE) | grepl("cool",eventtype$EVTYPE) | grepl("Cold",eventtype$EVTYPE)])
eventtype$EVTYPE[which(eventtype$EVTYPE %in% cold & eventtype$n==0)]<-"Cold"
eventtype$n[which(eventtype$EVTYPE %in% cold & eventtype$n==0)]<-1
rm(cold)

#dry
dry<-unique(eventtype$EVTYPE[grepl("dry",eventtype$EVTYPE, ignore.case = TRUE) | grepl("driest", eventtype$EVTYPE) | grepl("below normal precipitation",eventtype$EVTYPE) | grepl("Dry",eventtype$EVTYPE)])
eventtype$EVTYPE[which(eventtype$EVTYPE %in% dry & eventtype$n==0)]<-"Dry"
eventtype$n[which(eventtype$EVTYPE %in% dry & eventtype$n==0)]<-1
rm(dry)

#uncategorized
eventtype$EVCAT[is.na(eventtype$EVCAT)]<-"Other"
eventtype$n[which(eventtype$n==0)]<-1

#Health
event.health<-eventtype %>% group_by(EVCAT) %>% summarize(Fatalities=sum(FATALITIES),Injuries=sum(INJURIES),health=sum(health,na.rm=TRUE)) %>% arrange(desc(health))

head(event.health,10)

event.health<-event.health[1:10,]

#Plot
g<-ggplot(event.health, aes(EVCAT,health))
p<-g+geom_col(aes(fill=EVCAT))+labs(title="Mortality Rate of Weather Events in the United States")
print(p)

#Economic
event.damage<-eventtype %>% group_by(EVCAT) %>% summarize(Property=sum(PROPDMG,na.rm=TRUE),Crops=sum(CROPDMG,na.rm=TRUE),total=sum(PROPDMG,CROPDMG,na.rm=TRUE)) %>% arrange(desc(total))
event.damage<-event.damage[1:10,]

g<-ggplot(event.damage,aes(EVCAT,total))
p<-g+geom_col(aes(fill=EVCAT))+labs(title="Economic Effects of Weather Events in the United States")
