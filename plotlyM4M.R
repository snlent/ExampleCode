library(geosphere)
library(plotly)
library(dplyr)
library(htmlwidgets)

setwd('/Users/lent/Documents/projects/miles4migrants/interactivemap')

# Set colors
watercolor<-'#A5CBFD'
routecolor<-'#83C115'
bordercolor<-'grey65'
countrycolor<-'#FBFAFF'
partnercols<-c('#216F0B','#83C115','#0F2377','#4573DD','#820263')
names(partnercols)<-c('No One Left Behind','Collateral Repair Project','Together Now','Small Projects Istanbul','Caritas Belgium')

# airport locations
flights<-read.table('./data/RshinyFlightData_jitter1.txt',sep='\t',stringsAsFactors=F,header=T)
flights$color<-toRGB(partnercols[flights$partner])
# Remove first leg of EBB-BRU
flights<-flights[flights$caseno.partner!='9.1.CB',]
flights[flights$caseno.partner=='6.CB','route.nolayover']<-'BEY-DUS'
airportnames<-unique(unlist(strsplit(flights$route.nolayover,'-')))
airports<-fread('/Users/lent/Documents/projects/miles4migrants/AirportInfo.txt',data.table=F)
colnames(airports)<-c('ID','Name','City','Country','IATA','ICAO','lat','long','altitude','timezone','dst','timezone.tz','type','source')
airports<-airports[airports$IATA!='\\N',c('Name','IATA','City','lat','long')]
rownames(airports)<-airports$IATA
airports<-airports[airportnames,]
rownames(airports)<-airportnames
if (sum(is.na(airports$IATA))>0) {
  warning(sprintf("%i airport(s) missing from https://openflights.org/data.html, enter coordinates manually for: %s",sum(is.na(airports$IATA)),paste0(rownames(airports)[is.na(airports$IATA)],collapse=', ')))
}

airports['DUS','City']<-'Düsseldorf'
# DOH missing 
airports['DOH',]<-c('Doha International Airport','DOH','Doha',25.261111,31.565)
airports$lat<-as.numeric(airports$lat)
airports$long<-as.numeric(airports$long)
airports$partner<-'none'
airports<-airports[unique(c(gsub('-.*$','',flights$route.nolayover),gsub('^.*-','',flights$route.nolayover))),]

flights$orig.city<-airports[gsub('-.*$','',flights$route.nolayover),'City']
flights$dest.city<-airports[gsub('^.*-','',flights$route.nolayover),'City']
flightslong<-data.frame(lon=numeric(0),lat=numeric(0),caseno.partner=numeric(0)) 
for (i in 1:nrow(flights)) {
  origin<-c(flights$origin.long.jitter[i],flights$origin.lat.jitter[i])
  dest<-c(flights$dest.long.jitter[i],flights$dest.lat.jitter[i])
  dist<-sqrt(sum((origin-dest)^2))
  # Longest dist: route to Seattle, 191 degrees, worked reasonably well with n.inter=20
  n.inter=round(dist/1.5)
  inter <-data.frame(gcIntermediate(origin,dest,n=n.inter,addStartEnd=T))
  inter$caseno.partner<-flights$caseno.partner[i]
  flightslong<-rbind(flightslong,inter)
}

flightslong<-merge(flightslong,flights[,c('partner','caseno.partner','rewards.full','airline.full','route.nolayover','orig.city','dest.city')],by='caseno.partner')
flightslong$airline.full[flightslong$airline.full=='Etihad Airlines']<-'Etihad Airways'
flightslong$rewards.full[flightslong$rewards.full=='American Airlines Aadvantage']<-'American Airlines AAdvantage'
flightslong$rewards.full[flightslong$rewards.full=='Air Canada Altitude']<-'Air Canada Aeroplan'
flightslong$caption<-sprintf('Route: %s to %s\nPartner: %s\nRewards Program: %s\nAirline: %s',flightslong$orig.city,flightslong$dest.city,flightslong$partner,flightslong$rewards.full,flightslong$airline.full)

# globe projection
geo <- list(
  scope = 'world',
  projection = list(
    type = 'orthographic',
    rotation = list(
      lon = 5,
      lat = 20,
      roll = 0),
    scale=0.5),
  showland = TRUE,
  landcolor = toRGB('#FBFAFF'),
  countrycolor = toRGB('grey65'),
  showocean=TRUE,
  oceancolor=toRGB('#A5CBFD'),
  showcountries=TRUE,
  countrycolor=toRGB('#FBFAFF')
)

partnercols<-c('#216F0B','#83C115','#0F2377','#4573DD','#820263')
names(partnercols)<-c('No One Left Behind','Collateral Repair Project','Together Now','Small Projects Istanbul','Caritas Belgium')
colorlist<-c(partnercols[flightslong$partner],'grey50')
# With add_lines
GCs.grouped<-group_by(flightslong,caseno.partner)
p <- plot_geo(locationmode = 'World') %>%
  add_lines(
    data = GCs.grouped,
    x = ~lon, y = ~lat, 
    color = ~partner, colors=c('#8ED80F','#216F0B','#306EFF','grey55','#FFCC00','#1430A0'), text=~caption, size = I(2), hoverinfo = "text", alpha=1, inherit=FALSE
  ) %>%
  add_trace(
    data = airports, x = ~long, y = ~lat, text = '✈', 
    hoverinfo = "none", alpha=1, mode='text',inherit=FALSE, color = ~partner, textfont=list(size=20)
  ) %>% 
  add_markers(
    data = airports, x = ~long, y = ~lat, text = ~Name, 
    hoverinfo = "text", alpha=0.1, inherit=FALSE, color = ~partner
  ) %>%
  layout(
    geo = geo, showlegend = FALSE,
    hoverlabel = list(font=list(size=10),alpha=1),
    margin=list(l=0,r=0,b=0,t=0,pad=0)
  ) %>%
  config(showLink = FALSE)

# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
chart_link = api_create(p, filename="M4Mflights")
chart_link



