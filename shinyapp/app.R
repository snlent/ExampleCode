library(data.table)
library(maps)
library(shiny)
library(dplyr)
library(ggplot2)
library(ggiraph)
library(ggpubr)
library(shinyWidgets)

setwd('/Users/lent/Documents/projects/miles4migrants/AnnualReport2019/shinyapp/')
flights<-fread('./FlightData_2019Only_NoIdentifiers.txt',data.table=F)
load('./worlddata.RData')

#mapcolors<-get_palette(palette=c('white','#80c51d','#40841c'),10)
mapcolors<-get_palette(palette=c('white','#80c51d'),10)

map_theme <- function () { 
  theme_bw() + theme(axis.text = element_blank(),
                     axis.title = element_blank(),
                     axis.ticks=element_blank(),
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_rect(fill='#CBF2F7'), 
                     plot.margin = margin(0,0,0,0, "cm"),
                     panel.border = element_blank())
}

worlddata <- subset(worlddata, lat >= -60 & lat <= 90)

worldMapPlot<-function(flightdat, plot.type) {
  if (plot.type %in% c('Country of origin','Destination country')) {
    plotdat <- flightdat %>% group_by(iso3) %>% summarise(N = sum(TravelerCount)) %>% right_join(worlddata,by='iso3')
    p <- ggplot() + geom_polygon_interactive(data = plotdat, color = 'gray70', size = 0.4, aes(x = long, y = lat, fill = N, group = group, tooltip = sprintf("Country: %s<br/>Passengers: %s", region, N))) + scale_fill_gradientn(colours=mapcolors,guide=FALSE, na.value='white') + map_theme() + theme(aspect.ratio=0.5)
    p <- girafe(ggobj = p)
    p <- girafe_options(p, opts_zoom(min = .7, max = 2))
   # p <- ggplot() + geom_map_interactive(data = plotdat, color = 'gray70', size = 0.4, aes(x = long, y = lat, fill = N, group = group, tooltip = sprintf("Country: %s<br/>Passengers: %s", region, N))) + scale_fill_gradientn(colours=mapcolors,guide=FALSE,na.value = 'white') + map_theme() + theme(aspect.ratio=0.5)
  } else if (plot.type=='Flight paths') {
    with.stop<-which(grepl('-[A-Z]+-',flightdat$ItinerarySeq))
    flightdat$ItinerarySeq[with.stop]<-gsub('-[A-Z]+-','-',flightdat$ItinerarySeq[with.stop])
    plotdat <- flightdat %>% group_by(ItinerarySeq) %>% summarise(N=sum(TravelerCount)) %>% right_join(unique(flightdat[,c('OriginAirport.Lat','OriginAirport.Long','DestAirport.Lat','DestAirport.Long','ItinerarySeq')]),by='ItinerarySeq')
    p <- ggplot() + geom_polygon(data = worlddata, color = 'gray70', size = 0.4, aes(x = long, y = lat, group = group),fill='#FFFFFF') + geom_segment_interactive(data=plotdat,aes(x = OriginAirport.Long, xend =DestAirport.Long, y=OriginAirport.Lat, yend=DestAirport.Lat, tooltip=sprintf("Route: %s<br/>Passengers: %s", ItinerarySeq, N)), size=0.8, colour='#80c51d', alpha=0.3) + map_theme() + theme(aspect.ratio=0.5)
  }
  return(p)
}

ui<-shinyUI(fluidPage(
  chooseSliderSkin('Nice'),
  setSliderColor('#80c51d',1),
  fluidRow(
    column(1),
    column(3,selectizeInput('plotvar','Map type',choices=c('Country of origin','Destination country','Flight paths'),selected='Country of origin',multiple=F)),
    column(4,selectizeInput('partners','Charitable partner',choices=c('All',sort(unique(flights$Partner))),selected='All',multiple=T)),
    column(3,sliderInput('daterange','Date booked',min = as.Date("2019-01-01","%Y-%m-%d"), max = as.Date("2019-12-31","%Y-%m-%d"), value=c(as.Date("2019-01-01"),as.Date("2019-12-31")),timeFormat="%Y-%m-%d")),
    column(1)
  ),
  fluidRow(actionButton("makeplot", "Make map", icon("globe-americas")),align='center'),
  fluidRow(
    tags$style(type = "text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"),
    ggiraphOutput("map"))
))


server<-function(input,output) {
  plotdat<-eventReactive(input$makeplot,{
    mapdat<-flights[flights$BookingDate<=input$daterange[2] & flights$BookingDate>=input$daterange[1],]
    if(!('All' %in% input$partners)) {
      mapdat<-mapdat[mapdat$Partner %in% input$partners,]
    }
    if(input$plotvar=='Country of origin') {
      mapdat <- mapdat %>% rename(iso3 = OriginCode) 
    } else if (input$plotvar=='Destination country') {
      mapdat <- mapdat %>% rename(iso3 = DestCode) 
    }
    mapdat
  })
  output$map<-renderGirafe({
    p<-worldMapPlot(plotdat(), plot.type=input$plotvar)
    ggiraph(code = print(p),width=1,width_svg=11,height_svg=6)
  })
}

shinyApp(ui=ui,server=server)
