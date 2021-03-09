# Load shiny packages
library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyBS)
library(shinyjs)
library(DT)
library(profvis)

# Tables
library(rpivotTable)

# Data manipulation
library(plyr)
library(dplyr)
library(stringr)
library(tidyr)
library(readr)
library(fst)
library(fuzzyjoin)
library(forcats)
library(naniar)

# Plotting
library(ggplot2)
library(grid)

# Maps
library(rgdal)
library(rgeos)
library(raster)
library(leaflet)
library(leaflet.minicharts)

# Global archive functions
library(devtools)
#devtools::install_github('UWAMEGFisheries/GlobalArchive')
library(GlobalArchive)

# Dashboard header (logos and name) ----
dbHeader <- dashboardHeader()
dbHeader$children[[2]]$children <-  tags$a(href='http://mycompanyishere.com',
                                           tags$img(src='https://www.nespmarine.edu.au/sites/default/themes/nespmarine/logo.png',height='60',width='200'))

dbHeader <- dashboardHeader(title = "Visualiser",
                            tags$li(a(href = 'https://marineecology.io/',
                                      img(src = 'https://github.com/UWAMEGFisheries/UWAMEGFisheries.github.io/blob/master/images/MEG-white.png?raw=true',
                                          title = "Marine Ecology Group", height = "50px"),
                                      style = "padding-top:10px; padding-bottom:10px;"),
                                    class = "dropdown"),
                            tags$li(a(href = 'https://www.nespmarine.edu.au/',
                                      img(src = 'https://github.com/UWAMEGFisheries/UWAMEGFisheries.github.io/blob/master/images/mbh-logo-white-cropped.png?raw=true',
                                          title = "Marine Biodiversity Hub", height = "50px"),
                                      style = "padding-top:10px; padding-bottom:10px;"),
                                    class = "dropdown"))

# functions for summarising data on plots ----
se <- function(x) sd(x) / sqrt(length(x))
se.min <- function(x) (mean(x)) - se(x)
se.max <- function(x) (mean(x)) + se(x)

empty_as_na <- function(x){
  if("factor" %in% class(x)) x <- as.character(x) ## since ifelse wont work with factors
  ifelse(as.character(x)!="", x, NA)
}

# LIFE HISTORY DATA ----
life.history <- read.csv("data/Australia.life.history - australia.life.history.csv")%>%
    mutate(trophic.group=ga.capitalise(RLS.trophic.group))%>%
    dplyr::mutate(target.group=str_replace_all(.$Fishing.type,c("R"="Recreational","C"="Commercial","B/"="","B"="Bycatch","Commercial/Recreational"="Target","Commercial"="Target","Recreational"="Target")))%>%
    dplyr::mutate(trophic.group=str_replace_all(.$trophic.group,c("NANA"="Missing trophic group","NA"="Missing trophic group")))%>%
    naniar::replace_with_na(replace = list(trophic.group = ""))%>%
    tidyr::replace_na(list(target.group="Non-target",trophic.group="Missing trophic group"))%>%
    dplyr::mutate(target.group = factor(target.group, levels = c("Target","Bycatch","Non-target")))%>%
    dplyr::mutate(target.group = fct_relevel(target.group, "Target","Bycatch","Non-target"))%>%
    dplyr::select(CAAB,Family,Genus,Species,trophic.group,target.group,Australian.common.name)%>%
    ga.clean.names()%>%
    dplyr::mutate(fish.names=paste(genus," ",species," (",australian.common.name,")",sep=""))

# Theme for plotting ----
Theme1 <-
  theme( # use theme_get() to see available options
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    legend.background = element_blank(),
    legend.key = element_blank(), # switch off the rectangle around symbols in the legend
    legend.text = element_text(size=14),
    legend.title = element_blank(),
    legend.position = "right",
    strip.text.y = element_text(size = 14,angle = 0),
    axis.title.x=element_text(vjust=0.3, size=14),
    axis.title.y=element_text(angle=90, size=14), #vjust=0.6, 
    axis.text.y=element_text(size=12),
    axis.text.x=element_text(size=12),
    axis.line.x=element_line(colour="black", size=0.5,linetype='solid'),
    axis.line.y=element_line(colour="black", size=0.5,linetype='solid'),
    strip.background = element_blank(),
    plot.title = element_text(color="black", size=15, face="bold"),
    text=element_text(family="Times New Roman", size=14),
    panel.grid = element_blank(), 
    panel.border = element_blank(), 
    axis.line = element_line(colour = "black"))

theme.stacked.plot <-    theme_bw()+
  theme( # use theme_get() to see available options
    panel.grid = element_blank(), 
    panel.border = element_blank(), 
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    legend.background = element_blank(),
    legend.key = element_blank(), # switch off the rectangle around symbols in the legend
    legend.text = element_text(size=12),
    legend.title = element_blank(),
    #legend.position = "top",
    text=element_text(size=12),
    strip.text.y = element_text(size = 12,angle = 0),
    axis.title.x=element_text(vjust=0.3, size=12),
    axis.title.y=element_text(vjust=0.6, angle=90, size=12),
    axis.text.y=element_text(size=12),
    axis.text.x=element_text(size=12),
    axis.line.x=element_line(colour="black", size=0.5,linetype='solid'),
    axis.line.y=element_line(colour="black", size=0.5,linetype='solid'),
    strip.background = element_blank(),
    plot.title = element_text(color="black", size=12, face="bold.italic"))


theme_collapse<-theme(      ## the commented values are from theme_grey
  panel.grid.major=element_line(colour = "white"), ## element_line(colour = "white")
  panel.grid.minor=element_line(colour = "white", size = 0.25), 
  plot.margin= grid::unit(c(0, 0, 0, 0), "in"))


css <- "
.shiny-output-error { visibility: hidden; }
.shiny-output-error:before {
  visibility: visible;
  content: 'An error occurred. Please contact the admin.'; }
}
"

