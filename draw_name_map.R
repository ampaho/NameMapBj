getPckg <- function(pckg) install.packages(pckg, repos = "http://cran.r-project.org")

pckg = try(require(maps))
if(!pckg) {
  cat("Installing 'maps' from CRAN\n")
  getPckg("maps")
  require("maps")
}

pckg = try(require(mapdata))
if(!pckg) {
  cat("Installing 'mapdata' from CRAN\n")
  getPckg("mapdata")
  require("mapdata")
}

pckg = try(require(maptools))
if(!pckg) {
  cat("Installing 'maptools' from CRAN\n")
  getPckg("maptools")
  require("maptools")
}

pckg = try(require(scales))
if(!pckg) {
  cat("Installing 'scales' from CRAN\n")
  getPckg("scales")
  require("scales")
}

pckg = try(require(data.table))
if(!pckg) {
  cat("Installing 'data.table' from CRAN\n")
  getPckg("data.table")
  require("data.table")
}

#plot.new()
#dev.off()

if(!exists("cities"))
{
  cat("Loading cities\n")
  cities <- data.table(read.csv("./cities.csv"))
}

if(!exists("departements"))
{
  cat("Loading departements\n")
  departements <- data.table(read.csv("./departements.csv"))
}

if(!exists("people"))
{
  cat("Loading people\n")
  #people.csv
  people <- read.csv("./people.csv")
}

#1,3 7,12
#graphics.off()



show_name_map <- function(name, area="city")
{
  plot.new()
  map("worldHires","Benin", col="gray90", fill=TRUE) 
  map.axes()
  
  name <- iconv(name, from="UTF8", to="ASCII//TRANSLIT")
  name <- gsub("[^[:alpha:]]", "", name)
  name <- toupper(name)
  
  cat("Distribution geographique du nom de famille:", name, "\n")
  
  if(area == "city")
  {
  people_name <- subset(people, people$Name==name, as.vector(cities$CityID))
  plot_max_size=2
  }
  else
  {
  people_name <- subset(people, people$Name==name, as.vector(departements$DepartementID))
  plot_max_size=4
  }
  
  #print(people_name)
  #return
  
  
  
  #print(cities$CityID)
  
  #glimpse(people_name)
  people_name=people_name[1,]
  
  #head(people_name)
  
  #print(length(people_name))
  
  sub_max=max(people_name)
  col_names=colnames(people_name)
  
  #print(is.na(sub_max))
  
  if(is.na(sub_max))
  {
    stop ("Desolé. Le nom ",name," n'est pas populaire au Bénin.", call.=FALSE)
  }
  
  #print(col_names)
  
 # return()
  
  for(i in seq_along(col_names)){
    if(people_name[[col_names[i]]] > 0)
    {
      tmp=cities[CityID==col_names[i]]
      #cat(col_names[i],"\n")
      plot_size=(people_name[[col_names[i]]]*plot_max_size)/sub_max
      if(plot_size < 0.1){
        plot_size=0.1
      }
      points(tmp$Lat, tmp$Long, pch=19,cex=plot_size,col=alpha("red", 0.6))
      
      if(plot_size > 1)
      {
        text(tmp$Lat, tmp$Long, labels=tmp$City, cex=0.5, adj=.2, pos=2)
  
      
      }
    }
  }
  
 
 
}
#dev.off()