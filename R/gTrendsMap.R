gTrendsMap <-
function(gTrendsData)  
{
  if(class(gTrendsData) != "gTrendsData"){
    warning("Must be an object of class gTrendsData.", call. = F)    
    return(NULL)
  }
  
  temp = data.frame(loc = gTrendsData$Regions$Regions, hits = gTrendsData$Regions$Hits)
  
  G1 <- gvisGeoChart(temp, 'loc', 'hits', options=list(displayMode = "regions", region = gTrendsData$GEO, resolution = "provinces"))
  
  
  temp = data.frame(x = gTrendsData$Cities$Cities, y = gTrendsData$Cities$Hits)
  
  G2 <- gvisGeoChart(temp, 'x', 'y', options=list(displayMode = "markers", region = gTrendsData$GEO, resolution = "provinces"))
  
  
  temp = data.frame(x = gTrendsData$MonthlyHits$Dates, y = gTrendsData$MonthlyHits$Hits)
  G3 = gvisLineChart(temp,'x', 'y', options=list(width = 1000, title = (toupper(gTrendsData$SearchInfo$SearchTerms)), vAxes="[{title:'Search hits'}]", hAxes="[{title:'Date'}]"))
  
  
  G4 = gvisMerge(gvisMerge(G3, G1), G2)
  
  plot(G4)
  
}
