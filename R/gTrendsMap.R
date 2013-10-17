gTrendsMap <-
function(gTrendsData)  
{
  if(class(gTrendsData) != "gTrendsData"){
    stop("Must be an object of class gTrendsData.")    
  }
  
  temp = data.frame(loc = gTrendsData$Regions$Regions, hits = gTrendsData$Regions$Hits)
  
  G1 <- gvisGeoChart(temp, 'loc', 'hits', options=list(displayMode = "regions", region = gTrendsData$GEO, resolution = "provinces"))
  
  
  temp = data.frame(x = gTrendsData$Cities$Cities, y = gTrendsData$Cities$Hits)
  
  G2 <- gvisGeoChart(temp, 'x', 'y', options=list(displayMode = "markers", region = gTrendsData$GEO, resolution = "provinces"))
  
  
  temp = data.frame(x = gTrendsData$WeeklyHits$JulianDates, y = gTrendsData$WeeklyHits$Hits)
  G3 = gvisLineChart(temp,'x', 'y', options=list(width = 1000, title =paste("Interest over time for keyword", (toupper(gTrendsData$SearchInfo$SreachTerms))), vAxes="[{title:'Search hits'}]", hAxes="[{title:'Date (julian day)'}]"))
  
  
  G4 = gvisMerge(gvisMerge(G3, G1), G2)
  
  plot(G4)
  
}
