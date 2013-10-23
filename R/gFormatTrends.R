gFormatTrends <-
function(gData)
{
  #gData =data
  
  blocks = strsplit(gData, "\\\n{2,}")[[1]]  
  blocks = blocks[1:5]
  
  ans = lapply(blocks, readCSVBlock)
  #print(ans)
  
  temp  = ans[[1]]  
  ss = unlist(strsplit(colnames(temp), "[.]"))
  ss = ss[length(ss)]
  SearchInfo = data.frame(Timespan = temp, SearchTerms = gsub("\n", " " ,blocks[1]))
  
  #-------------------------------------
  #Weekly hits
  #-------------------------------------
  temp  = ans[[2]]
  Weeks = data.frame(sDates = temp$row.names[2:length(temp[,1])], Hits = as.numeric(as.character(temp[2:length(temp[,1]),2])))
  Weeks$Dates = as.Date(substr(Weeks$sDates,1,10),format = "%Y-%m-%d")
  Weeks$JulianDates = as.numeric(as.Date(Weeks$sDate)) + 2440588
  Weeks$Months = format(Weeks$Dates,"%m")
  Weeks$Years = format(Weeks$Dates,"%y")
  Weeks$Days = format(Weeks$Dates,"%d")
  
  #-------------------------------------
  #Normllize data google style
  #Montly hits
  #-------------------------------------
  Months =  aggregate(Weeks$Hits, by = list(Months = Weeks$Months, Years = Weeks$Years), sum)
  Months$x = Months$x / max(Months$x, na.rm = T)*100
  colnames(Months)[3] = "Hits"
  
  #lapply(as.numeric(Weeks$Months),mean)
  
  #Regions
  temp  = ans[[3]]
  Regions = data.frame(Regions = temp$row.names[2:length(temp[,1])], Hits = as.numeric(as.character(temp[2:length(temp[,1]),2])))
  
  #Cities
  temp  = ans[[4]]
  Cities = data.frame(Cities = temp$row.names[2:length(temp[,1])], Hits = as.numeric(as.character(temp[2:length(temp[,1]),2])))
  
  #Top searches
  temp  = ans[[5]]
  TopSearches = data.frame(Keywords = temp$row.names[2:length(temp[,1])], Hits = as.numeric(as.character(temp[2:length(temp[,1]),2])))
  
  
  gDataFormated = list(SearchInfo = SearchInfo, WeeklyHits = Weeks, MonthlyHits = Months, Regions = Regions, Cities = Cities, TopSearches = TopSearches)
  
  class(gDataFormated) = "gTrendsData"
  
  return(gDataFormated)
  
}
