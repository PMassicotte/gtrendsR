formatTrends <-
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
  SearchInfo = data.frame(Timespan = temp, SreachTerms = ss)
    
  #Weekly hits
  temp  = ans[[2]]
  Weeks = data.frame(sDates = temp$row.names[2:length(temp[,1])], Hits = as.numeric(as.character(temp[2:length(temp[,1]),2])))
  Weeks$Dates = as.Date(substr(Weeks$sDates,1,10),format = "%Y-%m-%d")
  Weeks$JulianDates = as.numeric(as.Date(Weeks$sDate)) + 2440588
  Weeks$Months = format(Weeks$Dates,"%m")
  Weeks$Years = format(Weeks$Dates,"%y")
  Weeks$Days = format(Weeks$Dates,"%d")
  
  #-------------------------------------
  #Normllize data google style
  #-------------------------------------
  xx =  aggregate(Weeks$Hits, by = list(months = Weeks$Months, years = Weeks$Years), sum)
  xx$x = xx$x / max(xx$x, na.rm = T)*100
  
  
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

      
  gDataFormated = list(SearchInfo = SearchInfo, Weeks = Weeks, Regions = Regions, Cities = Cities, TopSearches = TopSearches, Norm = xx$x)

  
  return(gDataFormated)
  
}
