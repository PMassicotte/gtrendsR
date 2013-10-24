gFormatTrends <-
function(gData)
{
  #gData =data
  
  blocks = strsplit(gData, "\\\n{2,}")[[1]]  
  
  ##-----------------------------------------------------------------------------
  ## Results should have 6 blocks 
  ## 1 - Web interest
  ## 2 - Interest over time
  ## 3 - Top subregions
  ## 4 - Top cities
  ## 5 - Top releated seraches
  ## 6 - Rising searches
  ##-----------------------------------------------------------------------------
  if(length(blocks) < 6){
    print("Not enough search volume to show results.")
    return(NULL)
  }
      
  blocks = blocks[1:6]
  
  ans = lapply(blocks, readCSVBlock)
  
  ## Find the block where each subset is located
  index.interest = grep("Interest.over.time", ans)
  index.subreg = grep("Top.subregions", ans)
  index.cities = grep("Top.cities", ans)
  index.topsearches = grep("Top.searches", ans)
  index.risingsearches = grep("Rising.searches", ans)
  
  
  temp  = ans[[1]] 
  SearchInfo = data.frame(Timespan = temp, SearchTerms = gsub("\n", " " ,blocks[1]))
  
  #-------------------------------------
  #Regions
  #-------------------------------------
  temp  = ans[[index.subreg]]
  Regions = data.frame(Regions = temp$row.names[2:length(temp[,1])], Hits = as.numeric(as.character(temp[2:length(temp[,1]),2])))
  
  #-------------------------------------
  #Cities
  #-------------------------------------
  temp  = ans[[index.cities]]
  Cities = data.frame(Cities = temp$row.names[2:length(temp[,1])], Hits = as.numeric(as.character(temp[2:length(temp[,1]),2])))
  
  #-------------------------------------
  #Top searches
  #-------------------------------------
  temp  = ans[[index.topsearches]]
  TopSearches = data.frame(Keywords = temp$row.names[2:length(temp[,1])], Hits = as.numeric(as.character(temp[2:length(temp[,1]),2])))
  
  
  #-------------------------------------
  #Weekly hits
  #-------------------------------------
  temp  = ans[[index.interest]]
  Weeks = data.frame(sDates = as.character(temp$row.names[2:length(temp[,1])]), Hits = as.numeric(as.character(temp[2:length(temp[,1]),2])))
  
  ## Sometime, search volume if not enough, so Google return montly data rather than daily.
  if(nchar(as.character(Weeks$sDate[1])) > 7){
    
    Weeks$Dates = as.Date(substr(Weeks$sDates,1,10),format = "%Y-%m-%d")
    #Weeks$JulianDates = as.numeric(as.Date(Weeks$sDate)) + 2440588
    Weeks$Months = format(Weeks$Dates,"%m")
    Weeks$Years = format(Weeks$Dates,"%Y")
    Weeks$Days = format(Weeks$Dates,"%d")
    
    #-------------------------------------
    #Normllize data google style
    #Montly hits
    #-------------------------------------
    Months =  aggregate(Weeks$Hits, by = list(Months = Weeks$Months, Years = Weeks$Years), sum)
    Months$x = Months$x / max(Months$x, na.rm = T)*100
    colnames(Months)[3] = "Hits"
    Months$Dates = as.Date(paste(paste(Months[,2], Months[,1], sep = "-"),"-01",sep=""))
    
    gDataFormated = list(SearchInfo = SearchInfo, WeeklyHits = Weeks, MonthlyHits = Months, Regions = Regions, Cities = Cities, TopSearches = TopSearches)
    
  }else{
    
    warning("Not enough search volume to show daily results. Data formated using monthly hits.", call. = F)    
    Weeks$Dates = as.Date(paste(substr(Weeks$sDates,1,7),"-01",sep=""))
    
    gDataFormated = list(SearchInfo = SearchInfo, MonthlyHits = Weeks, Regions = Regions, Cities = Cities, TopSearches = TopSearches)
  }
     
  class(gDataFormated) = "gTrendsData"
  
  return(gDataFormated)
  
}
