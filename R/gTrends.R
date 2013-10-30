gTrends = function(ch, geo = 'all', query)  
{
  authenticatePage2 = getURL("http://www.google.com", curl = ch)
  
  ## get Google Insights results CSV
  #trendsURL <- "http://www.google.com/trends/viz?"
  trendsURL <- "http://www.google.com/trends/TrendsRepport?"
  resultsText <- getForm(trendsURL, .params = list(q = query, geo = geo, export = 1, hl = 'en', content=1, graph = 'all_csv'), curl = ch, .opts = list(verbose = F))
  
  #print(resultsText)
  
  #print(rawToChar(resultsText))
  
  ## Sometimes we reach quota limit, in that case stop!
  if(any(grep("QUOTA", resultsText))){
    stop("Reached Google Trends quota limit! Please try again later.") 
  }
    
  resultsText = gFormatTrends(resultsText)
  
  resultsText$GEO = geo
  
  return(resultsText)
  
}
