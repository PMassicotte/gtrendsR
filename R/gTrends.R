gTrends <-
function(ch, geo = 'all', query)  
{
  
  ## get Google Insights results CSV
  trendsURL <- "www.google.com/trends/trendsReport?"
  resultsText <- getForm(trendsURL, .params = list(q = query, geo = geo, export = 1, hl = 'en'), curl = ch, .opts = list(verbose = F))
  
  print(resultsText)
  
  ## Sometimes we reach quota limit, in that case stop!
  if(any(grep("QUOTA", resultsText))){
    stop("Reached Google Trends quota limit! Please try again later.") 
  }
    
  resultsText = gFormatTrends(resultsText)
  
  resultsText$GEO = geo
  
  return(resultsText)
  
}
