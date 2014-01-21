gFormatTrends2 <-
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
      
  nblock = length(blocks)
  
  gDataFormated = list(nblock)
  
  for(i in 2:(nblock-1)){
    gDataFormated[i-1] = list(readBlock(blocks[i]))
  }
  
  
  class(gDataFormated) = "gTrendsData"
  
  return(gDataFormated)
  
}
