## Description here
readBlock = function(block){
  con = textConnection(block)
  txt = read.csv(con, sep="\n")
  
  tt = str_split(as.character(txt[,1]), ",")
  df = data.frame(t(sapply(tt[2:length(tt)],c)))
  
  names(df) = unlist(tt[1])
  
  ## Could use names(txt) to name the data frame
  
  return(df)
  
}