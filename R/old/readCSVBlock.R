readCSVBlock <-
function(txt)
{
  con = textConnection(txt)
  on.exit(close(con))
  read.csv(con, row.names=NULL)  
}
