rm(list = ls())
vec <- readRDS("c:/Users/Filoche/Desktop/vec.rds")


res <-   lapply(3:length(vec), function(i)
    read.csv(
      textConnection(strsplit(vec[i], "\\\n")[[1]]),
      skip = 1,
      stringsAsFactors = FALSE
    ))

res1 <- res[2]

aggregate(regions[regions$Name %in% res1[[1]][[1]], ],
          by=list('Target.Type'), 
          FUN=count)

## Test
data(regions, envir = environment())

which_type <- function(res){
  
  aggregate(regions[regions$Name %in% res[[1]], ],
                    by=list('Target.Type'), 
                    FUN=count)

  
  
  tmp <- regions[regions$Name %in% res[[1]], ] %>% 
    group_by(Target.Type) %>% 
    summarise(n = n())
  
  found <- tmp$n == length(res[[1]])
  
  if(any(found)){
    return(tmp$Target.Type[found])
  }else{
    return(tmp$Target.Type[which.max(tmp$n)])
  }
    
}

r <- lapply(res, which_type)




