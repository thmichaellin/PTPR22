
#Data Cleaning
cleanData <- function(data){
  data %>% 
    mutate(deltaLSAS = LSAS.SR.post - LSAS.SR.pre) %>% 
    mutate(deltaLSAS = as.numeric(deltaLSAS))%>% 
    mutate(LSAS.SR.pre = as.numeric(LSAS.SR.pre)) %>%
    mutate(LSAS.SR.post = as.numeric(LSAS.SR.post)) %>%
    mutate(Age = as.numeric(Age)) %>%
    arrange(Condition)
  #Adjusted Mean for Plot
  meanPrePost <- matrix(nrow = nrow(data), ncol = 1)
  for (i in 1:nrow(data)) {
    lsasPre <- as.numeric(data %>% select(LSAS.SR.pre) %>% slice(i))
    lsasPost <- as.numeric(data %>% select(LSAS.SR.post) %>% slice(i))
    meanPrePost[i,1] <- mean(c(lsasPre, lsasPost))
  }
  meanPrePostCond <- matrix(nrow = nrow(data), ncol = 1)
  for (i in 1:nrow(data)) {
    if (data[i,]$Condition == 0 ){
      meanPrePostCond[i,] <- mean(meanPrePost[data$Condition == 0])  
    } else if (data[i,]$Condition == 1 ){
      meanPrePostCond[i,] <- mean(meanPrePost[data$Condition == 1])  
    } else if (data[i,]$Condition == 2 ){
      meanPrePostCond[i,] <- mean(meanPrePost[data$Condition == 2])  
    }
  }
  
  adjustment <- meanPrePostCond - meanPrePost
  data %>% mutate(Adjustment = adjustment) %>% 
    mutate(PreAdj = LSAS.SR.pre + Adjustment) %>%
    mutate(PostAdj = LSAS.SR.post + Adjustment)%>%
    mutate(Condition = as.factor(Condition)) 
  }

