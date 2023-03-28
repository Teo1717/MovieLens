edx_sample <- edx %>% slice_sample(n = 5)

# Function for testing sample
testing <- function(sample){
  variables_numeric <- names(edx)[sapply(edx, is.numeric)]
  variable_cat <- names(edx)[!sapply(edx, is.numeric)]
  testN <- sapply(variables_numeric, function(col){
    ks.test(edx[,col], sample[,col])$p.value
  }) 
  testC <- sapply(variable_cat, function(col){
    chisq.test(table(edx[,col]) %>% cbind(table(sample[,col])))$p.value
  }) 
  return(any(testC <= 0.05) | any(testN <= 0.05))
}

i <- testing(edx_sample)
j <- 0
while(isTRUE(i)){
  j <- j + 1
  edx_sample <- edx %>% slice_sample(n = 10000)
  i <- testing(edx_sample)
  print(paste("interation", j))
} 