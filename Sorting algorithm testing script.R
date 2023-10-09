

testarray <- round(runif(n=10000, min=1, max=20))
testarray
###################################################bubble sort#####################################################
bubble_sort_time = function(array) {
  start.time <- Sys.time()
  count = 0
  while(1) {
    count_swaps = 0
    for (j in 1 : (length(array) - 1 - count)) {
      if (array[j] > array[j + 1]) {
        s = array[j]
        array[j] = array[j+1]
        array[j+1] = s
        count_swaps = count_swaps + 1
      }
    }
    count = count + 1
    if(count_swaps == 0) break
  }
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  time.taken
  
}

bubble_sort = function(array) {
  #start.time <- Sys.time()
  count = 0
  while(1) {
    count_swaps = 0
    for (j in 1 : (length(array) - 1 - count)) {
      if (array[j] > array[j + 1]) {
        s = array[j]
        array[j] = array[j+1]
        array[j+1] = s
        count_swaps = count_swaps + 1
      }
    }
    count = count + 1
    if(count_swaps == 0) break
  }
  array
 # end.time <- Sys.time()
 # time.taken <- end.time - start.time
 # time.taken
  
}
bubble_sort_time(testarray)
bubble_sort(testarray)
################################################Selection_Sort##################################################

selsr_time <- function(x) {
  # Selection sort a vector
  start.time <- Sys.time()
  n <- length(x)
  
  for (i in 1:(n - 1)) {
    j <- i + which.min(x[(i + 1):n])
    if (j != i) {
      temp <- x[i]
      x[i] <- x[j]
      x[j] <- temp
    }
  }
  x
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  time.taken
}

selsr <- function(x) {
  # Selection sort a vector
 # start.time <- Sys.time()
  n <- length(x)
  
  for (i in 1:(n - 1)) {
    j <- i + which.min(x[(i + 1):n])
    if (j != i) {
      temp <- x[i]
      x[i] <- x[j]
      x[j] <- temp
    }
  }
  x
  #end.time <- Sys.time()
  #time.taken <- end.time - start.time
  #time.taken
}


#system.time(result <- selsr(testarray))
selsr_time(testarray)
selsr(testarray)
##############################################quicksort######################################################

qs_time <- function(vec) {
  start.time <- Sys.time()
  if(length(vec) > 1) {
    
    pivot <- vec[1]
    
    low <- qs(vec[vec < pivot])
    mid <- vec[vec == pivot]
    high <- qs(vec[vec > pivot])
    
    c(low, mid, high)
    
    
  }
  
  else vec
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  time.taken
}

qs <- function(vec) {
  #start.time <- Sys.time()
  if(length(vec) > 1) {
    
    pivot <- vec[1]
    
    low <- qs(vec[vec < pivot])
    mid <- vec[vec == pivot]
    high <- qs(vec[vec > pivot])
    
    c(low, mid, high)
    
    
  }
  
  else vec
  #end.time <- Sys.time()
  #time.taken <- end.time - start.time
  #time.taken
}
qs_time(testarray)
qs(testarray)

testarray

############################################ANOVA ANALYSIS######################################


interaction.plot()



