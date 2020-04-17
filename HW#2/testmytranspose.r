source('mytranspose.r')

#1
Test1 <- function(){
  myvar <- matrix(1:10, nrow=5, ncol=2)
  expected <- 0
  result <- sum(t(myvar) != mytranspose(myvar))
  
  return(all.equal(expected, result))
}


#2
Test2 <- function(){
  myvar <- matrix(NA, nrow=0, ncol=0)
  expected <- NULL
  result <- mytranspose(myvar)
  
  return(all.equal(expected, result))
}


#3
Test3 <- function(){
  myvar <-  matrix(c(1,2), nrow=1, ncol=2)
  expected <- 0
  result <- sum(t(myvar) != mytranspose(myvar))
  
  return(all.equal(expected, result))
}


#4
Test4 <- function(){
  myvar <- matrix(c(1,2), nrow=2, ncol=1)
  expected <- 0
  result <- sum(t(myvar) != mytranspose(myvar))
  
  return(all.equal(expected, result))
}


#5
Test5 <- function(){
  myvar <- c(1,2,NA,3)
  expected <- t(matrix(myvar))
  Result <- mytranspose(myvar)
  result <- sum(expected[!is.na(expected)] != Result[!is.na(Result)])
  
  return(all.equal(0, result))
}


#6
Test6 <- function(){
  myvar <- c(NA)
  expected <- 0
  result <- sum(t(matrix(myvar)) != mytranspose(myvar))
  
  return(all.equal(expected, result))
}


#7
Test7 <- function(){
  myvar <-  c()
  expected <- 0
  result <- sum(NULL != mytranspose(myvar))
  
  return(all.equal(expected, result))
}


#8
Test8 <- function(){
  d <- c(1,2,3,4)
  e <- c("red", "white", "red", NA)
  f <- c(TRUE,TRUE,TRUE,FALSE)
  myvar <- data.frame(d,e,f)
  expected <- t(myvar)
  Result <- mytranspose(myvar)
  result <- sum(expected[!is.na(expected)] != Result[!is.na(Result)])
  
  return(all.equal(0, result))
}



#Test
print(Test1())
print(Test2())
print(Test3())
print(Test4())
print(Test5())
print(Test6())
print(Test7())
print(Test8())
