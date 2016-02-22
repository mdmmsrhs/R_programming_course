T.test <- function(y1,y2) {
  n1 <- length(y1) # number of elemnts in y1
  n2 <- length(y2) # number of elemnts in y2
  y.bar1 <- sum(y1)/n1
  y.bar2 <- sum(y2)/n2
  s1 <-  sqrt(sd(y1))
  s2 <- sqrt(sd(y2))
  s.sq <- (((n1 - 1)*(s1^2)) + ((n2 - 1)*(s2^2))) / ((n1 - 1) + (n2 -1))
  s <- sqrt(s.sq)
  t <- (y.bar1 - y.bar2) / (s * sqrt((1 / n1) + (1 / n2)))
  p <- 2*pt(-abs(t),df=(length(test1)+length(test2))-1)
  out <- c(t, p)
  return(out) # return is optional but left here for clarity
}

test1 <- c(10,20,30,40,50)
test2 <- c(1,2,3,4,5)
(T.test(test1,test2))
#(p <- 2*pt(-abs(t),df=(length(test1)+length(test2))-1))

