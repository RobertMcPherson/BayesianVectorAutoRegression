#This is a function for the "Kelly's Criteria" method of cash management

#p = probability of a win, a gain, or avoiding a loss
#q = money odds or return on investment

Kellys <- function(p,q){
result <- (p*q+(p-1))/q
return(result)
}

#This example assumes a 401(k) plan where the employer requires 100%
#of an employee's investment be put into the company's stock,
#with a .95 probability that the company will stay in business: p=0.95,
# and an expected return on investment of .10: q=0.10.
#This example results in a recommendation that only 45% of the
#emplyee's investment be put into the company's stock plan.

Kellys(.95,.10)


#Example using generated values, so that range of values can be graphed
#Sample data that varies a vector, P while keeping Q constant
(P <- seq(.1,.99, by=.01))
(Q <- rep(.1,length(P)))
length(P)
length(Q)

(K <- Kellys(P,Q))

KbyP <- cbind(P,K)
colnames(KbyP) <- c("P","K")
KbyP

plot(K~P)

#Sample data that varies a vector, Q while keeping P constant
(Q <- seq(.1,5, by=.01))
(P <- rep(.6,length(Q)))
length(P)
length(Q)

(K <- Kellys(P,Q))

KbyQ <- cbind(Q,K)
colnames(KbyQ) <- c("Q","K")
KbyQ

plot(K~Q)

?runif
?seq
?colnames
