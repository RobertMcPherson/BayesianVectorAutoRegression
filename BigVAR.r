#install.packages("BigVAR")
library(BigVAR)
library(reshape)
library(ggplot2)

# Fit a Basic VAR-L(3,4) on simulated data
data(Y)
T1=floor(nrow(Y)/3)
T2=floor(2*nrow(Y)/3)
m1=constructModel(Y,p=4,struct="Basic",gran=c(50,10),verbose=FALSE,T1=T1,T2=T2,IC=FALSE)
plot(m1)
results=cv.BigVAR(m1)
plot(results)
predict(results,n.ahead=1)

SparsityPlot.BigVAR.results(results)

str(results)
results@preds
results@alpha
results@Granularity
results@Structure
results@lagmax
results@Data

plot(results@Data)

########################################
df.melted <- melt(results@Data, id="x")

ggplot(data=df.melted, aes(x=X1, y=value, color=X2)) + 
  geom_line()

ggplot(data=df.melted, aes(x=X1, y=value, color=X2)) + 
  geom_line() + facet_grid(X2 ~ .)

df.with.pred <- rbind(results@Data, t(predict(results, n.ahead=1)))
df.melted.with.pred <- melt(df.with.pred, id="x")
ggplot(data=df.melted.with.pred, aes(x=X1, y=value, color=X2)) + 
  geom_line() + facet_grid(X2 ~ .)


#?cv.BigVAR
#?SparsityPlot.BigVAR.results
#?VarptoVar1MC