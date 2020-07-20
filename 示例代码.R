library(bkmr)
library(ggplot2)
rm(list = ls())

# X=read.csv("test1.csv")
# X=X[1:50,3:8]
# X=as.matrix(X)

set.seed(111)
dat <- SimData(n = 50, M = 6)
y <- dat$y
Z <- dat$Z
X <- dat$X


getwd()
# write.csv(X,file = "X.csv")
# write.csv(Z,file = "Z.csv")
# write.csv(y,file = "y.csv")
z1 <- seq(min(dat$Z[, 1]), max(dat$Z[, 1]), length = 20)
z2 <- seq(min(dat$Z[, 2]), max(dat$Z[, 2]), length = 20)
hgrid.true <- outer(z1, z2, function(x,y) apply(cbind(x,y), 1, dat$HFun))

res <- persp(z1, z2, hgrid.true, theta = 30, phi = 20, expand = 0.5, 
             col = "lightblue", xlab = "", ylab = "", zlab = "")

set.seed(111)
fitkm <- kmbayes(y = y, Z = Z, X = X, iter = 1000, verbose = FALSE, varsel = TRUE)

ExtractPIPs(fitkm)

pred.resp.univar <- PredictorResponseUnivar(fit = fitkm)


ggplot(pred.resp.univar, aes(z, est, ymin = est - 1.96*se, 
                             ymax = est + 1.96*se)) + 
  geom_smooth(stat = "identity") + 
  facet_wrap(~ variable) +
  ylab("h(z)")

#add  xmin&xmax
ggplot(pred.resp.univar, aes(z, est, ymin = est - 1.96*se, 
                             ymax = est + 1.96*se,
                             xmin=-5,
                             xmax=5)) + 
  geom_smooth(stat = "identity") + 
  facet_wrap(~ variable) +
  ylab("h(z)")
## xmax and xmin must exceed the range of X given by the data. 
##If not, the setting is invalid and follows the situation of real data

pred.resp.bivar <- PredictorResponseBivar(fit = fitkm, min.plot.dist = 1)

ggplot(pred.resp.bivar, aes(z1, z2, fill = est)) + 
  geom_raster() + 
  facet_grid(variable2 ~ variable1) +
  scale_fill_gradientn(colours=c("#0000FFFF","#FFFFFFFF","#FF0000FF")) +
  xlab("expos1") +
  ylab("expos2") +
  ggtitle("h(expos1, expos2)")

pred.resp.bivar.levels <- PredictorResponseBivarLevels(
  pred.resp.df = pred.resp.bivar, 
  
  Z = Z, qs = c(0.1, 0.5, 0.9))

ggplot(pred.resp.bivar.levels, aes(z1, est)) + 
  geom_smooth(aes(col = quantile), stat = "identity") + 
  facet_grid(variable2 ~ variable1) +
  ggtitle("h(expos1 | quantiles of expos2)") +
  xlab("expos1")

risks.singvar <- SingVarRiskSummaries(fit = fitkm, y = y, Z = Z, X = X, 
                                      qs.diff = c(0.25, 0.75), 
                                      q.fixed = c(0.25, 0.50, 0.75),
                                      method = "exact")

ggplot(risks.singvar, aes(variable, est, ymin = est - 1.96*sd, 
                          ymax = est + 1.96*sd, col = q.fixed)) + 
  geom_pointrange(position = position_dodge(width = 0.75)) + 
  coord_flip()


