## ----setup, include=FALSE----------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ---- echo = FALSE-----------------------------------------------------------------------------------------------------------
databasket <- read.csv("ACB-STATS-21-22csv.csv", header=TRUE, sep = ";")
colnames(databasket)[1] = "Position"


## ---- echo = FALSE-----------------------------------------------------------------------------------------------------------
which(is.na(databasket) == "TRUE")


## ---- echo = FALSE-----------------------------------------------------------------------------------------------------------
head(summary(databasket))


## ---- echo = FALSE-----------------------------------------------------------------------------------------------------------
ncol(databasket)
nrow(databasket)


## ---- echo = FALSE-----------------------------------------------------------------------------------------------------------
databasket[databasket$Name == "Mirotic, Nikola", 1]


## ---- echo = FALSE-----------------------------------------------------------------------------------------------------------
databasket[databasket$Name == "Mirotic, Nikola", 1]


## ---- echo = FALSE-----------------------------------------------------------------------------------------------------------
databasket[1,"SHORTNAME"]


## ---- echo = FALSE-----------------------------------------------------------------------------------------------------------
databasket[1,"NAME"]


## ---- echo = FALSE-----------------------------------------------------------------------------------------------------------
databasket[databasket$Name == "Ubal, Agustin", "NAME"]


## ---- echo = FALSE-----------------------------------------------------------------------------------------------------------
databasket[databasket$Name == "Ubal, Agustin", "G"]


## ---- echo = FALSE-----------------------------------------------------------------------------------------------------------
databasket[databasket$Name == "Ubal, Agustin", "MIN"]


## ---- echo = FALSE-----------------------------------------------------------------------------------------------------------
mean(databasket[,"MIN"])


## ---- echo = FALSE-----------------------------------------------------------------------------------------------------------
max(databasket[,"MIN"])


## ---- echo = FALSE-----------------------------------------------------------------------------------------------------------
which(databasket$MIN == 32.5)
databasket[9,"Name"]


## ---- echo = FALSE-----------------------------------------------------------------------------------------------------------
which(databasket$Name == "Ubal, Agustin")

databasket[c(9,11), c(14:22)]


## ---- echo = FALSE-----------------------------------------------------------------------------------------------------------
which(databasket$BS == 0.0)


## ---- echo = FALSE-----------------------------------------------------------------------------------------------------------
which(databasket$BS == 0.0 & databasket$MIN > 16)


## ---- echo = FALSE-----------------------------------------------------------------------------------------------------------
length(which(databasket$BS == 0.0 & databasket$MIN > 16))


## ---- echo = FALSE-----------------------------------------------------------------------------------------------------------
which(databasket$X3PM == 0.0)


## ---- echo = FALSE-----------------------------------------------------------------------------------------------------------
which(databasket$X3PM == 0.0 & databasket$Position == "C")


## ---- echo = FALSE-----------------------------------------------------------------------------------------------------------
databasket[149, "Name"]


## ---- out.width='50%', out.height='50%', echo = FALSE------------------------------------------------------------------------
knitr::include_graphics("SOMimage.png")


## ---- echo = FALSE-----------------------------------------------------------------------------------------------------------
datasom <- databasket[,-c(1:3,24:26)]


## ---- echo = FALSE-----------------------------------------------------------------------------------------------------------

scaled <- scale(datasom, scale = TRUE, center = TRUE)


## ---- echo = FALSE-----------------------------------------------------------------------------------------------------------
library(kohonen)
set.seed(1234)
firstsom <- som(scaled, somgrid(7,7,"hexagonal"), rlen = 100)


## ---- echo = FALSE-----------------------------------------------------------------------------------------------------------
plot(firstsom, type="changes")


## ---- echo = FALSE-----------------------------------------------------------------------------------------------------------
plot(firstsom, type="count", shape = "straight")


## ---- echo = FALSE-----------------------------------------------------------------------------------------------------------
numberofelements <- table(firstsom$unit.classif)
numberofelements


## ---- echo = FALSE-----------------------------------------------------------------------------------------------------------
library(kohonen)
coolBlueHotRed <- function(n, alpha = 1) {
  rainbow(n, end=4/6, alpha=alpha)[n:1]
}

par(mfrow=c(1,2))
plot(firstsom, type = "property", property = getCodes(firstsom)[,2], main=colnames(getCodes(firstsom))[2], palette.name=coolBlueHotRed)
plot(firstsom, type = "property", property = getCodes(firstsom)[,6], main=colnames(getCodes(firstsom))[6], palette.name=coolBlueHotRed)


## ---- echo = FALSE-----------------------------------------------------------------------------------------------------------
which(firstsom$unit.classif == 10)


## ---- echo = FALSE-----------------------------------------------------------------------------------------------------------
databasket[c(11,14,46,64,85,158,222,271,285),"Name"]


## ---- echo = FALSE-----------------------------------------------------------------------------------------------------------
databasket[c(11,14,46,64,85,158,222,271,285),"MIN"]


## ---- echo = FALSE-----------------------------------------------------------------------------------------------------------
databasket[c(11,14,46,64,85,158,222,271,285),"RNK"]


## ---- echo = FALSE, warning = FALSE, message=FALSE---------------------------------------------------------------------------
library("factoextra")
fviz_nbclust(na.omit(datasom),kmeans,method="wss",k.max=10)


## ---- echo = FALSE-----------------------------------------------------------------------------------------------------------
fviz_nbclust(na.omit(datasom),kmeans,method="silhouette",k.max=10)


## ---- echo = FALSE-----------------------------------------------------------------------------------------------------------
kmeansbasket <- kmeans(datasom,centers=2,iter.max=1000,nstart=100)


## ---- echo = FALSE-----------------------------------------------------------------------------------------------------------
length(which(kmeansbasket$cluster == 1))
length(which(kmeansbasket$cluster == 2))


## ----------------------------------------------------------------------------------------------------------------------------
databasket[c(which(kmeansbasket$cluster == 1)), "RNK"]


## ---- echo = FALSE-----------------------------------------------------------------------------------------------------------
min(databasket[c(which(kmeansbasket$cluster == 1)), "RNK"])
max(databasket[c(which(kmeansbasket$cluster == 1)), "RNK"])
mean(databasket[c(which(kmeansbasket$cluster == 1)), "RNK"])


## ----------------------------------------------------------------------------------------------------------------------------
databasket[c(which(kmeansbasket$cluster == 2)), "RNK"]


## ---- echo = FALSE-----------------------------------------------------------------------------------------------------------
min(databasket[c(which(kmeansbasket$cluster == 2)), "RNK"])
max(databasket[c(which(kmeansbasket$cluster == 2)), "RNK"])
mean(databasket[c(which(kmeansbasket$cluster == 2)), "RNK"])


## ---- echo = FALSE-----------------------------------------------------------------------------------------------------------
plot(databasket$MIN, databasket$PTS, xlab = "Minutes played", ylab = "Average points")


## ---- echo = FALSE, message=FALSE--------------------------------------------------------------------------------------------
plot(databasket$MIN, databasket$PTS, xlab = "Minutes played", ylab = "Average points") +
abline(lm(PTS ~ MIN, data = databasket))


## ---- echo = FALSE-----------------------------------------------------------------------------------------------------------
datareg <- databasket[,-c(1:3,25:26)]


## ---- echo = FALSE-----------------------------------------------------------------------------------------------------------
model1 <- lm(RNK ~., data = datareg)


## ---- echo = FALSE-----------------------------------------------------------------------------------------------------------
summary(model1)


## ---- echo = FALSE-----------------------------------------------------------------------------------------------------------
anova(model1)


## ---- echo = FALSE-----------------------------------------------------------------------------------------------------------
plot(model1, 1)


## ---- echo = FALSE-----------------------------------------------------------------------------------------------------------
training <- datareg[1:250,]
test <- datareg[251:310,]


## ---- echo = FALSE-----------------------------------------------------------------------------------------------------------
model2 <- lm(RNK ~., data = datareg)


## ---- echo = FALSE-----------------------------------------------------------------------------------------------------------
prediction <- predict(model2, newdata = test)
prediction


## ---- echo = FALSE-----------------------------------------------------------------------------------------------------------
Residuals <- prediction - test[,20]
sort(Residuals)


## ---- echo = FALSE-----------------------------------------------------------------------------------------------------------
mean(Residuals)
databasket[290,]


## ---- include = FALSE, message=FALSE, warning=FALSE--------------------------------------------------------------------------
library(h2o)
h2o.init(nthreads = -1)


## ---- echo = FALSE-----------------------------------------------------------------------------------------------------------
datah2o <- h2o.importFile(path = "./ACB-STATS-21-22csv.csv", sep = ";", header = TRUE, destination_frame = "datah2o")


## ---- echo = FALSE-----------------------------------------------------------------------------------------------------------
colnames(datah2o)[1] = "Position"
head(h2o.colnames(datah2o))
h2o.dim(datah2o)


## ---- echo = FALSE-----------------------------------------------------------------------------------------------------------
head(h2o.describe(datah2o))


## ---- echo = FALSE-----------------------------------------------------------------------------------------------------------
#index <- h2o.columns_by_type(object = datah2o, coltype = "numeric")
h2o.cor(x = datah2o[, c("PTS","FTM","FGM","3PM")], y = NULL, method = "Pearson", na.rm = TRUE)


## ---- echo = FALSE-----------------------------------------------------------------------------------------------------------
group <- h2o.splitFrame(data = datah2o, ratios = c(0.8), seed = 1234)
trainh2o <- h2o.assign(data = group[[1]], key = "trainh2o")
testh2o <- h2o.assign(data = group[[2]], key = "testh2o")


## ---- echo = FALSE, warning = FALSE, message=FALSE---------------------------------------------------------------------------

#Respuesta de train y test a numérica
trainh2o$RNK <- h2o.asnumeric(trainh2o$RNK)
testh2o$RNK <- h2o.asnumeric(testh2o$RNK)

#Respuesta y predictores
response <- "RNK"
predictors <- setdiff(h2o.colnames(datah2o), response)

#Modelo
model_h2o <- h2o.glm(y = response, x = predictors, training_frame = trainh2o, family = "gaussian")


## ---- echo = FALSE, message=FALSE--------------------------------------------------------------------------------------------
#Predicciones
predictionh2o <- h2o.predict(model_h2o, newdata = testh2o)

#Testh2o a dataframe
test2 <- as.data.frame(testh2o)

#Predicciones a dataframe
predicts <- as.data.frame(predictionh2o)

residuals <- test2[,"RNK"] - predicts


## ---- echo = FALSE, warning=FALSE, message=FALSE-----------------------------------------------------------------------------
gbmmodel <- h2o.gbm(y = response, x = predictors, training_frame = trainh2o, distribution = "gaussian")


## ---- echo = FALSE, warning = FALSE, message=FALSE---------------------------------------------------------------------------
#Predicciones
predictiongbm <- h2o.predict(gbmmodel, newdata = testh2o)

#Testh2o a dataframe
test2 <- as.data.frame(testh2o)

#Predicciones a dataframe
predicts2 <- as.data.frame(predictiongbm)

residuals2 <- test2[,"RNK"] - predicts2

