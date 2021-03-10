
Design.1 <- FrF2(nruns= NULL ,nfactors= 2 , blocks= 1 , alias.block.2fis = 
  FALSE , ncenter= 0 , MaxC2 = FALSE , resolution = 5 ,replications= 4 ,
  repeat.only= FALSE ,randomize= TRUE ,seed= 26482 , factor.names=list( 
  Método=c("Manual","Spray"),Tipo=c("A","B") ) )
## creator element of design.info will be different, when using the command line command!
print( Design.1 , std.order=TRUE)
summary( Design.1 , brief = TRUE)
Design.1 <- add.response(Design.1,resposta, replace=FALSE)
LinearModel.1 <- lm(resposta ~ (Método + Tipo)^2, data=Design.1)
summary(LinearModel.1)
print( Design.1 )
Design.1 <- FrF2(nruns= NULL ,nfactors= 2 , blocks= 1 , alias.block.2fis = 
  FALSE , ncenter= 0 , MaxC2 = FALSE , resolution = 5 ,replications= 4 ,
  repeat.only= FALSE ,randomize= FALSE ,seed= 24044 , factor.names=list( 
  Método=c("Manual","Spray"),Tipo=c("A","B") ) )
## creator element of design.info will be different, when using the command line command!
Design.1 <- add.response(Design.1,resposta, replace=FALSE)
print( Design.1 , std.order=TRUE)
print( Design.1 )
Design.1 <- add.response(Design.1,resposta, replace=TRUE)
print( Design.1 )
LinearModel.2 <- lm(resposta ~ (Método + Tipo)^2, data=Design.1)
summary(LinearModel.2)
DanielPlot(Design.1, code=TRUE, autolab=TRUE, alpha=0.05, half=TRUE, 
  response="resposta")
DanielPlot(Design.1, code=TRUE, autolab=TRUE, alpha=0.1, half=TRUE, 
  response="resposta")
MEPlot(Design.1, abbrev=4, select=c(), response="resposta")
DanielPlot(Design.1, code=TRUE, autolab=TRUE, alpha=0.1, half=FALSE, 
  response="resposta")
DanielPlot(Design.1, code=FALSE, autolab=FALSE, alpha=0.1, half=FALSE, 
  response="resposta")
library(MASS, pos=28)
Confint(LinearModel.2, level=0.95)
summary( Design.1 , brief = TRUE)
Design.1 <- col.remove( Design.1 , c( "Blocks" ))
temp <-  Design.1 ; response.names(temp) <- NULL; plot(temp, select = c( 
  "Método","Tipo" )); rm(temp)
table( Design.1 [, c( "Método","Tipo" )])
print( Design.1 )
Design.1 <- FrF2(nruns= 4 ,nfactors= 2 , blocks= 1 , alias.block.2fis = 
  FALSE , ncenter= 0 , MaxC2 = FALSE , resolution = NULL ,replications= 1 ,
  repeat.only= FALSE ,randomize= FALSE ,seed= 6593 , factor.names=list( 
  Método=c("Manual","Spray"),Tipo=c("A","B") ) )
## creator element of design.info will be different, when using the command line command!
print( Design.1 )
Design.1 <- FrF2(nruns= 16 ,nfactors= 2 , blocks= 1 , alias.block.2fis = 
  FALSE , ncenter= 0 , MaxC2 = FALSE , resolution = NULL ,replications= 1 ,
  repeat.only= FALSE ,randomize= FALSE ,seed= 11314 , factor.names=list( 
  Método=c("Manual","Spray"),Tipo=c("A","B") ) )
## creator element of design.info will be different, when using the command line command!
print( Design.1 )
Design.1 <- add.response(Design.1,resposta, replace=FALSE)
LinearModel.3 <- lm(resposta ~ (Método + Tipo)^2, data=Design.1)
summary(LinearModel.3)
DanielPlot(Design.1, code=FALSE, autolab=TRUE, alpha=0.1, half=FALSE, 
  response="resposta")
DanielPlot(Design.1, code=FALSE, autolab=TRUE, alpha=0.1, half=TRUE, 
  response="resposta")
editDataset(Design.1)
MEPlot(Design.1, abbrev=4, select=c(), response="resposta")
IAPlot(Design.1, abbrev=4, show.alias=FALSE, select=c())
DanielPlot(Design.1, code=TRUE, autolab=TRUE, alpha=0.1, half=TRUE, 
  response="resposta")
DanielPlot(Design.1, code=FALSE, autolab=TRUE, alpha=0.2, half=TRUE, 
  response="resposta")
DanielPlot(Design.1, code=FALSE, autolab=TRUE, alpha=0.2, half=FALSE, 
  response="resposta")
DanielPlot(Design.1, code=FALSE, autolab=TRUE, alpha=0.5, half=FALSE, 
  response="resposta")
DanielPlot(Design.1, code=FALSE, autolab=TRUE, alpha=0.1, half=FALSE, 
  response="resposta")
summary(Design.1)
local({
  .Table <- xtabs(~ Método , data= Design.1 )
  cat("\nFrequency counts (test is for first level):\n")
  print(.Table)
  prop.test(rbind(.Table), alternative='two.sided', p=.5, conf.level=.95, 
  correct=FALSE)
})
with(Design.1, Dotplot(resposta, bin=FALSE))
Boxplot( ~ resposta, data=Design.1, id=list(method="y"))
LinearModel.4 <- lm(resposta ~ (Método + Tipo)^2, data=Design.1)
summary(LinearModel.4)

