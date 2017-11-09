############################################################################################################################################
## Code Purpose: Helper Functions
########################################################################################################################################

### >>>>>>>>>>>> Setting wd & loading packages

#setwd("C:/Users/ap13970/Desktop/Data required for analysis")

library(caret) # Splitting and  Confusion Matrix
library(Rgraphviz) # Glmtree Plotting
library(partykit) # Glmtree function
library(rBayesianOptimization) #Bayesian Optimization
library(ROCR) # ROC Curve
#library(plyr)
library(pROC)
library(data.table)
library(Hmisc)
library(plm) # Hauman Test
library(GGally) # EDA plots
library(car)
library(grid) # Multi grid plots and sharing legends for  Effectiveness vs Exposure plots
library(corrplot) # Correlation Matrix Plot
library(gridExtra) # Multi grid plots and sharing legends for  Effectiveness vs Exposure plots
library(foreach) # Parallel Processing
library(doParallel) # Parallel Processing
library(doRNG)  # Ensuring Reproducibility in parallel processing
library(devtools)
library(bindr) 
library(dplyr) # Data manipulation
library(lazyeval) # Lazy load of data in dplyr
library(gtools) # For permutation
library(sigmoid) # For sigmoid function

set.seed(12345)
#oldw <- getOption("warn")
options(warn = -1)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% GENERAL PURPOSE FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



### >>>>>>>>>>>> Normalizing Functions -----------------------------------

range01 <- function(x){(x-mean(x))/(sd(x))}
range02<- function(x){(x-min(x))/(max(x)-min(x))}



### >>>>>>>>>>>> Function to calculate Sensitivity & Specificity ---------------------------------------

#' Sens.and.spec.calculator.
#' @param cut Threshold value for BINARY classification.
#' @param predictions A vector containing predicted values of the dependenet variable.
#' @param actuals A vector containing actual values of the dependenet variable.
#'
#' @return Sensitivity & Specificity at given cut off value
#' @return A matrix (of dimensions 1 row * 2 columns),  containing sensitivity value and specificity value respectively.
#' @examples
#'
#' # Creating a vector of actuals
#' act <- sample(c(0,1), replace=TRUE, size = 100)
#'
#' # Creating a vector of predictions
#' pred <- runif(100, min=0, max=1)
#'
#' # Creating Sensitivity and Specificity at threshold = 0.5
#' Sens.and.spec.calculator(0.5, pred, act)
#'
#' @export
Sens.and.spec.calculator = function(cut, predictions, actuals)  {
  
  predicted.ones = (predictions>cut)
  predicted.ones.indices = which(actuals==1)
  sensitivity = mean( predicted.ones[predicted.ones.indices] == 1 )
  specificity = mean( predicted.ones[-predicted.ones.indices] == 0 )
  sens.and.spec = t(as.matrix(c(sensitivity, specificity)))
  colnames(sens.and.spec) = c("sensitivity", "specificity")
  return(sens.and.spec)
  
}



### >>>>>>>>>>>> Function for plotting univariate plots ---------------------------------------------------------


#' Univariate.plotter.
#' @description  Plots all variables against the response variable.
#'
#' @param data Dataset (containing the dependent & independent variables) which is to be analyzed.
#' @param response.variable Dependent Variable.
#'
#' @return List containing plots of all independent variables v/s the dependent variable. Plots box plots, scatter plots and stacked bar plots
#'         for categorical - continous, continous - continous  and categorical - categorical variables combinations respectively.
#' @examples
#'
#' #  Load US States Production Data from plm package
#' data("Produc", package = "plm")
#'
#' Univariate.plotter(data = Produc, response.variable = "unemp" )
#' @export
Univariate.plotter <- function(data, response.variable) {
  
  seq1 <- 1:ncol(data)
  seq1 <- seq1[which(! seq1 %in% which(colnames(data) == response.variable))]
  plot_list=list()
  for (i in seq1)
    local({
      i <- i
      if(! is.factor(data[,response.variable])) {
        if(is.factor(data[,i])) {
          p <-  ggplot() + geom_boxplot(data = data, mapping = aes_string(x = data[,i],
                                                                          y= noquote(response.variable))) +  ggtitle(paste0( colnames(data)[i]  )) +
            theme(plot.title = element_text(face = 'bold', hjust = 0.5,size = 10),
                  axis.text.x = element_text(size = 7), axis.title.x = element_blank(),
                  axis.text.y = element_text(size = 7), axis.title.y = element_text(size = 9))
        } else {
          p <-  ggplot() + geom_point(data = data, mapping = aes_string(x = data[,i],
                                                                        y= noquote(response.variable))) +  ggtitle(paste0(colnames(data)[i]  )) +
            theme(plot.title = element_text(face = 'bold', hjust = 0.5,size = 10),
                  axis.text.x = element_text(size = 7),axis.title.x = element_blank(),
                  axis.text.y = element_text(size = 7), axis.title.y = element_text(size = 9))
        }
      } else {
        if(! is.factor(data[,i])) {
          p <-  ggplot() + geom_boxplot(data = data, mapping = aes_string(y = data[,i],
                                                                          x = noquote(response.variable))) +  ggtitle(paste0(colnames(data)[i]  )) +
            theme(plot.title = element_text(face = 'bold', hjust = 0.5,size = 10),
                  axis.text.y = element_text(size = 7), axis.title.y = element_blank(),
                  axis.text.x = element_text(size = 7), axis.title.x = element_text(size = 9))
        } else {
          var <- colnames(data)[i]
          df <- as.data.frame(data %>%  group_by_(c(response.variable),var) %>% summarise(Count = n())) %>%
            mutate(freq = Count / sum(Count)) %>% mutate_at("freq", round, 2)
          df$perc <-  paste0(sprintf("%.0f", 100*df$freq), "%")
          
          p <-  ggplot(df, mapping = aes_string(noquote(response.variable),  noquote("Count"), fill = noquote(var),
                                                label = noquote("perc"))) + geom_bar(stat="identity") + geom_text(aes_string(label = noquote("perc")), size= 3,
                                                                                                                  position = position_stack(vjust = 0.5), color = "black") +  ggtitle(paste0(var)) +
            theme(plot.title = element_text(face = 'bold',  hjust = 0.5,size = 10), axis.text.y = element_text(size = 7),
                  axis.title.y = element_text(size = 9), axis.text.x = element_text(size = 7),
                  axis.title.x = element_text(size = 9))
        }
      }
      plot_list[[i]] <<- p
    })
  plot_list[sapply(plot_list, is.null)] <- NULL
  par(mfrow=c(4, 4))
  marrangeGrob(plot_list, nrow=4, ncol=4)
  
}



### >>>>>>>>>>>> Function for plotting correlation matrix ---------------------------------------------------------

#' Correlation.matrix.plotter.
#' @description  Plots correlation matrix for a given data.
#'
#' @param data Dataset for which correlation matrix is required.
#'
#' @return Correlation Matrix for all the numeric variables in the dataset
#' @examples
#'
#' #  Load US States Production Data from plm package
#' data("Produc", package = "plm")
#'
#' Correlation.matrix.plotter(data = Produc)
#'
#' @export
Correlation.matrix.plotter <- function(data) {
  
  is.fact <- sapply(data, is.factor)
  not_factors <- data[, !is.fact]
  cor.m <- cor(as.matrix(not_factors), method = c("pearson"))
  cex.before <- par("cex")
  par(cex = 0.7)
  corrplot(cor.m, title = 'Correlation Plot')
  
}



### >>>>>>>>>>>> Function for sharing legend -------------------------------------------------------



Grid.arrange.shared.legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {
  
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position="none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)
  
  combined <- switch(position,"bottom" = arrangeGrob(do.call(arrangeGrob, gl), legend,ncol = 1, heights = unit.c(unit(1, "npc") -
                                                                                                                   lheight, lheight)), "right" = arrangeGrob(do.call(arrangeGrob, gl), legend, ncol = 2,
                                                                                                                                                             widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  grid.newpage()
  grid.draw(combined)
  invisible(combined)
  
}



### >>>>>>>>>>>> Function to check NA values --------------------

#' Na.check.
#' @description  Checks for missing values in a dataset.
#'
#' @param data Dataset which is to be scrutinized for missing values.
#'
#' @return Returns NULL if no missing values are present. In case missing values are present, it returns the column names with
#'         corresponding counts of missing values.
#' @examples
#'
#' #  Load US States Production Data from plm package
#' data("Produc", package = "plm")
#'
#' Na.check (data = Produc)
#'
#' @export
Na.check <- function(data) {
  
  complete.list <- sapply(data, function(x) length(which(is.na(x))))
  if(length(complete.list[complete.list>0])==0) {
    cat("\n No NA values present in the data \n ")
    return(NULL)
    # complete.list <- c("\n No NA values present in the data \n")
  }
  else { cat("\n Following columns have NA values \n")
    return(complete.list[complete.list>0])
  }
  # return(complete.list[complete.list>0])
}



### >>>>>>>>>>>> Function to check zero SD --------------------

#' Zero.sd.check.
#' @description  Checks for zero variance for numeric columns in a dataset.
#'
#' @param data Dataset which is to be scrutinized for zero variance variables.
#'
#' @return Returns NULL if all numeric columns have non zero variance. In case there are numeric columns with zero variance,it
#'         returns the column names with suffix 1.
#' @examples
#'
#' #  Load US States Production Data from plm package
#' data("Produc", package = "plm")
#'
#' Zero.sd.check(data = Produc)
#'
#' @export
Zero.sd.check <- function(data){
  
  complete.list <- sapply(data, function(x) length(which(sd(x)==0)))
  if(length(complete.list[complete.list>0])==0) {
    cat("\n No variables have zero SD  in the data \n")
    return(NULL)
  }
  else { cat("\n Following columns have zero SD values \n")
    return(complete.list[complete.list>0])
  }
  
}



### >>>>>>>>>>>> Function to check Nan values --------------------

#' Nan.check
#' @description  Checks for 'NaN' values in a dataset.
#'
#' @param data Dataset which is to be scrutinized for NaN values.
#'
#' @return Returns NULL if no columns have 'NaN' values. In case there are  columns with 'NaN' values, it
#'         returns the column names with suffix 1.
#' @examples
#'
#' #  Load US States Production Data from plm package
#' data("Produc", package = "plm")
#'
#' Nan.check(data = Produc)
#'
#' @export
Nan.check <- function(data){
  
  complete.list <- sapply(data, function(x) sum(is.nan(as.matrix(x))))
  if(length(complete.list[complete.list>0])==0) {
    cat("\n No variables have NaN  in the data \n")
    return(NULL)
  }
  else { cat("\n Following columns have NaN values \n")
    return(complete.list[complete.list>0])
  }
  
}

### >>>>>>>>>>>> Hausman Test --------------------

#' Hausman.test.
#' @description  Checks for the type of effects (Fixed v/s Random) in panel data. This is analyzed prior to fitting a regression model.
#'
#' @param data Dataset on which regression model will be fitted.
#' @param response.variable Dependent Variable.
#' @param x.variables Independent variables on which the dependent variable will be regressed.
#' @param panel.variables Name of the variable which uniquely identifies set of records pertaining to an ID/Account.
#' @param panel.effects The effects introduced in the model, one of "individual", "time", or "twoways". By default it is 'individual'.
#'
#' @return P value and the hypothesis which follows (Null or Alternate)
#' @examples
#'
#' #  Load US States Production Data from plm package
#' data("Produc", package = "plm")
#'
#' Hausman.test(data = Produc, response.variable = "unemp", x.variables = c("pcap", "hwy",
#'  "water","util", "pc", "gsp","emp"), panel.variables = "state", panel.effects = "individual")
#'
#' @seealso \link[plm]{plm}
#' @author Sri Krishna Rao Achyutuni <srikrishnarao.achyutuni@zs.com>
#' @export
Hausman.test <- function(data, response.variable, x.variables, panel.variables, panel.effects = "individual",  ...) {
  
  formula.for.modeling=paste(response.variable,"~",paste(x.variables,collapse = " + "),sep=" ")
  plm.fixed <- plm(as.formula(formula.for.modeling), data = data,model = "within",
                   effect = panel.effects,index = panel.variables)
  plm.random <- plm(as.formula(formula.for.modeling), data = data,model = "random",
                    effect = panel.effects,index = panel.variables)
  haustest <- phtest(plm.fixed, plm.random)
  
  cat("\n P value :",haustest$p.value)
  
  if(haustest$p.value < 0.05) {
    cat("\n NULL HYPOTHESIS IS REJECTED - THE MODEL HAS FIXED EFFECTS! \n")
  }else{
    cat("\n NULL HYPOTHESIS IS ACCEPTED - THE MODEL HAS RANDOM EFFECTS! \n")
  }
}



### >>>>>>>>>>>> Breush Pagen Test --------------------


#' Breusch.pagan.test.
#' @description Checks for Heteroscedasticity. Generally speaking, this is analyzed after fitting a linear regression model.
#' 
#' @param data Dataset containing independent variables which were used for regression.
#' @param yhat  Predicted Value of Dependent Variable by the model.
#' @param residuals Residual values from a fitted model. (Y actual - Y predicted).
#' @param studentize Logical. Studentise = TRUE will use studenteized residuals. By default this is TRUE.
#'
#' @author Sri Krishna Rao Achyutuni <srikrishnarao.achyutuni@zs.com>
#' @return P value and the hypothesis which follows (Null or Alternate).
#' @note Studentized BP test is more robust than the original one.The asymptotic power of the Breusch Pagan test is extremely 
#'       sensitive to the kurtosis of the distribution of epsilon, and the asymptotic size of the test is correct only in 
#'       special case of Gaussian kurtosis.
#'
#' @examples
#'
#' #  Load US States Production Data from plm package
#' data("Produc", package = "plm")
#'
#' # Fit a glmtree model 
#' model <- Model.fitting(x.variables = c("pcap", "hwy", "water","util", "pc", "gsp","emp"),
#'          cutting.variables = "region",min.node.size = 50, response.variable = "unemp", data =
#'         Produc[Produc$year <=  max(Produc$year),], data.type = "Example")
#'
#' # Get residuals
#' actuals <- Produc$unemp[Produc$year ==  max(Produc$year)]
#' predicted <- predict(model$final.model, newdata= Produc[Produc$year ==  max(Produc$year),],
#'             type = "response" )
#'             
#' Breusch.pagan.test(data = Produc, yhat = predicted, residuals =  (actuals- predicted ))
#' @export
Breusch.pagan.test <- function (data, yhat, residuals, studentize = TRUE) {
  
  n <- nrow(data)
  sigma2 <- sum(residuals^2)/n
  if (studentize) {
    w <- residuals^2 - sigma2
    fv <-  yhat
    bp <- n * sum(fv^2)/sum(w^2)
    method <- "studentized Breusch-Pagan test"
  }
  else {
    f <- residuals^2/sigma2 - 1
    fv <- yhat
    bp <- 0.5 * sum(fv^2)
    method <- "Breusch-Pagan test"
  }
  names(bp) <- "BP"
  data <- ncol(data) - 1
  names(data) <- "data"
  RVAL <- list(statistic = bp, parameter = data, method = method,  p.value = pchisq(bp, data, lower.tail = FALSE))
  class(RVAL) <- "htest"
  
  cat("\n P value :",RVAL$p.value)
  
  if(RVAL$p.value < 0.05) {
    cat("\n NULL HYPOTHESIS IS REJECTED - Heteroskedascity Present !. Consider tranforming the response variable. \n")
  }else{
    cat("\n NULL HYPOTHESIS IS ACCEPTED - Heteroskedascity NOT Present ! \n")
  }
  
}



### >>>>>>>>>>>> Durbin Watson Test--------------------

#' Durbin.watson.test
#' @description Checks for Autocorrelation.This is analyzed after fitting a linear regression model.
#'
#' @param residuals Residual values from a fitted model. (Y actual - Y predicted)
#' @return Durbin Watson Statistic (d).
#' @note   To test for positive autocorrelation at significance alpha, the test statistic d is compared to lower and upper
#'          critical values (dL,alpha and dU,alpha):
#'
#'    '     If d < dL,alpha, there is statistical evidence that the error terms are positively autocorrelated.
#'          If d > dU,alpha, there is no statistical evidence that the error terms are positively autocorrelated.
#'          If dL,alpha < d < dU,alpha, the test is inconclusive.
#'
#'          Positive serial correlation is serial correlation in which a positive error for one observation increases the
#'          chances of a positive error for another observation.To test for negative autocorrelation at significance alpha,
#'          the test statistic (4 - d) is compared to lower and upper critical values (dL,alpha and dU,alpha):
#'
#'          If (4 - d) < dL,alpha, there is statistical evidence that the error terms are negatively autocorrelated.
#'          If (4 - d) > dU,alpha, there is no statistical evidence that the error terms are negatively autocorrelated.
#'          If dL,alpha < (4 - d) < dU,alpha, the test is inconclusive.
#'
#' @examples
#'
#' #  Load US States Production Data from plm package
#' data("Produc", package = "plm")
#'
#' # Fit a glmtree model
#' model <- Model.fitting(x.variables = c("pcap", "hwy", "water","util", "pc", "gsp","emp"),
#'          cutting.variables = "region",min.node.size = 50, response.variable = "unemp", data =
#'         Produc[Produc$year <=  max(Produc$year),], data.type = "Example")
#'
#' # Get residuals
#' actuals <- Produc$unemp[Produc$year ==  max(Produc$year)]
#' predicted <- predict(model$final.model, newdata= Produc[Produc$year ==  max(Produc$year),],
#'             type = "response" )
#' Durbin.watson.test(residuals =  (actuals- predicted ))
#'@export
Durbin.watson.test <- function(residuals){
  
  DW_p_value <- durbinWatsonTest(residuals)
  cat("\n Durbin Watson Statistic :",DW_p_value)
  
  cat(" \n The value of d always lies between 0 and 4, d = 2 indicates no autocorrelation. If the value is substantially less 
      than 2, there is evidence of positive serial correlation. If d > 2, there is evidence of negative serial correlation.")
}



### >>>>>>>>>>>> Outlier Removal Function --------------------


#' Outlier.remover
#' @description  Detects and removes outliers from a given dataset.
#'
#' @param data Dataset which is to be analyzed for outlier detection.
#' @param input.variables Variables to be tested for otlier Detection. These should be continous.
#' @param id.variable Name of the variable which uniquely identifies set of records pertaining to an ID/Account.
#' @param cut.off Threshold for classifying an observation as a outlier.
#' @param method.arg1 Method for outlier detection. It can take values  'sd' & 'boxplot'.If 'sd' is selected, then the bounds
#'                   (limiting value) for classifying an observatoin as an outlier are computed using standard deviation.
#'                   In case 'boxplot' is selected, the bounds are computed using the inter quantile range of the boxplot.
#' @param method.arg2 Selection of method for classifying observation as an outlier. It can take values  'all' & 'any'.If 'all'
#'                   is selected, then an observation is considered as an outlier if it is outside the specified bounds for all
#'                   the input variables. In case of 'any', an observation is considered as an outlier if it is outside the
#'                   specified bounds for any one of the input variables.
#'
#' @return Overwrites (invisibly) the data frame after removing the outliers (if 'Yes' was selected), in parent environment in which the
#'         function is being called. In case 'No' was selected, it does nothing.
#' @examples
#'
#' #  Load US States Production Data from plm package
#' data("Produc", package = "plm")
#'
#' Outlier.remover(data = Produc, input.variables = c("pcap", "hwy", "water","util", "pc", "gsp",
#' "emp"), id.variable = "state", cut.off = 3, method.arg1 = "sd", method.arg2 = "any")
#'
#' @export
Outlier.remover <- function(data, input.variables, id.variable, cut.off, method.arg1, method.arg2 ) {
  
  if(method.arg1 == "sd") {
    outlier_list <- sapply(data[,input.variables], function(x)  x-mean(x,na.rm=TRUE) > cut.off*sd(x,na.rm=TRUE))
  }
  
  if(method.arg1 == "boxplot") {
    outlier_list <- sapply(data[,input.variables],function(x)  x %in%  boxplot.stats(x, coef = cut.off )$out )
  }
  
  if(method.arg2 == "all") {
    outliers <- data[which(apply(outlier_list,1, function(x) all(x == T))),id.variable]
  }
  
  if(method.arg2 == "any") {
    outliers <- data[which(apply(outlier_list,1, function(x) any(x == T))),id.variable]
  }
  
  cat("\n \n Outliers identified:", length(outliers))
  cat(" \n Unique accounts/IDs identified:", length(unique(outliers)))
  outliers.observations <- data[which( data[,id.variable] %in% outliers),]
  cat(" \n Total # outliers considering all time periods for an account/ID:",
      nrow(outliers.observations))
  
  # cat("\n Proportion (%) of outliers:", (length(outliers) / nrow(data))*100)
  cat("\n Proportion (%) of outliers:",
      paste0(round(  (nrow(outliers.observations) / nrow(data))*100 ,4),"%" ))
  cat("\n Mean of the outliers: \n")
  print(colMeans(data[which( data[,id.variable] %in% outliers),input.variables]))
  cat(" \n Mean without removing outliers: \n")
  print(colMeans(data[,input.variables]))
  cat(" \n Mean if we remove outliers:\n")
  print(colMeans(data[which(! data[,id.variable] %in% outliers),input.variables]))
  cat(" \n Contribution of the outliers (proprtion) : \n")
  print( colSums(data[which( data[,id.variable] %in% outliers),input.variables])/
           colSums(data[,input.variables]))
  
  response <- readline(prompt="Do you want to remove outliers ? [yes/no]: ")
  if(response == "y" | response == "yes" | response == "Y"){
    data <- data[which(! data[,id.variable] %in% outliers),]
    assign(as.character(as.list(match.call())$data), data, envir = parent.frame()) #.GlobalEnv
    cat("\n Outliers successfully removed")
    return(invisible(data))
  } else{  cat("\n Nothing changed")
  }
}



### >>>>>>>>>>>> Transformation function--------------------


#' Feature.engineering
#' @description  Transform variables in a dataset.
#'
#' @param data Dataset in which the variables to be transformed are present.
#' @param input.variables Variables which are to be transformed.
#' @param transformations Transformation type desired. Choose from log.transform (log(1+x)), sqroot, cuberoot ,negative.exponential,
#'                        sigmoid & inverse. There is also provision of 'No.transformation' if no transformation is desired. This is
#'                         handy  in case all the variables of a dataset are included in the 'input.variables' argument, but transformation
#'                        is desired on a few of them only.
#'
#' @return An object of class data.frame. This is the transformed Data.
#' @examples
#'
#' #  Load US States Production Data from plm package
#' data("Produc", package = "plm")
#'
#' new.df <- Feature.engineering(data = Produc, input.variables = c("pcap", "hwy", "water",
#'          "util"), transformations = c("log.transform", "log.transform","negative.exponential",
#'          "sigmoid" ))
#'
#' @export
Feature.engineering <- function (data, input.variables, transformations)  {
  
  sqroot <- sqrt
  log.transform <- function(x) log(1+x)
  inverse <- function(x) 1/(1+x)
  negative.exponential <- function(x) exp(-x)
  cuberoot <- function(x) x**(1/3)
  No.transformation <- function(x) x
  
  if (length(input.variables) != length(transformations)) {
    stop("\n The # of variables and # transformations do not match \n")
  }
  
  data.with.var <- data[,input.variables]
  data.without.var <- data[,which(!colnames(data) %in% input.variables)]
  data.with.var <- as.data.table(data.with.var)
  funcs <-  transformations
  
  transformed.data <-  data.with.var[, lapply(.SD, function(u)
  { sapply(funcs, function(f) do.call(f,list(u)))})][, t(.SD)]
  
  X <- list()
  for (i in 1:dim(transformed.data)[1]) {
    X [[i]]<- t(transformed.data)[((nrow(data) * (i-1)) + 1):(nrow(data) * i),i]
  }
  transformed.data <- as.data.frame(t(do.call("rbind",X)))
  colnames(transformed.data) <- colnames(data.with.var)
  transformed.data <- cbind(transformed.data,data.without.var)
  if(length(input.variables)==1){
    colnames(transformed.data)[1]=input.variables
  }
  return(transformed.data)
  
}



### >>>>>>>>>>>> Function for Koyck Transformation  ----------------------------------


# Koyck.transform
# @description  Perform koyck Transformation on one variable.
#
# @param x.var Variable to be transformed.
# @param transformation.constant Value of the transformation constant (Lambda) used.
# @param no.of.time.periods Number of observations pertaining to a single ID.
#
# @return An object of class vector. This is the Transformed Variable
# @examples
#
# #  Load US States Production Data from plm package
# data("Produc", package = "plm")
#
# new.var <- Koyck.transform( x.var = Produc$pcap , transformation.constant = 0.2, no.of.time.periods = 17)
Koyck.transform <- function(x.var ,transformation.constant, no.of.time.periods) {
  
  y= vector()
  y <- x.var
  seq1 <- (1:length(x.var))[which(!(1:length(x.var)) %in%   seq(1, length(x.var),no.of.time.periods))]
  for (i in seq1) {
    y[i] =  x.var[i] + transformation.constant*y[i-1]
  }
  return (y)
}



### >>>>>>>>>>>> Function for Calculation of Optimal Lambda for adstock  ----------------------------------


# Optim.input
# @description Finds optimum adstock constant for Koyck transformation for one variable
#
# @param lambda.value Initial value of the transformation constant (Lambda) to be optimized.
# @param df.optim Dataset containing the variable on which koyck transformation is to be performed.
# @param variable The independent X variable on which koyck transformation is to be performed.
# @param ID.var Name of the ID.var for each record.
# @param Month.variable Name of the Variable containing the time series value.
# @param response.variable Dependent Variable.
# @param Months Number of observations pertaining to a single ID.
# @param return.data Logical. If TRUE data is returned with Koyck transformation on the given variable.If FALSE, the optimized
#                    (maximized) value of corelation between response variable and the independent variable is returned.
#
# @return An object of class 'data.frame' containing the transformed variable with optimized constant (Lambda).
# @examples
#
# #  Load US States Production Data from plm package
# data("Produc", package = "plm")
#
# new.df <- Optim.input( lambda.value = 0.2, df.optim = Produc, variable = "pcap", ID.var = "state", Month.variable = "year",
#                        response.variable = "unemp", Months = 17, return.data = T)
Optim.input <-   function(lambda.value = 0.25, df.optim, variable, ID.var, Month.variable, response.variable, Months,
                          return.data = F) {
  
  # Grouping data by ID.var and arranging by Month
  
  df.optim <- df.optim %>% group_by_(as.name(ID.var)) %>% arrange_(.dots = c(as.name(ID.var), Month.variable))# %>%
  df.optim <-  as.data.frame(df.optim)
  temp <- df.optim[, which(colnames(df.optim) == variable)]
  
  # Transformation
  
  varNames <- quos(temp, lambda.value, Months)
  df.optim[, which(colnames(df.optim) == variable)]  <- Koyck.transform(as.vector(temp), lambda.value, Months)
  
  # Calculating correlation
  
  response.values <-  df.optim[, which(colnames(df.optim) == response.variable)]
  if(return.data == F) {
    -cor(response.values, df.optim[,which(colnames(df.optim) == variable)])
  } else {
    return(df.optim)
  }
  
}



### >>>>>>>>>>>> Function for Adstock.transformation ----------------------------------

#' Adstock.transformation
#' @description  Performs adstock (Koyck) transformation on multiple variables with optimized constants for each variable.
#'               This assumes that the dataset is balanced i.e. number of records (rows) are same for each unique ID/Account.
#'
#' @param data Dataset containing the variables on which koyck transformation is to be performed.
#' @param y.variable Dependent Variable.
#' @param variables.list Variables on which Koyck transformation is to be performed.
#' @param total.time.period Length of the time period. It should be equal to number of observations pertaining to a single ID.
#' @param id.variable Name of the variable which uniquely identifies set of records pertaining to an ID/Account.
#' @param time.period.id Name of the variable containing the time series values. These can be anything, example - weeks, months, years
#'                       etc., but should be non negative integers.
#' @param bounds.list List containing upper and lower bounds for transformation constant for each variable. By default the lower and
#'                   upper bounds are 0 and 1 respectively.
#' @param data.type Character describing which data is used (Train/Test/Validation).This is helpful when saving the results to
#'                 disk.
#'
#' @return A list containing the following elements:
#' \item{transformed.data}{Transformed data. An object of class 'data.frame' containing the transformed variable with optimized
#'                    constants (Lambda).}
#' \item{adstock.constants}{Optimum Value of transfomation constants (Lambdas) for each variable (in the order they were
#'  specified).}
#' @examples
#'
#' #  Load US States Production Data from plm package
#' data("Produc", package = "plm")
#'
#' new.df <- Adstock.transformation(data= Produc,  y.variable = "unemp",  variables.list = c("pcap",
#'          "hwy","water", "util", "pc", "gsp","emp"), total.time.period = 17, id.variable = "state",
#'          time.period.id = "year", data.type = "Example")
#'
#' @export
Adstock.transformation <- function(data,  y.variable, variables.list, total.time.period, id.variable, time.period.id,
                                   bounds.list = NULL, data.type){
  
  if( nrow(data) / length(unique(data[,id.variable])) !=   total.time.period  ) {
    stop(" \n The dataset is not balanced i.e. number of records (rows) are NOT same for each unique ID/Account.")
  }
  variables.list <- variables.list[which(!grepl("lag", variables.list)==T)]
  lambda.values <-  vector(mode = "numeric", length = length(variables.list))
  
  for (i in 1:length(variables.list)) {
    if(is.null(bounds.list)){
      lower.bound = 0
      upper.bound = 1
    } else {
      if(length(bounds.list) != length(variables.list)) {
        stop(" \n Length of bound list should be equal to number of variables to be transformed") }
      lower.bound = bounds.list[[i]][1]
      upper.bound = bounds.list[[i]][2]
    }
    
    # Getting optimized values of lambda
    
    lambda.values[i] <-  optim(0.25, Optim.input, df.optim = data, variable = variables.list[i], ID.var = id.variable,
                               Month.variable = time.period.id, response.variable = y.variable, Months =  total.time.period,
                               method =  "L-BFGS-B",  lower = lower.bound, upper = upper.bound)$par
    
    cat(" Lambda for ",variables.list[i],"for ", data.type," is : \t", lambda.values[i] , "\n")
    data <- Optim.input(lambda.value = lambda.values[i], df.optim = data, variable = variables.list[i],
                        ID.var = id.variable, Month.variable = time.period.id, response.variable = y.variable,
                        Months = total.time.period, return.data = T)
    
  }
  
  return(list(transformed.data = data, adstock.constants = lambda.values))
  
}



### >>>>>>>>>>>> Linear Regression Performance evaluator ----------------------------------------------

#' Linear.evaluation.
#' @description  Calculates the performance metrics for a Linear model.
#'
#' @param data Dataset (containing the dependent & independent variables) on which the performance has to be evaluated.
#' @param model.fitted Fitted model whose performance is to be evaluated.
#' @param response.variable Dependent Variable.
#' @param visuals Logical. Should visuals be generated (plots & graphs). By default it is TRUE.
#' @param save.results Logical. Should results be saved to disk. By default it is FALSE. If TRUE, a folder name has to be specified
#'                     in the 'folder.name' argument.
#' @param folder.name Used in case argument 'save.results' is set to TRUE. It is the name of the folder in which the results
#'                    are saved.
#' @param data.type Character describing which data is used (Train/Test/Validation).This is helpful when saving the results to
#'                 disk.
#'
#' @return A list containing the following elements:
#' \item{RMSE}{Root Mean Square Error.}
#' \item{Rsq}{Coefficient of Determination.}
#' @examples
#'
#' #  Load US States Production Data from plm package
#' data("Produc", package = "plm")
#'
#' # Fit a glmtree model
#' model <- Model.fitting(data = Produc[Produc$year <=  max(Produc$year),], response.variable =
#'         "unemp", x.variables = c("pcap", "hwy", "water","util", "pc", "gsp","emp"),
#'          cutting.variables = "region", min.node.size = 50,  data.type = "Example")
#'
#' Linear.evaluation(data = Produc[Produc$year <=  1975,], model.fitted = model$final.model,
#' response.variable = "unemp", data.type = "Example")
#'
#' @export
Linear.evaluation = function (data, model.fitted, response.variable, visuals = T, save.results = F,
                              folder.name = NULL, data.type)  {
  
  # Performance Metrics
  
  response.values <- data[,which(colnames(data) %in% response.variable)]
  predicted.values <- predict(model.fitted,data,type = "response") #unlist
  residual.values <- response.values - predicted.values
  RMSE.value <- sqrt(mean((residual.values)^2))
  mean.response <- mean(response.values)
  Rsq.value <- 1- ( sum((residual.values)^2) / sum(( mean.response- response.values)^2) )
  
  # Ploting
  
  if(visuals==T) {
    if(save.results==T) {
      jpeg(paste(filename=paste0("./",folder.name,"/GLMtree.performance.plot.", data.type,".", response.variable,".jpeg")),
           width = 2000, height = 1000,res = 100,quality=600,pointsize = 18)
    }
    par(mfrow=c(2,2))
    hist(residual.values, nclass=100, main = paste("Histogram of Residuals"))
    plot(residual.values,xlab = "Observations",ylab = "Residuals",main = paste("Residual Plot"))
    plot(predicted.values,residual.values,ylab = "Residuals",xlab = "Fitted Values",
         main = paste("Residuals v/s Fitted"))
    plot(response.values, predicted.values,ylab = "Fitted Values",xlab = "Actual Values",
         main = paste("Fitted v/s Actual"))
    #mtext(paste0("Diagnostic plots for ",data.type), side = 3, line = -2, outer = TRUE)
    if(save.results==T) {
      dev.off()
    }
  }
  
  # Writing to disk
  
  if(save.results==T){
    write.table(paste0("Performance Results for ",data.type),file=paste0("./",folder.name,"/Performance.Metrics.",
                                                                         response.variable,".csv"),col.names = F, row.names = F,append = T,sep=",")
    write.table(c("RMSE  :",RMSE.value),file=paste0("./",folder.name,"/Performance.Metrics.",response.variable,".csv"),
                col.names = F,row.names = F,append = T,sep=",")
    write.table(c("Rsq :",Rsq.value, "\n"),file=paste0("./",folder.name,"/Performance.Metrics.",response.variable,".csv"),
                col.names = F,row.names = F,append = T,sep=",")
  }
  
  return(list(RMSE = RMSE.value, Rsq = Rsq.value))
  
}



### >>>>>>>>>>>> Logistic Regression Performance evaluator ---------------------------------------------------------------

#' Logistic.evaluation.
#' @description  Calculates the performance metrics for a BINARY classifier
#'
#' @param data Data (containing the dependent & independent variables) on which the performance has to be evaluated.
#' @param model.fitted Fitted model whose performance is to be evaluated.
#' @param response.variable Dependent Variable.
#' @param threshold Threshold (cut off) value to classify the observations in each class with respect to their predicted
#'                  probability.
#' @param visuals Logical. Should visuals be generated (plots & graphs). By default it is TRUE.
#' @param save.results Logical. Should results be saved to disk. By default it is FALSE. If TRUE, a folder name has to be specified
#'                     in the 'folder.name' argument.
#' @param folder.name Used in case argument 'save.results' is set to TRUE. It is the name of the folder in which the results
#'                    are saved.
#' @param data.type Character describing which data is used (Train/Test/Validation). This is helpful when saving the results to
#'                  disk.
#'
#' @author Shivang Singhal <shivang.singhal@zs.com>
#' @return A list containing the following elements:
#' \item{AUC}{Area Under ROC curve.}
#' \item{Accuracy}{Accuracy value.}
#' \item{Confusion.Matrix}{Confusion matrix.}
#' \item{Threshold}{Returns the threshold supplied. In case no threshold was supplied, it returns the value at which the
#'                  difference between Specificty and Sensitivity values is minimum (on the dataset supplied in the 'data' argument.}
#' @examples
#'
#' #  Load US States Production Data from plm package
#' data("Produc", package = "plm")
#'
#' # Create a categorical variable
#' Produc$new.var <- as.factor(ifelse(Produc$unemp > median(Produc$unemp),1,0))
#'
#' # Fit a glmtree model
#' model <- Model.fitting(data = Produc[Produc$year <=  max(Produc$year),], response.variable = 
#'        "new.var", x.variables = c("pcap", "hwy", "water","util", "pc", "gsp","emp"), 
#'         cutting.variables = "region", min.node.size = 50,  data.type = "Example")
#'
#' Logistic.evaluation(data = Produc[Produc$year <=  1975,], model.fitted = model$final.model ,
#' response.variable = "new.var", data.type = "Example")
#' @export
Logistic.evaluation = function (data, model.fitted,  response.variable, threshold,  visuals = T, save.results = F,
                                folder.name = NULL ,data.type) {
  
  # Performance Metrics
  
  response.values <- as.factor(data[,which(colnames(data) %in% response.variable)])
  predicted.values <- predict(model.fitted,data,type = "response") #unlist
  
  ## Readjusting levels
  if(levels(response.values) == c("L" ,"H")){
    response.values <- revalue(response.values, c( "L" ="0", "H" = "1" ))
  }
  
  if(levels(response.values) == c("1" ,"2")){
    response.values <- revalue(response.values, c("1"="0", "2" = "1" ))
  }
  
  threshold.sequence = seq(.01,.99,length=100)
  sens.and.spec.matrix = matrix(0,100,4)
  for(q in 1:100) sens.and.spec.matrix[q,] <- Sens.and.spec.calculator(threshold.sequence[q],  as.numeric(as.character(predicted.values)),as.numeric(as.character(response.values))) 
  #Sens.and.spec.calculator(threshold.sequence[q],predicted.values,response.values)

  if(missing(threshold)) {
    threshold <- threshold.sequence[which.min(abs(sens.and.spec.matrix[,1]-sens.and.spec.matrix[,2]))]
  }
  
  predicted.classes <-ifelse(as.numeric(as.character(predicted.values)) > threshold ,1,0)
  mat<- confusionMatrix(reference=response.values, data=predicted.classes,mode = "prec_recall",positive = "1")
  AUC.value <- auc(as.numeric(as.character(response.values)), as.numeric(as.character(predicted.values)))
  Accuracy.value <- sum(diag(mat$table))/nrow(data)
  mat.transpose=t(mat$table)
  colnames(mat.transpose)=c("Predicted.0","Predicted.1")
  rownames(mat.transpose)=c("Actual.0","Actual.1")
  
  # Ploting
  predicted.values <- as.numeric(as.character(predicted.values))
  response.values <- as.numeric(as.character(response.values))
  
  if(visuals==T) {
    prediction.obj <- prediction(predicted.values, response.values )
    perf.train.auc <- round(as.numeric(performance(prediction.obj, "auc")@y.values), digits = 4)
    perf.train.roc <- performance(prediction.obj, "tpr", "fpr")
    perf.train.senspec <- performance(prediction.obj, "prec", "rec")
    pref.train.acc <- performance(prediction.obj, measure = "acc")
    if(save.results==T) {
      jpeg(paste(filename=paste0("./",folder.name,"/GLMtree.performance.plot.", data.type,".",response.variable,".jpeg")),
           width = 2000,height = 1000,res = 100,quality=600,pointsize = 18)
    }
    par(mfrow = c(2, 2))
    plot(sens.and.spec.matrix[,1], xlab = "Cutoff", ylab = "Value", cex.lab = 1, cex.axis = 0.7, ylim = c(0,1), type = "l", lwd = 2,
         axes = FALSE, col = 2, main = " Sensitivity v/s Specificity")
    axis(1, seq(0,1,length=5), seq(0,1,length=5), cex.lab=0.7)
    axis(2, seq(0,1,length=5), seq(0,1,length=5), cex.lab=0.7)
    lines(sens.and.spec.matrix[,2], col="darkgreen", lwd=2)
    box()
    
    plot(perf.train.roc, colorize = TRUE, print.cutoffs.at = seq(0.1, 1, 0.1),
         main = paste0("ROC Curve", " AUC:", perf.train.auc))
    plot(perf.train.senspec, colorize = TRUE, print.cutoffs.at = seq(0.1, 1, 0.1), main = " Precision-Recall Curve")
    plot(pref.train.acc, lwd = 3, main = "Accuracy vs CutOff")
    #mtext(paste0("Performance plots for ",data.type), side = 3, line = -2,  outer = TRUE)
    
    if(save.results==T) {
      dev.off()
    }
  }
  
  if(save.results==T){
    write.table(paste0("Performance Results for ",data.type),file=paste0("./",folder.name,"/Performance.Metrics.",
                                                                         response.variable,".csv"),col.names = F, row.names = F,append = T,sep=",")
    write.table(c("Optimum.cutoff.used.for.seperating.writers.v/s.non.writers :",threshold), file=paste0("./",folder.name,
                                                                                                         "/Performance.Metrics.",response.variable,".csv"), col.names = F,row.names = F,append = T,sep=",")
    write.table(c("Confusion matrix : "),file=paste0("./",folder.name,"/Performance.Metrics.", response.variable,".csv"),
                col.names = F,row.names = F,append = T,sep=",")
    write.table( as.matrix(mat.transpose,nrow=2),file=paste0("./",folder.name, "/Performance.Metrics.",response.variable,".csv"),
                 col.names = T,row.names = T, append = T,sep=",")
    write.table(c("AUC  :",AUC.value),file=paste0("./",folder.name,"/Performance.Metrics.", response.variable,".csv"),
                col.names = F,row.names = F,append = T,sep=",")
    write.table(c("Accuracy.value :",Accuracy.value, "\n"),file=paste0("./",folder.name, "/Performance.Metrics.",
                                                                       response.variable,".csv"),col.names = F,row.names = F, append = T,sep=",")
  }
  return(list(AUC = AUC.value, Accuracy = Accuracy.value, Confusion.Matrix = mat.transpose, Threshold = threshold))
  
}


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5%%%%%%%%%%%% ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% SUPPORTING FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5%%%%%%%%%%%%%%%%%%


### >>>>>>>>>>>> Function for plotting  Exposure v/s Effectiveness  ---------------------------------------------------------

#' Exposure.vs.effect.plotter.
#' @description  Plot the Exposure (Means) v/s Effectiveness (Betas) for each tactic (X variable) for each segment (Node).
#'
#' @param data.summary Model summary containing Means and Betas for each segment.
#' @param x.var Independent Variables used in regression.
#' @param color.var Variable by which the points  will be colored. This should be continous.
#' @param size.var Variable by which the points' size will be scaled. This should be continous.
#' @param label.var Variable by which the points will be labelled.
#'
#' @return A list containing Exposure (Means) v/s Effectiveness (Betas) for each tactic (X variable) for each segment (Node).
#' @examples
#'
#' #  Load US States Production Data from plm package
#' data("Produc", package = "plm")
#'
#' # Fit a glmtree model
#' model <- Model.fitting( x.variables = c("pcap", "hwy", "water","util", "pc", "gsp","emp"),
#'           cutting.variables = "region",  min.node.size = 50, response.variable = "unemp",
#'           data = Produc[Produc$year <=  max(Produc$year),], data.type = "Example")
#'
#' # Generate model summary
#' model.summary <- Summary.generator(data = Produc[Produc$year <=  max(Produc$year),], 
#'                  model.fitted = model, x.variables = c("pcap","hwy", "water","util", "pc", "gsp",
#'                  "emp"), response.variable = "unemp", data.type = "Example")
#'
#' plot(Exposure.vs.effect.plotter(data.summary = model.summary[[1]], x.var = c("pcap", "hwy", 
#'     "water","util", "pc", "gsp","emp"), color.var = "Contribution.y", size.var = "Mean.y",
#'      label.var = "Node"))
#'
#' @export
Exposure.vs.effect.plotter <- function(data.summary, x.var, color.var, size.var, label.var) {
  
  variables <- x.var
  df <-  data.summary[-nrow(data.summary),]
  betas <-colnames(df[,x.var])
  means <- colnames(df)[which(grepl("Mean", colnames(df))== T)]
  
  plot_list <- list()
  
  for ( i in 1 : length(variables))
    local({
      i <- i
      #    X = betas[which(grepl(variables[i], betas)==T)]
      #   Y = means[which(grepl(variables[i], means)==T)]
      X = betas[which(betas %in% variables[i])]
      Y = means[which(means %in% paste0("Mean.",variables[i]))]
      med.X <- median(df[,X])
      med.Y <- median(df[,Y])
      p <-  ggplot(df, mapping = aes_string(noquote(X), noquote(Y), size = noquote(size.var), color = noquote(color.var),
                                            label = noquote(label.var))) + geom_point() + geom_text(aes_string(label = noquote(label.var)), size= 5,
                                                                                                    color = "white") + scale_size_continuous(range = c(10,20)) +  scale_color_continuous(low = "#56B1F7",
                                                                                                                                                                                         high = "#132B43") + geom_hline(yintercept = med.Y) + geom_vline(xintercept = med.X) + xlab("Betas") + ylab("Means") +
        theme(plot.title = element_text(face = 'bold', hjust = 0.5 ,size = 14), axis.text.x = element_text(size = 10),
              axis.title.x = element_text(size = 12), axis.text.y = element_text(size = 10),
              axis.title.y = element_text(size = 12)) + ggtitle(paste0("Exposure v/s Effectiveness for  ", variables[i]))
      
      plot_list[[i]] <<- p
      
    })
  
  n <- length(plot_list)
  nCol <- ceiling(sqrt(n))
  plots.grid <- do.call("Grid.arrange.shared.legend", c(plot_list, ncol=nCol, nrow = ceiling(n/nCol)))
  return(plots.grid)
  
}



### >>>>>>>>>>>> Function to get Summary  -----------------------------------------------------------

#' Summary.generator.
#' @description Gets the detailed summary with impacts of a model on a dataset and the predicted values (Y and beta*X).
#'
#' @param data Dataset on which the summary (& Impacts) are desired.
#' @param model.fitted Model used for generating summary.
#' @param response.variable Dependent Variable.
#' @param x.variables Independent variables to be included for regression.
#' @param intercept Logical. Should the impacts of intercept be calculated. By default it is TRUE.
#' @param exposure.effectiveness Logical. If TRUE, Exposure v/s Effectiveness plots are generated. By default it is FALSE.
#' @param size.var Used in case argument 'exposure.effectiveness' is set to TRUE. It is the variable by which the points' size
#'                 will be scaled.  If this is a continous variable, ratio of sum of this variable for a segment divided by the total
#'                 sum of this variable (accross all segments) will be used. If this is a categorical variable (Binary), ratio of number
#'                 of values (in a segment) equal to the second level,   divided by the number of total values (in that segment) will be
#'                 used, for each segment. By default it is the response.variable.
#' @param color.var Used in case argument 'exposure.effectiveness' is set to TRUE. It is the variable by which the points  will be
#'                  colored. If this is a continous variable, average value of the variable for each segment will be used. If this is a
#'                  categorical variable (Binary), ratio of number of  values (in a segment) equal to the second level, divided by the
#'                  total number of values (accross all segments) equal to the second level. By default it is the response.variable.#' @param visualizations  Logical. Should visuals be generated (plots & graphs). By default it is TRUE.
#' @param visuals  Logical. Should visuals be generated (plots & graphs). By default it is TRUE.
#' @param save.results Logical. Should results be saved to disk. By default it is FALSE. If TRUE, a folder name has to  be specified in
#'                     the 'folder.name' argument.
#' @param folder.name Used in case argument 'save.results' is set to TRUE. It is the name of the folder in which the results are saved.
#' @param data.type Character describing which data is used (Train/Test/Validation).This is helpful when saving the results to
#'                 disk.
#'                 
#' @return A list containing the following elements:
#' \item{model.summary}{A detailed summary showing the coefficients, p-values, means, predictions and impactables for each
#'                       segment and at an overall level}
#' \item{predictions.data}{Input data with predicted values.}
#' @examples
#'
#' #  Load US States Production Data from plm package
#' data("Produc", package = "plm")
#'
#' # Fit a glmtree model
#' model <- Model.fitting(data = Produc[Produc$year <=  max(Produc$year),], response.variable = 
#'         "unemp", x.variables = c("pcap", "hwy", "water","util", "pc", "gsp","emp"),
#'          cutting.variables = "region", min.node.size = 50,  data.type = "Example")
#'
#'# Generate model sumary
#' model.summary <- Summary.generator(data = Produc[Produc$year <=  max(Produc$year),],
#'                   model.fitted = model, response.variable = "unemp",  x.variables = c("pcap",
#'                   "hwy", "water", "util", "pc", "gsp","emp"), data.type = "Example")
#'
#' @export
Summary.generator <- function (data, model.fitted, response.variable, x.variables,  intercept = T, exposure.effectiveness = F,
                               size.var , color.var ,visuals = T, save.results = F,  folder.name = NULL, data.type)
{
  
  variable.type <- ifelse(is.numeric(data[,which(colnames(data) %in% response.variable)])==T,
                          "continous","categorical")
  if(missing(size.var)) {
    size.var <- response.variable
  }
  
  if(missing(color.var)) {
    color.var <- response.variable
  }
  
  # Adding predictions to the  data
  
  predictions.data <- as.data.frame(cbind(data, "Predicted Value" = predict(model.fitted$final.model,data),
                                          "Node"= predict(model.fitted$final.model, data,type = "node")))
  
  coef.data <- coef(model.fitted$final.model)
  coef.data[(is.na(coef.data))] <- 0
  if(variable.type == "categorical") {
    coef.data <- exp(coef.data)
  }
  single.node = ifelse(is.null(nrow(coef.data)) == T, T, F)
  unique.nodes <- ifelse(single.node == F, length(unique(predictions.data$Node)), 1)
  
  # Generating empty summary
  
  model.summary <- as.data.frame(matrix(0,ncol = (4 + 5*length(x.variables) + 1 + 3),nrow = unique.nodes))
  x.variables.with.intercept=c("(Intercept)",x.variables)
  p.value.column <- paste0("P.value.",x.variables.with.intercept)
  Mean.value.column <- paste0("Mean.",x.variables.with.intercept)
  pred.columns <- paste0("Prediction.",x.variables.with.intercept)
  pred.columns.all <- c(pred.columns,"Prediction.from.model")
  perc.columns <- paste0("Percentage.Impact.",x.variables.with.intercept)
  
  colnames(model.summary)=c("rule","Observations",x.variables.with.intercept,p.value.column,
                            Mean.value.column,pred.columns.all,perc.columns)
  
  if (single.node == T) {
    model.summary$rule=1
  } else {
    model.summary$rule= sort(as.numeric(unique(predictions.data$Node)))
  }
  
  summary.model.fitteds <- summary(model.fitted$final.model)
  no.of.rules <- unique.nodes
  model.summary$Observations <- as.vector(table(predictions.data$Node))
  
  # Taking required Nodes and corresponding coefficients, pvalues
  
  if(single.node == T) {
    all.nodes <- 1
    coef.data <- as.data.frame(t(coef.data))
  } else {
    all.nodes <- as.numeric(unlist(dimnames(coef(model.fitted$final.model))[1]))
    coef.data <- as.data.frame(coef.data)
  }
  
  coef.data[,(ncol(coef.data) +1 )] <- all.nodes
  required.nodes <- all.nodes[which(all.nodes %in% as.numeric(unique(predictions.data$Node)))]
  coef.data <- coef.data[which(coef.data[,ncol(coef.data)] %in% required.nodes),]
  
  # Adding betas * X in data at physician level
  
  coef.data2 <- coef.data
  colnames(coef.data2)[ncol(coef.data2)] <- "Node"
  predictions.data.frame2 <-  predictions.data %>% left_join(coef.data2, by = "Node")
  predictions.data.frame3 <-  predictions.data.frame2[ , c(paste0(x.variables,".x"))] *
    predictions.data.frame2[,  c(paste0(x.variables,".y"))]
  predictions.data.frame2 <- cbind(predictions.data.frame2, predictions.data.frame3)
  colnames(predictions.data.frame2)[(ncol(predictions.data.frame2) - length(x.variables) + 1):(ncol(predictions.data.frame2))] <-
    paste0("predicted_",x.variables)
  coef.data <- coef.data[,-ncol(coef.data)]
  model.summary[,3: (3 + length(x.variables) )] <-  coef.data
  
  # P values
  
  if (single.node == T) {
    # model.summary[,p.value.column] <-  summary.model.fitteds$coefficients[,4]
    p.value.vecor <- vector(mode = "numeric", length = length(x.variables.with.intercept))
    zero.index.values <- which(! x.variables.with.intercept %in% attributes(summary.model.fitteds$coefficients[,4])$names )
    non.zero.index.values <- which( x.variables.with.intercept %in% attributes(summary.model.fitteds$coefficients[,4])$names )
    p.value.vecor[non.zero.index.values] <- summary.model.fitteds$coefficients[,4]
    p.value.vecor[zero.index.values] <- 0
    model.summary[,p.value.column] <-  p.value.vecor
  }  else {
    p.values <- lapply(names(summary.model.fitteds), function(x) as.data.table(t(summary.model.fitteds[[x]]$coefficients[,4])))
    p.values <-  as.data.frame(rbindlist(p.values,fill = T)  )
    p.values[,(ncol(p.values) + 1 )] <- all.nodes
    p.values <- p.values[which(p.values[,ncol(p.values)] %in% required.nodes),]
    p.values <- p.values[,-ncol(p.values)]
    
    model.summary[,p.value.column] <-  p.values
  }
  
  # Adding Means
  
  predictions.data <- as.data.table(predictions.data)
  if(intercept == F) {
    model.summary[,Mean.value.column] <- cbind(0, (setorder(predictions.data[,lapply(.SD, mean, na.rm=TRUE),
                                                                             by=Node,.SDcols=x.variables],Node)[,-1]))
  } else {
    model.summary[,Mean.value.column] <- cbind(1, (setorder(predictions.data[,lapply(.SD, mean, na.rm=TRUE),
                                                                             by=Node,.SDcols=x.variables],Node)[,-1]))
  }
  
  # Adding Predictions from variables
  
  model.summary[,pred.columns] <-    model.summary[, x.variables.with.intercept ] * model.summary[, Mean.value.column]
  model.summary$Prediction.from.model = rowSums(model.summary[pred.columns])
  
  # Adding % impact
  
  model.summary[,perc.columns] <-   model.summary[, pred.columns ]/model.summary$Prediction.from.model
  
  # Adding overall % impact
  
  if (single.node == F) {
    new.row <- nrow(model.summary) + 1
    model.summary[ new.row,perc.columns] <- (colSums(model.summary$Observations * model.summary[ ,perc.columns ])  /
                                               sum(model.summary$Observations))
    model.summary[ new.row, pred.columns.all] <-  colSums(model.summary[(1:no.of.rules),pred.columns.all])
    model.summary$Observations[nrow(model.summary)] <- sum(model.summary$Observations[1:no.of.rules])
  }
  
  model.summary$rule[nrow(model.summary)] <-  "Total"
  colnames(model.summary)[which(colnames(model.summary)=="rule")] <- "Node"
  
  
  # Adding Y  mean & % contribution
  
  size.variable.type <- ifelse(is.numeric(data[,which(colnames(data) %in% size.var)])==T,
                               "continous","categorical")
  color.variable.type <- ifelse(is.numeric(data[,which(colnames(data) %in% color.var)])==T,
                                "continous","categorical")
  
  if(single.node == F){
    
    if(size.variable.type == "continous") {
      model.summary$Mean.y <- c(eval(parse(text = paste0("(setorder(predictions.data[,lapply(.SD, mean, na.rm=TRUE),
                                                         by=Node,.SDcols=size.var],Node)[,-1])$",size.var) )),  mean(data[,size.var]) )
    } else {
      temp.df <- predictions.data[predictions.data[,which(colnames(predictions.data) %in% size.var)] ==
                                    levels(data[,which(colnames(data) %in% size.var)])[2],]
      model.summary$Mean.y <- c(eval(parse(text =   paste0("(setorder(temp.df[,lapply(.SD, length),
                                                           by=Node,.SDcols=size.var],Node)[,-1])$",size.var) )),  nrow(temp.df) ) /
        c(eval(parse(text =   paste0("(setorder(predictions.data[,lapply(.SD, length),
                                     by=Node,.SDcols=size.var],Node)[,-1])$",size.var) )),  nrow(predictions.data) )
    }
    } else{ model.summary$Mean.y <- 1
  }
  
  if(single.node == F){
    if(color.variable.type == "continous") {
      model.summary$Contribution.y <- c(eval(parse(text =   paste0("(setorder(predictions.data[,lapply(.SD, sum,
                                                                   na.rm=TRUE),by=Node,.SDcols=color.var],Node)[,-1] / sum(data[,color.var]) )$",color.var) )),1)
    } else {
      
      model.summary$Contribution.y <- c(eval(parse(text =   paste0("(setorder(temp.df[,lapply(.SD, length),
                                                                   by=Node,.SDcols=color.var],Node)[,-1])$",color.var) )),  nrow(temp.df) ) /
        length(which(predictions.data[,.SD,.SDcols = color.var] ==
                       levels(data[,color.var])[2] ))
    }
  } else {
    model.summary$Contribution.y <- 1
  }
  model.summary[(is.na(model.summary))] <- 0
  
  
  if(visuals == T){
    if(save.results == T) {
      jpeg(paste(filename=paste0("./",folder.name,"/Impacts.plot.",data.type,".",response.variable,".jpeg")),width = 2000,
           height = 1000,res = 80,quality=480,pointsize = 14)
    }
    plot(Impact.plotter(model.summary))
    #mtext(paste0("Tree plot for ",data.type), side = 3, line = -2, outer = TRUE)
    
    if(save.results == T) {
      dev.off()
    }
  }
  
  
  
  if(save.results==T){
    write.csv(model.summary,file=paste0("./",folder.name,"/Model.summary.",data.type,".", response.variable,".csv"),row.names = F)
    write.csv(predictions.data.frame2,file=paste0("./",folder.name,"/Data.with.predictions.",data.type,".",response.variable,
                                                  ".csv"),row.names = F)
    save(model.fitted,file = paste0("./",folder.name,"/Fitted.Model.",data.type,".",response.variable,"..Rda"))
  }
  
  if(exposure.effectiveness==T){
    if(save.results==T) {jpeg(paste(filename=paste0("./",folder.name,"/Exposure.vs.Effectiveness.plot.",data.type,".",
                                                    response.variable,".jpeg")),width = 2000, height = 1000,res = 80,quality=480,pointsize = 14)
    }
    plot(Exposure.vs.effect.plotter(data.summary =  model.summary,  x.var = x.variables,color.var = "Contribution.y",
                                    size.var = "Mean.y",label.var = "Node" ))
    #mtext(paste0("Exposure.vs.Effectiveness.plot. for ",data.type), side = 3, line = -2, outer = TRUE)
    
    if(save.results == T) {
      dev.off()
    }
  }
  
  return(list(model.summary ,as.data.frame(predictions.data)))
  
    }



### >>>>>>>>>>>> Impact Display Function --------------------

#' Impact.calculator
#' @description Calculates impacts of independent variables on the dependent variables at an overall level.
#'
#' @param summary A model summary (of class data.frame) containing impacts of independent variables at segment (Node) level.
#'
#' @return Returns the impactables in descending order.
#' @examples
#'
#' #  Load US States Production Data from plm package
#' data("Produc", package = "plm")
#'
#' # Fit a glmtree model
#' model <- Model.fitting(data = Produc[Produc$year <=  max(Produc$year),], response.variable = 
#'         "unemp", x.variables = c("pcap", "hwy", "water","util", "pc", "gsp","emp"), 
#'          cutting.variables = "region", min.node.size = 50,  data.type = "Example")
#'
#' # Generate a summary
#' model.summary <- Summary.generator(data = Produc[Produc$year <=  max(Produc$year),],
#'                  model.fitted = model, x.variables = c("pcap","hwy", "water","util", "pc", "gsp",
#'                  "emp"), response.variable = "unemp", data.type = "Example")
#'
#' Impact.calculator(summary =  model.summary[[1]], response.variable = "unemp" )
#'
#' @export
Impact.calculator <- function(summary) {
  
  x <- summary[nrow(summary),which(grepl("Percentage.Impact.",colnames(summary))==T)]
  print(t(sort(100*(x),decreasing = T)),row.names=F)
  
}




### >>>>>>>>>>>> Impact Display Function --------------------

#' Impact.calculator
#' @description Plots impacts of independent variables on the dependent variables at an overall level.
#'
#' @param summary A model summary (of class data.frame) containing impacts of independent variables at segment (Node) level.
#'
#' @return Plots the impactables in descending order.
#' @examples
#'
#' #  Load US States Production Data from plm package
#' data("Produc", package = "plm")
#'
#' # Fit a glmtree model
#' model <- Model.fitting(data = Produc[Produc$year <=  max(Produc$year),], response.variable = 
#'         "unemp", x.variables = c("pcap", "hwy", "water","util", "pc", "gsp","emp"), 
#'          cutting.variables = "region", min.node.size = 50,  data.type = "Example")
#'
#' # Generate a summary
#' model.summary <- Summary.generator(data = Produc[Produc$year <=  max(Produc$year),],
#'                  model.fitted = model, x.variables = c("pcap","hwy", "water","util", "pc", "gsp",
#'                  "emp"), response.variable = "unemp", data.type = "Example")
#'
#' Impact.plotter(summary =  model.summary[[1]] )
#'
#' @export
Impact.plotter <- function(summary) {
  
  x <- summary[nrow(summary),which(grepl("Percentage.Impact.",colnames(summary))==T)]
  variables <- dimnames(t(sort(x, decreasing = T)))[[1]]  
  variables <- sub(".*Percentage.Impact.", "",  variables)
  Impacts <- t(sort(x, decreasing = T))
  y <- as.data.frame( cbind(Impacts, variables   )) 
  colnames(y) <- c("Impact", "variables")
  y$Impact <- as.numeric(as.character(y$Impact))
  
  
  impacts.plot <- qplot(data = y ,y = Impact, x = reorder(variables, -Impact)) +   geom_bar(stat="identity")  +
    geom_text(aes(y=Impact, label= scales::percent(round(Impact,4))),color="black", size=8,  position = position_stack(vjust = 0.5)) +
    ggtitle("Impactables of variables") + 
    labs(x="Variables",y="% Impact") +
    theme(title = element_text(size = 26), axis.text=element_text(size=18),axis.title=element_text(size=24)) +
    scale_y_continuous( labels =  scales::percent)
  
  return( impacts.plot)
  
}



### >>>>>>>>>>>> Scoring Function to analyze performance on subsets of the data--------------------

#' Score.my.data.
#' @description Rescore performance of a model on a new dataset.
#'
#' @param new.data Dataset (containing the dependent & independent variables) on which the performance has to be evaluated.
#' @param fitted.model Fitted model whose performance is to be evaluated.
#' @param y.metric Dependent Variable.
#' @param x.variables Independent variables which were used for regression.
#' @param intercept.inclusion Logical. Should the impacts of intercept be calculated. By default it is TRUE.
#' @param train.perf.latest.time.period Logical. Should train Rsq be reported for last time.period only (v/s all time.periods). By
#'                              default it is TRUE.
#' @param time.period.variable.name Name of the variable containing the time series values. These can be anything, example - weeks,
#'                                  months, years etc., but should be non negative integers.
#' @param threshold.for.categorical Threshold (cut off) value to classify the observations in each class
#'                                   with respect to their predicted probability.
#' @param visualizations Logical. Should visuals be generated (plots & graphs). By default it is TRUE.
#' @param write.results.to.disk Logical. Should results be saved to disk. By default it is FALSE. If TRUE, a folder name has to
#'                              be specified in the 'folder' argument.
#' @param folder Used in case argument 'write.results.to.disk' is set to TRUE. It is the name of the folder in which the results are saved.
#'
#'
#' @return Performance metrics obtained from Linear.evaluation & Logistic.evaluation
#' @seealso
#' \link[LaunchAnalyticZS]{Linear.evaluation}
#' \link[LaunchAnalyticZS]{Logistic.evaluation}
#' @examples
#'
#' #  Load US States Production Data from plm package
#' data("Produc", package = "plm")
#'
#' # Fit a glmtree model
#' model <- Model.fitting(data = Produc[Produc$year <=  max(Produc$year),], response.variable =
#'         "unemp", x.variables = c("pcap", "hwy", "water","util", "pc", "gsp","emp"),
#'          cutting.variables = "region", min.node.size = 50,  data.type = "Example")
#'
#' analysis.new.df <- Score.my.data( new.data =  Produc[Produc$year <= 1979,], fitted.model = model,
#'                    y.metric = "unemp", x.variables = c("pcap", "hwy","water","util", "pc", "gsp",
#'                    "emp"), time.period.variable.name = "year")
#'
#' @export
Score.my.data = function(new.data, fitted.model,  y.metric, x.variables,  intercept.inclusion = T,  train.perf.latest.time.period = T,
                         time.period.variable.name,  threshold.for.categorical = NULL, visualizations = T,
                         write.results.to.disk = F,  folder = NULL)
  
{
  cat("\n Mean of the X variables for this subset: \n")
  print(colMeans(new.data[,x.variables]))
  
  variable.type <-  ifelse(is.numeric(new.data[,which(colnames(new.data) %in% y.metric)])==T, "continous","categorical")
  
  if(variable.type == "categorical") {
    perf.evaluation.new.data <- Logistic.evaluation(model.fitted = fitted.model$final.model,  data = new.data,
                                                    response.variable = y.metric, data.type="New.Data",visuals = visualizations,
                                                    threshold = threshold.for.categorical,  save.results=write.results.to.disk)
  } else{
    if(train.perf.latest.time.period == F) {
      perf.evaluation.new.data <-  Linear.evaluation(model.fitted = fitted.model$final.model,
                                                     data=new.data,response.variable = y.metric,
                                                     visuals = visualizations,data.type="New.Data",
                                                     save.results=write.results.to.disk)
    } else {
      perf.evaluation.new.data <-  Linear.evaluation(model.fitted = fitted.model$final.model,
                                                     data = new.data[new.data[,time.period.variable.name] == 
                                                                       max(new.data[,time.period.variable.name]),],
                                                     response.variable = y.metric,  visuals = visualizations,
                                                     data.type="New.Data",save.results=write.results.to.disk)
    }
  }
  
  new.summary <-  Summary.generator(new.data, fitted.model, y.metric, x.variables,  intercept = intercept.inclusion,
                                    exposure.effectiveness = T, data.type ="New.Data",visuals = visualizations,
                                    save.results = write.results.to.disk,  folder.name = folder )
  new.impacts <-  Impact.calculator(new.summary[[1]],y.metric)
  
  return(perf.evaluation.new.data)
  
}



### >>>>>>>>>>>> Model Building  (Currently supported MOB models)----------------------------------------


#' Model.fitting
#' @description Fits a GLMTREE model
#'
#' @param data Dataset to be used for modeling.
#' @param response.variable Dependent Variable.
#' @param x.variables Independent variables to be included for regression.
#' @param cutting.variables Independent variables to be included for partitioning / making data cuts in the tree.
#' @param max.iterations Integer giving the maximal number of IWLS iterations. By default it is 500. See \link[partykit]{glmtree}
#' @param min.node.size The minimum number of observations in a node. By default it is 500. See \link[partykit]{glmtree}
#' @param alpha.value Numeric significance level. By default it is 0.05. See \link[partykit]{glmtree}
#' @param epsilon.value Positive convergence tolerance. By default it is 1e-4. See \link[partykit]{glmtree}
#' @param visuals Logical. Should visuals be generated (plots & graphs). By default it is TRUE.
#' @param save.results Logical. Should results be saved to disk. By default it is FALSE. If TRUE, a folder name has to be specified
#'                     in the 'folder.name' argument.
#' @param folder.name Used in case argument 'save.results' is set to TRUE. It is the name of the folder in which the results
#'                    are saved.
#' @param data.type Character describing which data is used (Train/Test/Validation).This is helpful when saving the results to
#'                  disk.
#'
#' @return A list containing the following elements:
#' \item{final.model}{Fitted GLMTREE model}
#' @examples
#'
#' #  Load US States Production Data from plm package
#' data("Produc", package = "plm")
#'
#' # Fit a glmtree model
#' model <- Model.fitting(data = Produc[Produc$year <=  max(Produc$year),], response.variable = 
#'         "unemp", x.variables = c("pcap", "hwy", "water","util", "pc", "gsp","emp"), 
#'          cutting.variables = "region",  min.node.size = 50,  data.type = "Example")
#' @export
Model.fitting <- function(data, response.variable, x.variables, cutting.variables,  max.iterations = 500, min.node.size = 500,
                          alpha.value = 0.05, epsilon.value = 1e-4, visuals = T, save.results = F, folder.name = NULL, data.type)  {
  
  # Get Family Type
  # if(missing(min.node.size)) {
  #   min.node.size <- 0.05 * nrow(data)
  # }
  family.type <-  ifelse(is.numeric(data[,which(colnames(data) %in% response.variable)])==T,"gaussian","binomial")
  formula.for.modeling <- paste(response.variable,"~",paste(x.variables,collapse = " +"),"|",
                                paste(cutting.variables,collapse="+"),sep=" ")
  
  # Building model
  
  start.time <- Sys.time()
  model.fitted <- partykit::glmtree(as.formula(formula.for.modeling),data = data, family = family.type,
                                    maxit = max.iterations, minsplit = min.node.size, alpha = alpha.value,epsilon = epsilon.value)
  
  cat("\n execution time for final model on ",data.type," (in secs) : \t",difftime(Sys.time(),start.time,units="secs"),"\n")
  
  # Checking for missing values among coefficients
  
  coef.data=coef(model.fitted)
  
  if(length(which(is.na(coef.data))) > 0 ) {
    warning("\n There were NA values in coeficients for some models.These were replaced with 0s.\n")
  }
  coef.data[is.na(coef.data)] <- 0
  
  # Ploting
  
  if(visuals == T){
    if(save.results == T) {
      jpeg(paste(filename=paste0("./",folder.name,"/GLMtree.tree.plot.",data.type,".",response.variable,".jpeg")),width = 2000,
           height = 1000,res = 80,quality=480,pointsize = 14)
    }
    plot(as.constparty(model.fitted),tnex=1,gp=gpar())
    #mtext(paste0("Tree plot for ",data.type), side = 3, line = -2, outer = TRUE)
    
    if(save.results == T) {
      dev.off()
    }
  }
  
  return(list(final.model= model.fitted))
  
}



### >>>>>>>>>>>> Model Building Parallel CV (Currently supported MOB models) ----------------------------------------

#' Model.fitting.kfold.cv
#' @description  Fits a GLMTREE model with cross validation
#'
#' @param data Dataset to be used for modeling.
#' @param data.test Test data.
#' @param response.variable Dependent Variable.
#' @param x.variables Independent variables to be included for regression.
#' @param cutting.variables Independent variables to be included for partitioning / making data cuts in the tree.
#' @param min.node.size The minimum number of observations in a node. See \link[partykit]{glmtree}
#' @param no.of.folds value of k for k cross fold validation.By default it is 5.
#' @param visuals Logical. Should visuals be generated (plots & graphs). By default it is TRUE.
#' @param save.results Logical. Should results be saved to disk. By default it is FALSE. If TRUE, a folder name has to be specified
#'                     in the 'folder.name' argument.
#' @param folder.name Used in case argument 'save.results' is set to TRUE. It is the name of the folder in which the results
#'                    are saved.
#' @param data.type Character describing which data is used (Train/Test/Validation).This is helpful when saving the results to
#'                  disk.
#'
#' @return A list containing the following elements:
#' \item{Score}{Optimized (Maximum) value of the metric to be maximized. It is 1/ RMSE for linear  and AUC for
#'               logistic models.}
#' \item{optimum.RMSE.value, optimum.AUC.value}{Optimized RMSE/ AUC value for linear/logistic regression}
#' \item{optimum.Rsq.value, optimum.Accuracy.value}{Optimized R square/ Accuracy value for linear/logistic regression.}
#' \item{optimum.mat.transpose}{Confusion matrix for Validation Data for logistic regression. This will be 0 for linear regression.}
#' \item{optimum.cutoff}{Value of threshold used for Validation Data for logistic regression. This will be 0 for linear regression.}
#' \item{Final.train.set.optim}{Train data after appropriate transformations as per 'adstock' argument and optimized hyperparameters.}
#' \item{Test.set.optim}{Test data after appropriate transformations as per 'adstock' argument and optimized hyperparameters.}
#' \item{Best.Model}{Fitted GLMTREE model}
#' @seealso \link[partykit]{glmtree}
#' @examples
#'
#' #  Load US States Production Data from plm package
#' data("Produc", package = "plm")
#'
#' # Fit a glmtree model
#' model <- Model.fitting.kfold.cv(data = Produc[Produc$year <=  max(Produc$year),], data.test =
#'          Produc[Produc$year ==  max(Produc$year),], response.variable = "unemp", x.variables =
#'           c("pcap", "hwy", "water","util", "pc","gsp","emp"), cutting.variables = "region",
#'           min.node.size = 50, data.type = "Example")
#' @export
Model.fitting.kfold.cv <-  function(data, data.test, response.variable, x.variables, cutting.variables, min.node.size,
                                    no.of.folds = 5, visuals = T, save.results = F, folder.name = NULL, data.type ) {
  
  variable.type.arg <-  ifelse(is.numeric(data[,which(colnames(data) %in% response.variable)]) == T,
                               "continous","categorical")
  sample.1 <- createFolds(data[ ,colnames(data)[which(colnames(data) %in% response.variable)]],
                          returnTrain = F, k = no.of.folds)
  split.up <- lapply(sample.1, function(ind, dat) dat[ind,], dat = data)
  seq.all <- seq(1,length(split.up),1)
  start.time.parallel <- Sys.time()
  
  # Parallelization
  
  set.seed(12345)
  results <- foreach(splitts = seq.all,
                     .export =   c(objects(),
                                   "Breusch.pagan.test",  "Correlation.matrix.plotter",   "Driver.analysis.kfold.cv" ,  #"Driver.analysis.pipeline.kfold.cv",
                                   "Driver.analysis.with.sparse.data", "Durbin.watson.test",   "Feature.engineering" ,"folder.name", "Hausman.test",
                                   "Impact.calculator", "Linear.evaluation", "Logistic.evaluation" ,  "Model.fitting" ,"Model.fitting.kfold.cv",
                                   "Na.check", "Optimizer.External.kfold.cv" , "Outlier.remover", "Sens.and.spec.calculator", #"regression.var", "y.metric", #"multiplot" ,
                                   "Score.my.data"  , "Summary.generator", #"test.phys", #"train.df",    #"train.data.frame" , "Summary.Display",  "splitting.var"
                                   "Univariate.plotter",  "Zero.sd.check"),               #"validation.set", "train.phys" ,"variable.type", "describe.my.data",
                     .packages = c("partykit", "caret","dplyr","Rgraphviz","rBayesianOptimization","ROCR", "plyr","sigmoid","pROC", "data.table", "Hmisc", "plm",
                                   "GGally","gtools", "car", "grid", "corrplot", "gridExtra", "foreach", "doParallel","doRNG")) %dopar% {
                                     
                                     set.seed(12345)
                                     seq.all.2 <- seq.all[-seq.all[splitts]]
                                     validation.set <- split.up[[which(!seq.all %in% seq.all.2)]]
                                     train.data <- do.call("rbind", split.up[seq.all.2])
                                     
                                     model.opt <- Model.fitting(x.variables = x.variables,cutting.variables = cutting.variables,
                                                                response.variable = response.variable, data = train.data,
                                                                min.node.size = min.node.size, data.type = "Train.Data",
                                                                folder.name = folder.name )
                                     
                                     if(variable.type.arg=="categorical")
                                     {
                                       perf.evaluation.internal <-  Logistic.evaluation(model.fitted = model.opt$final.model,data =
                                                                                          validation.set, response.variable =
                                                                                          response.variable, data.type = "Validation.Data",
                                                                                        visuals = F, save.results = F,
                                                                                        folder.name = folder.name )
                                       internal.results <- list(Score = as.numeric(perf.evaluation.internal$AUC),
                                                                optimum.AUC.value = perf.evaluation.internal$AUC,
                                                                optimum.Accuracy.value = perf.evaluation.internal$Accuracy,
                                                                optimum.mat.transpose = perf.evaluation.internal$Confusion.Matrix,
                                                                optimum.cutoff = perf.evaluation.internal$Threshold,
                                                                Model = model.opt$final.model,Validation.Data = validation.set)
                                     }
                                     else{
                                       perf.evaluation.internal <- Linear.evaluation(model.fitted = model.opt$final.model,
                                                                                     data = validation.set, response.variable =
                                                                                       response.variable,data.type = "Validation.Data",
                                                                                     visuals = F, save.results = F, folder.name = folder.name )
                                       internal.results <- list(Score= as.numeric(1/ perf.evaluation.internal$RMSE),
                                                                optimum.RMSE.value = perf.evaluation.internal$RMSE,
                                                                optimum.Rsq.value = perf.evaluation.internal$Rsq,
                                                                Model = model.opt$final.model, Validation.Data = validation.set)
                                     }
                                     
                                     return(internal.results)
                                     
                                   }
  
  if(variable.type.arg=="categorical") {
    summary.results <- foreach(splitts.result=results, splitts.num=icount(), .combine=rbind) %do% {
      as.data.frame(cbind(AUC.value = splitts.result$optimum.AUC.value,
                          Accuracy.value = splitts.result$optimum.Accuracy.value,
                          Threshold = splitts.result$optimum.cutoff)) %>% mutate(Fold=splitts.num)
    }
    i= which.max(summary.results$AUC.value)
    optimum.cutoff<- results[[i]][[3]]
    
    perf.evaluation.validation <-Logistic.evaluation(model.fitted = results[[i]]$Model,data = results[[i]]$Validation.Data,
                                                     response.variable = response.variable, data.type ="Validation.Data",
                                                     visuals = visuals, save.results = save.results,folder.name = folder.name ,
                                                     threshold = optimum.cutoff)
    perf.results <- list(Score = max(summary.results$AUC.value),
                         optimum.AUC.value = perf.evaluation.validation$AUC,
                         optimum.Accuracy.value = perf.evaluation.validation$Accuracy,
                         optimum.mat.transpose = perf.evaluation.validation$Confusion.Matrix,
                         optimum.cutoff = perf.evaluation.validation$Threshold,
                         Final.train.set.optim = data, Test.set.optim = data.test,
                         Best.Model = results[[i]]$Model)
    
  } else {
    summary.results <- foreach(splitts.result = results, splitts.num = icount(), .combine = rbind) %do% {
      as.data.frame(cbind(RMSE.value = splitts.result$optimum.RMSE.value,
                          Rsq.value = splitts.result$optimum.Rsq.value)) %>% mutate(Fold = splitts.num)   }
    i= which.min(summary.results$RMSE.value)
    perf.evaluation.validation <-Linear.evaluation(model.fitted = results[[i]]$Model,
                                                   data = results[[i]]$Validation.Data, response.variable = response.variable,
                                                   data.type ="Validation.Data",  visuals = visuals,
                                                   save.results = save.results, folder.name = folder.name )
    perf.results <- list(Score = (1/min(summary.results$RMSE.value)),
                         optimum.RMSE.value = perf.evaluation.validation$RMSE,
                         optimum.Rsq.value = perf.evaluation.validation$Rsq,
                         optimum.mat.transpose = 0, optimum.cutoff = 0, Final.train.set.optim = data,
                         Test.set.optim = data.test, Best.Model = results[[i]]$Model)
    
  }
  
  if(visuals==T){
    if(save.results==T) {
      jpeg(paste(filename=paste0("./",folder.name,"/GLMtree.tree.plot.",data.type,".",response.variable,".jpeg")),
           width = 2000,height = 1000,res = 80,quality=480,pointsize = 14)
    }
    plot(as.constparty(perf.results$Best.Model),tnex=1,gp=gpar())
    if(save.results==T) {
      dev.off()
    }
  }
  
  return(perf.results)
}



### >>>>>>>>>>>> Function to return Metrics for Optimizer -----------------------------------------------------------


# Optimizer.External.
# @description  Finds the optimized hyperparameters for a GLMTree model by Bayesian Optimization using Holdout Validation.
#
# @param variable.type.optim Variable type of response variable to be optimized (categorical or continous).
# @param opt.init.seeds Number of points to sample before Bayesian Optimization.
# @param opt.iterations Number of Bayesian Optimization iterations.
# @param bounds.hyperparameters List containing upper & lower bound for all the hyperparameters.
# @param save.results.optim Logical. Should results be saved to disk ?
# @param testing.time.period.optim Value of time period for which the performance is to be evaluated. This assumes the  values of the time
#                      period follow the sequence 1,2,3... with highest value representing the latest time period.
# @param adstock.optim Binary. If TRUE adstock (Koyck) transformation will be performed on the dependent variables selected in
#                      'x.var.optim' argument.
# @param adstock.bounds.optim  List of upper and lower bounds for transformation constants for each variable. By default the
#                             lower and upper bounds are 0 and 1 respectively.
# @param visuals.optim Logical. Should visuals be generated (plots & graphs) ?
# @param train.dat.optim Data (containing the dependent & independent variables) on which the model has to be build.
# @param test.dat.optim Data (containing the dependent & independent variables) on which the performance has to be evaluated.
# @param x.var.optim Independent variables to be included for regression.
# @param cutting.variables.optim Independent variables to be included for making data cuts (tree).
# @param response.variable.optim Dependent Variable.
# @param time.period.variable.name.optim Name of Variable containing the time series value.
# @param normalize.data.optim Logical. If TRUE all the dependent (continous) variables selected in'x.var.optim' argument will
#                             be normalized (Z transformed).
# @param id.variable.name.optim Name of ID/Account variable for each record.
#
#
# @return A list containing the following elements:
# \item{Best.Parameters} {Optimized value of hyperparameters}
# \item{optimized.results} {Performance results for optimzed validation model}
# \item{optimized.threshold} {Value of threshold used for Validation Data for logistic regression. This will be 0 for linear
#                             regression}
# \item{Final.train.set} {Full Train data after appropriate transformations as per adstock argument or choice of hyperparameter}
# \item{Test.set} {Test data after appropriate transformations as per adstock argument or choice of hyperparameter}
# \item{Validation.set} {Holdout validation data after appropriate transformations as per adstock argument or choice of
#                        hyperparameter}
# \item{Initial.train.set} {Initial train data after appropriate transformations as per adstock argument or choice of
#                         hyperparameter. This excludes the validation data from the full Train data}
# \item{Train.adstock.lambdas} {Optimized values of constants (Lambdas) for adstock (Koyck) transformation on full Train data}
# \item{Test.adstock.lambdas} {Optimized values of constants (Lambdas) for adstock (Koyck) transformation on Test data}
# @seealso
# \link[LaunchAnalyticZS]{Linear.evaluation}
# \link[LaunchAnalyticZS]{Logistic.evaluation}
#


Optimizer.External <- function(variable.type.optim, opt.init.seeds = 10, opt.iterations= 10, bounds.hyperparameters,
                               train.dat.optim, test.dat.optim, x.var.optim, cutting.variables.optim, response.variable.optim,
                               length.validation.time.period.optim, transformations.used.optim, adstock.optim = F,
                               adstock.bounds.optim = NULL, time.period.variable.name.optim, normalize.data.optim = F,
                               folder.name.optim = NULL, id.variable.name.optim , save.results.optim = F, visuals.optim = F) {
  
  start.time <- Sys.time()
  Transformation.list <- gtools::permutations(length(transformations.used.optim), 1, v = transformations.used.optim, set= TRUE,
                                              repeats.allowed = TRUE)
  multiple.letters <- paste0("Transformation.var",seq(1,1000))
  #c(letters, c(t(outer(letters, letters, paste, sep = ""))))
  
  Optimizer.Internal <- function(min.node.size.opt, ... , #a,b,c,d,
                                 visuals.optim.int = F, save.results.opt.int = F) {  #transformation.opt,
    
    # Log Transformation
    # indices.vec <- c(a,b,c,d)
    indices.vec <- c(...)
    
    # if(transformation.opt == 1) {
    train.dat.optim.new <- Feature.engineering(data = train.dat.optim, x.var.optim,
                                               transformations = Transformation.list[indices.vec] )
    cat("\n Checking for possible variable(s) with no variance after transformation...\n")
    sd.check <- Zero.sd.check(train.dat.optim.new[,x.var.optim])
    if(is.null(sd.check) == F){stop(" \n Please revise your tranformations. Some of the variable(s) after transformation zero variance on train data. \n")}
    
    cat("\n Checking for possible variable(s) with NaN values after transformation...\n")
    Nan.check <- Nan.check(train.dat.optim.new[,x.var.optim])
    if(is.null(Nan.check) == F){stop(" \n Please revise your tranformations. Some of the variable(s) after transformation have NaN values on train data. \n")}
    
    # rep("log.transform",length(x.var.optim))
    # c(Transformation.list[a], Transformation.list[b],
    #   Transformation.list[c], Transformation.list[d] )
    test.dat.optim.new <- Feature.engineering(data = test.dat.optim, x.var.optim,
                                              transformations = Transformation.list[indices.vec] )  # rep("log.transform",length(x.var.optim))
    cat("\n Checking for possible variable(s) with no variance after transformation...\n")
    sd.check <- Zero.sd.check(test.dat.optim.new[,x.var.optim])
    if(is.null(sd.check) == F){stop(" \n Please revise your tranformations. Some of the variable(s) after transformation have zero variance on test data. \n")}
    
    
    cat("\n Checking for possible variable(s) with NaN values after transformation...\n")
    Nan.check <- Nan.check(test.dat.optim.new[,x.var.optim])
    if(is.null(Nan.check) == F){stop(" \n Please revise your tranformations. Some of the variable(s) after transformation have NaN values on test data. \n")}
    
    # } else  {
    #   train.dat.optim.new <-  train.dat.optim
    #   test.dat.optim.new <-   test.dat.optim
    # }
    
    # Adstock.transformation
    
    testing.time.period.optim <- max(test.dat.optim[,time.period.variable.name.optim])
    length.total.time.period.optim <- testing.time.period.optim - min(test.dat.optim[,time.period.variable.name.optim]) + 1
    
    if(adstock.optim == T) {
      if(variable.type.optim == "continous") {
        y.metric.adstock = response.variable.optim
      } else {
        stop(" \n Define an alternate y  metric to be used for Adstock transformation in case y metric is a categorical variable \n ")
        y.metric.adstock = "Xiidra_trx"
      }
      
      adstock.train <-  Adstock.transformation( data = train.dat.optim.new, variables.list = x.var.optim,
                                                id.variable = id.variable.name.optim, time.period.id = time.period.variable.name.optim,
                                                y.variable = y.metric.adstock, total.time.period = (length.total.time.period.optim-1),
                                                bounds.list = adstock.bounds.optim, data.type = "Train data" )
      train.dat.optim.new <- adstock.train$transformed.data
      train.adstock.constants <- adstock.train$adstock.constants
      
      adstock.test <-  Adstock.transformation( data = test.dat.optim.new, variables.list = x.var.optim ,
                                               id.variable = id.variable.name.optim, time.period.id = time.period.variable.name.optim,
                                               y.variable = y.metric.adstock, total.time.period = length.total.time.period.optim,
                                               bounds.list = adstock.bounds.optim, data.type = "Test data" )
      test.dat.optim.new <- adstock.test$transformed.data
      test.adstock.constants <- adstock.test$adstock.constants
      
    } else{
      train.adstock.constants <- NULL
      test.adstock.constants <- NULL
    }
    
    # Normalizing continous variables (Regression Variables only)
    
    if(normalize.data.optim == T ){
      temp.train<- train.dat.optim.new %>%   select(one_of(x.var.optim, response.variable.optim))   %>%
        mutate_each(funs(range01),vars= one_of(x.var.optim, response.variable.optim) )
      train.dat.optim.new[,c(x.var.optim,response.variable.optim) ] <- temp.train[(ncol(temp.train) + 1 - length(c(x.var.optim,
                                                                                                                   response.variable.optim))):ncol(temp.train)]
      
      temp.test<- test.dat.optim.new %>%   select(one_of(x.var.optim, response.variable.optim))   %>%
        mutate_each(funs(range01),vars= one_of(x.var.optim, response.variable.optim) )
      test.dat.optim.new[,c(x.var.optim,response.variable.optim) ] <- temp.test[(ncol(temp.test) + 1 - length(c(x.var.optim,
                                                                                                                response.variable.optim))):ncol(temp.test)]
    }
    
    # Defining datasets
    
    set.seed(12345)
    time.period.col.index.train <- which(colnames(train.dat.optim.new)== time.period.variable.name.optim)
    time.period.col.index.test <- which(colnames(test.dat.optim.new)== time.period.variable.name.optim)
    train.data <- train.dat.optim.new[train.dat.optim.new[,time.period.col.index.train] <=
                                        (testing.time.period.optim - 1 - length.validation.time.period.optim ) , ]
    validation.data <- train.dat.optim.new[train.dat.optim.new[,time.period.col.index.train] >
                                             (testing.time.period.optim - 1 - length.validation.time.period.optim ) , ]
    
    
    
    # Fitting Model
    
    model.opt <- Model.fitting(x.variables = x.var.optim, cutting.variables = cutting.variables.optim,
                               response.variable = response.variable.optim, data=train.data,
                               min.node.size = min.node.size.opt,  data.type = "Train.Data.initial",
                               visuals = visuals.optim.int, save.results = save.results.opt.int,
                               folder.name = folder.name.optim )
    
    # Checking performance on validation data
    
    if(variable.type.optim =="categorical")    {
      perf.evaluation.internal <- Logistic.evaluation(model.fitted = model.opt$final.model,data = validation.data,
                                                      response.variable = response.variable.optim, data.type ="Validation.Data",
                                                      visuals = visuals.optim.int, save.results = save.results.opt.int, folder.name = folder.name.optim )
      perf.results <- list(Score = as.numeric(perf.evaluation.internal$AUC), optimum.AUC.value = perf.evaluation.internal$AUC,
                           optimum.Accuracy.value = perf.evaluation.internal$Accuracy,
                           optimum.mat.transpose = perf.evaluation.internal$Confusion.Matrix,
                           optimum.cutoff = perf.evaluation.internal$Threshold, Final.train.set.optim = train.dat.optim.new,
                           Test.set.optim = test.dat.optim.new, Validation.set.optim = validation.data,
                           Initial.train.set.optim = train.data, train.adstock.constants.optim = train.adstock.constants,
                           test.adstock.constants.optim = test.adstock.constants)
    }    else {
      perf.evaluation.internal <- Linear.evaluation(model.fitted = model.opt$final.model,data = validation.data,
                                                    response.variable = response.variable.optim, data.type ="Validation.Data",
                                                    visuals = visuals.optim.int, save.results = save.results.opt.int, folder.name = folder.name.optim )
      perf.results <- list(Score = as.numeric(1/ perf.evaluation.internal$RMSE),
                           optimum.RMSE.value = perf.evaluation.internal$RMSE, optimum.Rsq.value = perf.evaluation.internal$Rsq,
                           optimum.mat.transpose = 0, optimum.cutoff = 0, Final.train.set.optim = train.dat.optim.new,
                           Test.set.optim = test.dat.optim.new, Validation.set.optim = validation.data,
                           Initial.train.set.optim = train.data, train.adstock.constants.optim = train.adstock.constants,
                           test.adstock.constants.optim = test.adstock.constants)
    }
    return(perf.results)
    
  }
  
  # Optimization
  
  OPT.Res <- BayesianOptimization(Optimizer.Internal, bounds = bounds.hyperparameters, init_grid_dt = NULL,
                                  init_points = opt.init.seeds, n_iter =  opt.iterations, acq = "ucb", kappa = 1,
                                  eps = 0.0, verbose = TRUE, kernel = list(type= "exponential", power = 2 )) # type= "exponential", power = 2 ,type="matern",nu=5/2
  cat("\n Execution time for optimization (in mins) :",difftime(Sys.time(), start.time,units="mins"),"\n")
  if(save.results.optim == T){
    write.csv(OPT.Res$History,file=paste0("./",folder.name.optim,"/Bayesian.opt.iterations.",response.variable.optim,".csv"),
              row.names = F)
  }
  
  # Running final model on initial train data
  
  # optimized.run <-  Optimizer.Internal(min.node.size.opt = OPT.Res$Best_Par[1], transformation.opt = OPT.Res$Best_Par[2],
  #                                      visuals.optim.int = visuals.optim, save.results.opt.int = save.results.optim )
  optimized.run <-  Optimizer.Internal(min.node.size.opt = OPT.Res$Best_Par[1], ... = OPT.Res$Best_Par[2: (2 + length(x.var.optim) -1)],
                                       visuals.optim.int = visuals.optim, save.results.opt.int = save.results.optim )
  if(adstock.optim == T){
    x.var.optim2 <- x.var.optim[which(!grepl("lag", x.var.optim)==T)]
    adstock.data <- data.table(Data.type = rep(c("Train","Test"),each = length(x.var.optim2)), Variables =
                                 rep(x.var.optim2, 2), Values = 
                                 c(optimized.run$train.adstock.constants.optim, 
                                   optimized.run$test.adstock.constants.optim) )
    write.csv(adstock.data, file = paste0("./",folder.name.optim,"/Adstock.constants.",response.variable.optim,".csv"),
              row.names = F)
  }
  
  #a = OPT.Res$Best_Par[2], b = OPT.Res$Best_Par[3],
  # c = OPT.Res$Best_Par[4], d = OPT.Res$Best_Par[5]
  
  cat("\n Best transformation combination on validation data : \n")
  print(Transformation.list[OPT.Res$Best_Par[2: (2 + length(x.var.optim) -1)]])
  cat(" for the variables (in respective order): \n ")
  print(x.var.optim)
  
  
  transformation.data <- data.table( Variables =  x.var.optim, Values = 
                                       Transformation.list[OPT.Res$Best_Par[2: (2 + length(x.var.optim) -1)]] )
  write.csv(transformation.data, file = paste0("./",folder.name.optim,"/transformations.",response.variable.optim,".csv"),
            row.names = F)
  
  return(list(Best.Parameters = OPT.Res$Best_Par,
              optimized.results = list(optimized.run[[2]],optimized.run[[3]],optimized.run[[4]]),
              optimized.threshold = optimized.run$optimum.cutoff, Final.train.set = optimized.run$Final.train.set.optim,
              Test.set = optimized.run$Test.set.optim,  Validation.set = optimized.run$Validation.set.optim,
              Initial.train.set = optimized.run$Initial.train.set.optim,
              Train.adstock.lambdas = optimized.run$train.adstock.constants.optim,
              Test.adstock.lambdas = optimized.run$test.adstock.constants.optim))
  
}



### >>>>>>>>>>>> Function to return Metrics for Optimizer - Parallel CV  -----------------------------------------------------------


# Optimizer.External.kfold.cv
# @description  Finds the optimized hyperparameters for a GLMTree model by Bayesian Optimization using k fold Validation.
#
# @param variable.type.optim Variable type of response variable to be optimized (categorical or continous).
# @param opt.init.seeds Number of points to sample before Bayesian Optimization.
# @param opt.iterations Number of Bayesian Optimization iterations.
# @param bounds.hyperparameters List containing upper & lower bound for all the hyperparameters.
# @param save.results.optim Logical. Should results be saved to disk ?
# @param testing.time.period.optim Value of time period for which the performance is to be evaluated. This assumes the  values of the time
#                      period follow the sequence 1,2,3... with highest value representing the latest time period.
# @param adstock.optim Binary. If TRUE adstock (Koyck) transformation will be performed on the dependent variables selected in
#                      'x.var.optim' argument.
# @param adstock.bounds.optim  List of upper and lower bounds for transformation constants for each variable. By default the
#                             lower and upper bounds are 0 and 1 respectively.
# @param visuals.optim Logical. Should visuals be generated (plots & graphs) ?
# @param train.dat.optim Data (containing the dependent & independent variables) on which the model has to be build.
# @param test.dat.optim Data (containing the dependent & independent variables) on which the performance has to be evaluated.
# @param x.var.optim Independent variables to be included for regression.
# @param cutting.variables.optim Independent variables to be included for making data cuts (tree).
# @param response.variable.optim Dependent Variable.
# @param time.period.variable.name.optim Name of Variable containing the time series value.
# @param normalize.data.optim Logical. If TRUE all the dependent (continous) variables selected in'x.var.optim' argument will
#                             be normalized (Z transformed).
# @param id.variable.name.optim Name of ID/Account variable for each record.
# @param folds.optim value of k for k cross fold validation.
#
# @return A list containing the following elements:
# \item{Best.Parameters} {Optimized value of hyperparameters}
# \item{optimized.results} {Performance results for optimzed validation model}
# \item{optimized.threshold} {Value of threshold used for Validation Data for logistic regression. This will be 0 for linear
#                             regression}
# \item{Final.train.set} {Full Train data after appropriate transformations as per adstock argument or choice of hyperparameter}
# \item{Test.set} {Test data after appropriate transformations as per adstock argument or choice of hyperparameter}
# \item{Train.adstock.lambdas} {Optimized values of constants (Lambdas) for adstock (Koyck) transformation on full Train data}
# \item{Test.adstock.lambdas} {Optimized values of constants (Lambdas) for adstock (Koyck) transformation on Test data}
# @seealso
# \link[LaunchAnalyticZS]{Linear.evaluation}
# \link[LaunchAnalyticZS]{Logistic.evaluation}
Optimizer.External.kfold.cv <- function(variable.type.optim, opt.init.seeds = 10, opt.iterations = 10, bounds.hyperparameters,
                                        train.dat.optim, test.dat.optim, x.var.optim, cutting.variables.optim,
                                        response.variable.optim, adstock.optim = F, adstock.bounds.optim = NULL,
                                        time.period.variable.name.optim ,transformations.used.optim,
                                        normalize.data.optim = F, folds.optim = 5,
                                        id.variable.name.optim ,  save.results.optim = F,visuals.optim = F,
                                        folder.name.optim = NULL) {
  
  start.time <- Sys.time()
  # Transformation.list <- permutations(4, 1, v = c("log.transform", "negative.exponential", "sigmoid","No.transformation"), set= TRUE,
  #                                     repeats.allowed=TRUE)
  Transformation.list <- gtools::permutations(length(transformations.used.optim), 1, v = transformations.used.optim, set= TRUE,
                                              repeats.allowed = TRUE)
  multiple.letters <- paste0("Transformation.var",seq(1,1000))
  
  Optimizer.Internal <- function(min.node.size.opt, ...  , visuals.optim.int = F, save.results.opt.int = F) {  # transformation.opt
    
    testing.time.period.optim <-  max(train.dat.optim[,time.period.variable.name.optim])
    
    # Log Transformation
    
    
    indices.vec <- c(...)
    train.dat.optim.new <- Feature.engineering(data = train.dat.optim, x.var.optim,
                                               transformations = Transformation.list[indices.vec] )
    cat("\n Checking for possible variable(s) with no variance after transformation...\n")
    sd.check <- Zero.sd.check(train.dat.optim.new[,x.var.optim])
    if(is.null(sd.check) == F){stop(" \n Please revise your tranformations. Some of the variable(s) after transformation have zero variance on train data. \n")}   
    test.dat.optim.new <- Feature.engineering(data = test.dat.optim, x.var.optim,
                                              transformations = Transformation.list[indices.vec] )
    
    cat("\n Checking for possible variable(s) with NaN values after transformation...\n")
    Nan.check <- Nan.check(train.dat.optim.new[,x.var.optim])
    if(is.null(Nan.check) == F){stop(" \n Please revise your tranformations. Some of the variable(s) after transformation have NaN values on train data. \n")}
    
    
    
    cat("\n Checking for possible variable(s) with no variance after transformation...\n")
    sd.check <- Zero.sd.check(test.dat.optim.new[,x.var.optim])
    if(is.null(sd.check) == F){stop(" \n Please revise your tranformations. Some of the variable(s) after transformation have zero variance on test data. \n")}   
    
    cat("\n Checking for possible variable(s) with NaN values after transformation...\n")
    Nan.check <- Nan.check(test.dat.optim.new[,x.var.optim])
    if(is.null(Nan.check) == F){stop(" \n Please revise your tranformations. Some of the variable(s) after transformation have NaN values on test data. \n")}
    
    # if(transformation.opt == 1) {
    # train.dat.optim.new <- Feature.engineering(data = train.dat.optim, x.var.optim, Transformation.list[transformation.opt,]) # rep("log.transform",length(x.var.optim))
    # test.dat.optim.new <- Feature.engineering(data = test.dat.optim, x.var.optim, Transformation.list[transformation.opt,]) # rep("log.transform",length(x.var.optim))
    # } else  {
    #   train.dat.optim.new <-  train.dat.optim
    #   test.dat.optim.new <-   test.dat.optim
    # }
    
    
    # Adstock.transformation
    
    if(adstock.optim == T) {
      if(variable.type.optim == "continous") {
        y.metric.adstock = response.variable.optim
      } else { y.metric.adstock = "Xiidra_trx"
      }
      
      adstock.train <-  Adstock.transformation(data = train.dat.optim.new, variables.list = x.var.optim ,
                                               id.variable = id.variable.name.optim, time.period.id = time.period.variable.name.optim,
                                               y.variable = y.metric.adstock, total.time.period = testing.time.period.optim,
                                               bounds.list = adstock.bounds.optim, data.type = "Train data"  )
      train.dat.optim.new <- adstock.train$transformed.data
      train.adstock.constants <- adstock.train$adstock.constants
      
      adstock.test <-  Adstock.transformation(data = test.dat.optim.new, variables.list = x.var.optim ,
                                              id.variable = id.variable.name.optim, time.period.id = time.period.variable.name.optim,
                                              y.variable = y.metric.adstock, total.time.period = testing.time.period.optim,
                                              bounds.list = adstock.bounds.optim, data.type = "Test data" )
      test.dat.optim.new <- adstock.test$transformed.data
      test.adstock.constants <- adstock.test$adstock.constants
      
    } else {
      train.adstock.constants <- NULL
      test.adstock.constants <- NULL
    }
    
    # print("Check1")
    #  print(test.adstock.constants)
    #  print("Check2")
    # Normalizing continous variables (Regression Variables only)
    
    
    if(normalize.data.optim == T ){
      temp.train<- train.dat.optim.new %>% select(one_of(x.var.optim, response.variable.optim))  %>%
        mutate_each(funs(range01),vars= one_of(x.var.optim, response.variable.optim) )
      train.dat.optim.new[,c(x.var.optim,response.variable.optim) ] <- temp.train[(ncol(temp.train) + 1 -
                                                                                     length(c(x.var.optim, response.variable.optim))):ncol(temp.train)]
      
      temp.test<- test.dat.optim.new %>%  select(one_of(x.var.optim, response.variable.optim))   %>%
        mutate_each(funs(range01),vars= one_of(x.var.optim, response.variable.optim) )
      test.dat.optim.new[,c(x.var.optim,response.variable.optim) ] <- temp.test[(ncol(temp.test) + 1 -
                                                                                   length(c(x.var.optim, response.variable.optim))):ncol(temp.test)]
    }
    
    # Defining datasets
    
    set.seed(12345)
    time.period.col.index.train <- which(colnames(train.dat.optim.new) == time.period.variable.name.optim)
    time.period.col.index.test <- which(colnames(test.dat.optim.new) == time.period.variable.name.optim)
    train.data <- train.dat.optim.new
    test.data <-  test.dat.optim.new
    
    # Fitting Model & Checking performance on validation data
    
    model.opt <- Model.fitting.kfold.cv(x.variables = x.var.optim, cutting.variables = cutting.variables.optim,
                                        response.variable = response.variable.optim, data = train.dat.optim,
                                        data.test = test.dat.optim, min.node.size = min.node.size.opt,
                                        data.type="Train.Data", visuals = visuals.optim.int,  #epsilon.value = 1e-4,
                                        save.results = save.results.opt.int,  no.of.folds = folds.optim,
                                        folder.name = folder.name.optim)
    
    model.opt <- c(model.opt, train.adstock.lambdas = list(train.adstock.constants), test.adstock.lambdas = 
                     list(test.adstock.constants))
    return(model.opt)
  }
  
  # Optimization
  
  OPT.Res <- BayesianOptimization(Optimizer.Internal, bounds = bounds.hyperparameters, init_grid_dt = NULL,
                                  init_points = opt.init.seeds, n_iter =  opt.iterations, acq = "ucb", kappa = 1, eps = 0.0,
                                  verbose = TRUE, kernel = list(type= "exponential", power = 2 )) # type= "exponential", power = 2 ,type="matern",nu=5/2
  cat("\n Execution time for optimization (in mins) :",difftime(Sys.time(), start.time,units="mins"),"\n")
  if(save.results.optim == T){
    write.csv(OPT.Res$History,file=paste0("./",folder.name.optim,"/Bayesian.opt.iterations.",response.variable.optim,".csv"),
              row.names = F)
  }
  
  # Running final model on initial train data
  
  #optimized.run <-  Optimizer.Internal(min.node.size.opt = OPT.Res$Best_Par[1], transformation.opt = OPT.Res$Best_Par[2],
  #                                     visuals.optim.int = visuals.optim, save.results.opt.int = save.results.optim )
  optimized.run <-  Optimizer.Internal(min.node.size.opt = OPT.Res$Best_Par[1], ... = OPT.Res$Best_Par[2: (2 + length(x.var.optim) -1)],
                                       visuals.optim.int = visuals.optim, save.results.opt.int = save.results.optim )
  if(adstock.optim == T){
    x.var.optim2 <- x.var.optim[which(!grepl("lag", x.var.optim)==T)]
    adstock.data <- data.table(Data.type = rep(c("Train","Test"),each = length(x.var.optim2)), Variables =
                                 rep(x.var.optim2, 2), Values = 
                                 c(as.numeric(optimized.run$train.adstock.lambdas), 
                                   as.numeric(optimized.run$test.adstock.lambdas)) )
    write.csv(adstock.data, file = paste0("./",folder.name.optim,"/Adstock.constants.",response.variable.optim,".csv"),
              row.names = F)
  }
  
  #a = OPT.Res$Best_Par[2], b = OPT.Res$Best_Par[3],
  # c = OPT.Res$Best_Par[4], d = OPT.Res$Best_Par[5]
  
  cat("\n Best transformation combination on validation data : \n")
  print(Transformation.list[OPT.Res$Best_Par[2: (2 + length(x.var.optim) -1)]])
  cat(" for the variables (in respective order): \n ")
  print(x.var.optim)
  
  transformation.data <- data.table( Variables =  x.var.optim, Values = 
                                       Transformation.list[OPT.Res$Best_Par[2: (2 + length(x.var.optim) -1)]] )
  write.csv(transformation.data, file = paste0("./",folder.name.optim,"/transformations.",response.variable.optim,".csv"),
            row.names = F)
  
  return(list(Best.Parameters = OPT.Res$Best_Par,
              optimized.results = list(optimized.run[[2]],optimized.run[[3]],optimized.run[[4]]),
              optimized.threshold = optimized.run$optimum.cutoff, Final.train.set = optimized.run$Final.train.set.optim,
              Test.set = optimized.run$Test.set.optim, Train.adstock.lambdas = optimized.run$train.adstock.lambdas,
              Test.adstock.lambdas = optimized.run$test.adstock.lambdas))
}




# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5%%%%%%%%%%%%%%% ----
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% DRIVER ANALYSIS FUNCTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



### >>>>>>>>>>>> Function for Driver Analysis -----------------------------

#' Driver.analysis.
#' @description Perform Driver Analysis with Holdout Validation. It takes train and test data as input with the ASSUMPTION that
#' max value of the time period in the test data is 1 more then the max value of the time period in the train data. Initially, this
#'  partitions train data into two sets - Train initial and Validation data (based on the argument 'length.validation.time.period'), 
#'  builds model on the initial train data and tunes hyperparameters (Bayesian Optimization) on the validation data. After this, a 
#'  final model is build using the complete train data (Train inital +  Validation), using the optimized hyperparameters and its
#'   performance is evaluated on this data (complete train) and test data.
#'
#' @param train.df Train data.
#' @param test.df Test data.
#' @param y.metric Dependent Variable.
#' @param regression.var Independent variables to be included for regression.
#' @param splitting.var Independent variables to be included for making data cuts (tree).
#' @param length.validation.time.period Length of the holdout validation time period. By default it is 1.
#' @param optimizer.seeds Number of points to sample for Bayesian Optimization. By default it is 10.
#' @param optimizer.iterations Number of Bayesian Optimization iterations. By default it is 10.
#' @param intercept.inclusion  Logical. Should the impacts of intercept be calculated. By default it is TRUE.
#' @param train.perf.latest.time.period Logical. Should train performance be reported for last time.period only (v/s all time.periods).
#'                                      By default it is TRUE.
#' @param adstock Logical If TRUE, adstock (Koyck) transformation will be performed on the dependent variables selected in
#'               'regression.var' argument. By default it is FALSE
#' @param adstock.bounds Used in case argument 'adstock' is set to TRUE. It is a list of upper and lower bounds for
#'                       transformation constants for each variable. By default the lower and upper bounds are 0 and 1 respectively.
#' @param normalize.data Logical. If TRUE, all the dependent (continous) variables selected in the 'regression.var' argument  will be
#'                       normalized (Z transformed).By default it is FALSE.
#' @param create.buckets Logical. If TRUE, the dependent variable is classified into buckets (categorical variable) and this new
#'                       variable becomes the dependent variable for analysis. By default it is FALSE.
#' @param no.of.buckets Used in case argument 'create.buckets' is set to TRUE. It is the number of buckets to be created.  Currently
#'                      supports 2 buckets only. The split is made by  obtaining the data cuts at 50 th percentile on the training data.
#' @param time.period.variable.name Name of the variable containing the time series values. These can be anything, example - weeks,
#'                                  months, years etc., but should be non negative integers.
#' @param transformations.used A character vector containing different types of transformation to be used (on regression variables only).
#'                            The best set of transformations will be determined by Bayesian Optimization.
#' @param id.variable.name Name of the variable which uniquely identifies set of records pertaining to an ID/Account.
#' @param exposure.vs.effect.plot Logical. If TRUE, Exposure v/s Effectiveness plots are generated. By default it is FALSE.
#' @param size.variable Used in case argument 'exposure.effectiveness' is set to TRUE. It is the variable by which the points' size
#'                       will be scaled. By default it is the y.metric. See \link[LaunchAnalyticZS]{Summary.generator}
#' @param color.variable Used in case argument 'exposure.effectiveness' is set to TRUE. It is the variable by which the points  will be
#'                       colored. By default it is the y.metric. See \link[LaunchAnalyticZS]{Summary.generator}
#' @param visualizations Logical. Should visuals be generated (plots & graphs). By default it is TRUE.
#' @param write.results.to.disk Logical. Should results be saved to disk. By default it is FALSE. If TRUE, a folder name has to
#'                              be specified in the 'folder' argument.
#' @param folder Used in case argument 'write.results.to.disk' is set to TRUE. It is the name of the folder in which the results are saved.
#'
#' @return A list containing the following elements:
#' \item{predicted.train.data}{Train data after appropriate transformations as per 'adstock' argument and optimized hyperparameters.}
#' \item{predicted.test.data}{Test data after appropriate transformations as per 'adstock' argument and optimized hyperparameters.}
#' \item{final.fitted.model}{Fitted GLMTREE model}
#' \item{Summary}{A detailed summary showing the coefficients, p-values, means, predictions and impactables for each segment
#'                 and at an overall level.}
#' \item{Test.performance}{Performance results on test Data. This is cacluated only for latest time period.}
#' @seealso
#' \link[LaunchAnalyticZS]{Linear.evaluation}
#' \link[LaunchAnalyticZS]{Logistic.evaluation}
#' @examples
#'
#' #  Load US States Production Data from plm package
#' data("Produc", package = "plm")
#'  
#' model <- Driver.analysis(train.df = Produc[Produc$year <  max(Produc$year),],  test.df =
#'          Produc[Produc$year <=  max(Produc$year),], y.metric = "unemp", regression.var = c( "util",
#'         "gsp" , "pcap",  "pc"), splitting.var = "region", length.validation.time.period = 5,
#'          adstock = T,  time.period.variable.name = "year", transformations.used =
#'          c("log.transform", "sqroot", "inverse","No.transformation"), id.variable.name = "state", 
#'          exposure.vs.effect.plot = T )
#'
#' @export
Driver.analysis = function(train.df, test.df, y.metric, regression.var, splitting.var, length.validation.time.period = 1,
                           optimizer.seeds = 10, optimizer.iterations = 10, intercept.inclusion = T,
                           train.perf.latest.time.period = T,  adstock = F, adstock.bounds = NULL, normalize.data = F,
                           create.buckets = F, no.of.buckets = 2, time.period.variable.name,  transformations.used =
                             c("log.transform", "negative.exponential", "sigmoid","No.transformation"), id.variable.name,
                           exposure.vs.effect.plot = F, size.variable, color.variable, visualizations = T,
                           write.results.to.disk = F, folder = NULL) {
  
  
  Analysis.start.time <- Sys.time()
  
  Na.check(as.data.frame(train.df))
  Zero.sd.check(as.data.frame(train.df))
  Nan.check(as.data.frame(train.df))
  
  Na.check(as.data.frame(test.df))
  Zero.sd.check(as.data.frame(test.df))
  Nan.check(as.data.frame(test.df))
  
  if(missing(size.variable)) {
    size.variable <- y.metric
  }
  
  if(missing(color.variable)) {
    color.variable <- y.metric
  }
  
  testing.time.period <- max(test.df[,time.period.variable.name])
  
  #  Read data
  
  #if(pipeline.model==F) {
  # full.train.data <- read.csv(train.df)
  # full.test.data <- read.csv(test.df)
  full.train.data <- train.df
  full.test.data <- test.df
  # } else {
  #  full.train.data <- initial.model$predicted.train.data
  #  full.test.data <- initial.model$predicted.test.data
  #}
  
  # full.train.data$FinalSegments=as.factor(full.train.data$FinalSegments)
  # full.train.data$IsWriter=as.factor(full.train.data$IsWriter)
  # full.train.data$lag_IsWriter = as.factor(full.train.data$lag_IsWriter)
  full.train.data[,which(colnames(full.train.data) == id.variable.name)] <-  as.factor(  full.train.data[,
                                                                                                         which(colnames(full.train.data) == id.variable.name)])
  
  # full.test.data$FinalSegments=as.factor(full.test.data$FinalSegments)
  # full.test.data$IsWriter=as.factor(full.test.data$IsWriter)
  # full.test.data$lag_IsWriter = as.factor(full.test.data$lag_IsWriter)
  full.test.data[,which(colnames(full.test.data) == id.variable.name)] <-  as.factor(  full.test.data[,
                                                                                                      which(colnames(full.test.data) == id.variable.name)])
  
  # Bucket Creation
  
  if( create.buckets == T) {
    bucket.thresholds <- quantile(full.train.data[,y.metric], probs = seq(0,1,1/no.of.buckets) )  # c(0,0.5,0.75,1)
    label.names <- c("L",  "H")
    full.train.data[,(ncol(full.train.data) + 1)] <- as.factor(cut(full.train.data[,y.metric], breaks = bucket.thresholds,
                                                                   labels = label.names ,include.lowest = T))
    table(full.train.data[,ncol(full.train.data)])
    colnames(full.train.data)[ncol(full.train.data)] <- paste0("Buckets_",y.metric)
    
    
    full.test.data[,(ncol(full.test.data) + 1)] <- as.factor(cut(full.test.data[,y.metric], breaks = bucket.thresholds,
                                                                 labels = label.names,include.lowest = T ))
    table(full.test.data[,ncol(full.test.data)])
    colnames(full.test.data)[ncol(full.test.data)] <- paste0("Buckets_",y.metric)
    
    y.metric <- paste0("Buckets_",y.metric)
  }
  
  time.period.col.index.train <- which(colnames(full.train.data)== time.period.variable.name)
  time.period.col.index.test <- which(colnames(full.test.data)== time.period.variable.name)
  variable.type <- ifelse(is.numeric(full.train.data[,which(colnames(full.train.data) %in% y.metric)]) == T,
                          "continous","categorical")
  
  # Optimization
  
  set.seed(12345)
  
  multiple.letters <- paste0("Transformation.var",seq(1,1000))
  #c(letters, c(t(outer(letters, letters, paste, sep = ""))))
  
  list.1 <- list(min.node.size.opt = c(eval( parse ( text = paste0(round(nrow(full.train.data)*0.01),"L")) ),
                                       eval( parse ( text = paste0(round(nrow(full.train.data)*0.05),"L")) )))
  # list.2 <- rep( list(c(1L, 4L)), length(regression.var))
  list.2 <- rep( list(c(1L,  eval( parse ( text = (paste0(length(transformations.used),"L"))))  )), length(regression.var))
  names(list.2) <- multiple.letters[1:length(regression.var)]
  
  
  OPT.2 <- Optimizer.External(variable.type.optim = variable.type, opt.init.seeds = optimizer.seeds,
                              opt.iterations = optimizer.iterations,
                              bounds.hyperparameters = c(list.1, list.2),
                              transformations.used.optim = transformations.used,
                              # list(min.node.size.opt = c(eval( parse ( text = paste0(round(nrow(full.train.data)*0.01),"L")) ),
                              #                            eval( parse ( text = paste0(round(nrow(full.train.data)*0.05),"L")) )) ,
                              #  bounds.hyperparameters =  c(bounds.hyperparameters, temp) ,
                              # eval(parse(text = paste0(letters[1]," = c(1L, 4L)" ))))
                              # eval(parse(text =(paste0(letters[1], " = c(1L, 4L)")) )))
                              #  transformation.opt = c(1L, as.integer(eval( parse( text = paste0("4**",length(regression.var),"L"))) ))),  #2L
                              # a = c(1L, 4L),  b = c(1L, 4L), c = c(1L, 4L), d = c(1L, 4L)),  #2L
                              # eval(parse(text =  paste0( letters[1:4], " = c(1L, 4L)"))) )
                              #  noquote(c(list(paste0( letters[1:3], " = c(1L, 4L)", sep = ",")), list(paste0( letters[4], " = c(1L, 4L)"))) ))
                              train.dat.optim = full.train.data, test.dat.optim = full.test.data,
                              x.var.optim = regression.var, cutting.variables.optim = splitting.var,
                              response.variable.optim = y.metric, length.validation.time.period.optim = length.validation.time.period,
                              adstock.optim = adstock,
                              adstock.bounds.optim = adstock.bounds, time.period.variable.name.optim = time.period.variable.name,
                              normalize.data.optim = normalize.data, id.variable.name.optim = id.variable.name,
                              save.results.optim = write.results.to.disk, visuals.optim = visualizations,
                              folder.name.optim = folder) #
  
  
  optimized.threshold.validation <- OPT.2$optimized.threshold
  train.data <-  OPT.2$Final.train.set
  test.data <-  OPT.2$Test.set
  
  time.period.col.index.train.df <- which(colnames(train.data)== time.period.variable.name)
  time.period.col.index.test.df <- which(colnames(test.data) == time.period.variable.name)
  
  train.data <- train.data[train.data[,which(colnames(train.data) == time.period.variable.name)] <= (testing.time.period-1),]
  #test.data <- test.data[test.data[,which(colnames(test.data) == time.period.variable.name)] <= (testing.time.period),]
  test.data <- test.data[test.data[,which(colnames(test.data) == time.period.variable.name)] == (testing.time.period),]
  
  # train.data <- train.data[train.data$Month <= (testing.time.period-1) , ]
  # test.data <- test.data[test.data$Month == testing.time.period , ]
  
  # Building model on complete train data
  
  complete.model <- Model.fitting(x.variables = regression.var, cutting.variables = splitting.var, response.variable = y.metric,
                                  min.node.size = OPT.2$Best.Parameters[1],data = train.data, data.type="Train.Data.final",
                                  visuals = visualizations, save.results = write.results.to.disk, folder.name = folder)
  
  # Evaluating performance on train & test data
  
  if(variable.type=="categorical") {
    
    
    if(train.perf.latest.time.period == F) {
      perf.evaluation.train <- Logistic.evaluation(model.fitted = complete.model$final.model, data = train.data,
                                                   response.variable = y.metric, visuals = visualizations,
                                                   threshold = optimized.threshold.validation,  data.type="Train.Data.final",
                                                   save.results = write.results.to.disk, folder.name = folder)
    } else {
      perf.evaluation.train <- Logistic.evaluation(model.fitted = complete.model$final.model, data = 
                                                     train.data[train.data[,time.period.col.index.train.df] == 
                                                                  max(train.data[,time.period.col.index.train.df]) ,],
                                                   response.variable = y.metric, visuals = visualizations,
                                                   threshold = optimized.threshold.validation,  data.type="Train.Data.final",
                                                   save.results = write.results.to.disk, folder.name = folder)
    }
    
    perf.evaluation.test <- Logistic.evaluation(model.fitted = complete.model$final.model,data = test.data,
                                                response.variable = y.metric, data.type="Test.Data", visuals = visualizations,
                                                threshold = optimized.threshold.validation, save.results = write.results.to.disk,
                                                folder.name = folder)
    
    
  }
  
  if(variable.type=="continous") {
    if(train.perf.latest.time.period == F) {
      perf.evaluation.train <-  Linear.evaluation(model.fitted = complete.model$final.model,data = train.data,
                                                  response.variable = y.metric, visuals = visualizations,
                                                  data.type="Train.Data.final", save.results = write.results.to.disk,
                                                  folder.name = folderl)
    } else {
      perf.evaluation.train <- Linear.evaluation(model.fitted = complete.model$final.model, data = 
                                                   train.data[train.data[,time.period.col.index.train.df] == 
                                                                max(train.data[,time.period.col.index.train.df]) ,],
                                                 response.variable = y.metric,  visuals = visualizations,
                                                 data.type="Train.Data.final", save.results = write.results.to.disk,
                                                 folder.name = folder)
    }
    
    perf.evaluation.test <-Linear.evaluation(model.fitted = complete.model$final.model, data = test.data,
                                             response.variable = y.metric,  data.type="Test.Data",visuals = visualizations,
                                             save.results = write.results.to.disk, folder.name = folder)
  }
  
  # Generating summary on train data
  
  summary.analysis <- Summary.generator(x.variables = regression.var, data = train.data, model.fitted = complete.model,
                                        response.variable = y.metric,data.type="Train.Data.final", visuals = visualizations,
                                        save.results = write.results.to.disk, exposure.effectiveness = exposure.vs.effect.plot,
                                        intercept = intercept.inclusion,  size.var =  size.variable, color.var = color.variable,
                                        folder.name = folder)
  cat("\n Total time for driver analysis (in mins) : \t",difftime(Sys.time(),Analysis.start.time, units="mins"),"\n")
  cat("\n The final model is stored in final.fitted.model.\n The model summary is stored in Summary \n")
  return(list(predicted.train.data = train.data, predicted.test.data = test.data,final.fitted.model = complete.model,
              Summary = summary.analysis[[1]],Test.performance = perf.evaluation.test))
  
}



### >>>>>>>>>>>> Function for Driver Analysis using paralle CV -----------------------------


#' Driver.analysis.kfold.cv.
#' @description Perform Driver Analysis with Cross Validation (k folds). It takes an initial data as input. Initially, this partitions
#'  train data into two sets - Train and Test data (based on 'split.ratio' arfument), builds model on (k-1) folds of train data and tunes hyperparameters
#'  (Bayesian Optimization) on the remaining 1 fold of the train data. After this, a final model is build using the complete train data
#'  (all folds), using the optimized hyperparameters and its performance is evaluated on this data (complete train) and test data.
#'
#' @param train.df Dataset to be analysed.
#' @param y.metric Dependent Variable.
#' @param regression.var Independent variables to be included for regression.
#' @param splitting.var Independent variables to be included for making data cuts (tree).
#' @param folds Value of k for k cross fold validation.By default it is 5.
#' @param split.ratio Ratio for splitting the inital dataset into train data and test data. By default it is 0.8.
#' @param optimizer.seeds Number of points to sample for Bayesian Optimization. By default it is 10.
#' @param optimizer.iterations Number of Bayesian Optimization iterations. By default it is 10.

#' @param intercept.inclusion  Logical. Should the impacts of intercept be calculated. By default it is TRUE.
#' @param adstock Logical If TRUE, adstock (Koyck) transformation will be performed on the dependent variables selected in
#'               'regression.var' argument. By default it is FALSE.
#' @param adstock.bounds Used in case argument 'adstock' is set to TRUE. It is a list of upper and lower bounds for
#'                       transformation constants for each variable. By default the lower and upper bounds are 0 and 1 respectively.
#' @param normalize.data Logical. If TRUE, all the dependent (continous) variables selected in the 'regression.var' argument  will be
#' @param create.buckets Logical. If TRUE, the dependent variable is classified into buckets (categorical variable) and this new
#'                       variable becomes the dependent variable for analysis. By default it is FALSE.
#' @param no.of.buckets Used in case argument 'create.buckets' is set to TRUE. It is the number of buckets to be created.  Currently
#'                      supports 2 buckets only. The split is made by  obtaining the data cuts at 50 th percentile on the training data.
#' @param time.period.variable.name Name of the variable containing the time series values.
#'                       normalized (Z transformed).By default it is FALSE.
#' @param transformations.used A character vector containing different types of transformation to be used (on regression variables only).
#'                            The best set of transformations will be determined by Bayesian Optimization.
#' @param id.variable.name  Name of the variable which uniquely identifies set of records pertaining to an ID/Account.
#' @param exposure.vs.effect.plot Logical. If TRUE, Exposure v/s Effectiveness plots are generated. By default it is FALSE.
#' @param size.variable Used in case argument 'exposure.effectiveness' is set to TRUE. It is the variable by which the points' size
#'                       will be scaled. By default it is the y.metric. See \link[LaunchAnalyticZS]{Summary.generator}
#' @param color.variable Used in case argument 'exposure.effectiveness' is set to TRUE. It is the variable by which the points  will be
#'                       colored. By default it is the y.metric. See \link[LaunchAnalyticZS]{Summary.generator}
#' @param visualizations Logical. Should visuals be generated (plots & graphs). By default it is TRUE.
#' @param write.results.to.disk Logical. Should results be saved to disk. By default it is FALSE. If TRUE, a folder name has to
#'                              be specified in the 'folder' argument.
#' @param folder Used in case argument 'write.results.to.disk' is set to TRUE. It is the name of the folder in which the results are saved.
#'
#' @return A list containing the following elements:
#' \item{predicted.train.data}{Train data after appropriate transformations as per 'adstock' argument and optimized hyperparameters.}
#' \item{predicted.test.data}{Test data after appropriate transformations as per 'adstock' argument and optimized hyperparameters.}
#' \item{final.fitted.model}{Fitted GLMTREE model}
#' \item{Summary}{A detailed summary showing the coefficients, p-values, means, predictions and impactables for each segment
#'                 and at an overall level.}
#' \item{Test.performance}{Performance results on test Data}
#' @seealso
#' \link[LaunchAnalyticZS]{Linear.evaluation}
#' \link[LaunchAnalyticZS]{Logistic.evaluation}
#' @examples
#'
#' #  Load US States Production Data from plm package
#' data("Produc", package = "plm")
#'
#' model <- Driver.analysis.kfold.cv( train.df = Produc, y.metric = "unemp", regression.var =
#'         c("util","gsp"), splitting.var = "region",adstock = F, time.period.variable.name = "year",
#'          transformations.used = c("log.transform", "sqroot", "No.transformation"), 
#'          id.variable.name = "state", exposure.vs.effect.plot = T )
#' @export
Driver.analysis.kfold.cv = function(train.df, y.metric, regression.var, splitting.var, folds = 5, split.ratio = 0.8,
                                    optimizer.seeds = 10, optimizer.iterations = 10, intercept.inclusion = T, adstock = F,
                                    adstock.bounds = NULL,  normalize.data = F, create.buckets = F, no.of.buckets = 2,
                                    time.period.variable.name,  transformations.used = c("log.transform", "negative.exponential",
                                                                                         "sigmoid","No.transformation"), id.variable.name, exposure.vs.effect.plot = F,
                                    size.variable , color.variable, visualizations = F, write.results.to.disk = F, folder = NULL) {
  
  Analysis.start.time <- Sys.time()
  
  Na.check(as.data.frame(train.df))
  Zero.sd.check(as.data.frame(train.df))
  Nan.check(as.data.frame(train.df))
  
  registerDoParallel(makeCluster(4))
  
  
  if(missing(size.variable)) {
    size.variable <- y.metric
  }
  
  if(missing(color.variable)) {
    color.variable <- y.metric
  }
  #  Read data
  
  #  if(pipeline.model==F) {
  # full.train.data <- read.csv(train.df)
  full.train.data <- train.df
  # }
  
  #  if(pipeline.model==T) {
  #   full.train.data <- initial.model$All.data
  # }
  
  # full.train.data$FinalSegments=as.factor(full.train.data$FinalSegments)
  # full.train.data$IsWriter=as.factor(full.train.data$IsWriter)
  # full.train.data$lag_IsWriter = as.factor(full.train.data$lag_IsWriter)
  full.train.data[,which(colnames(full.train.data) == id.variable.name)] <-  as.factor(  full.train.data[,which(colnames(full.train.data) == id.variable.name)])
  #
  # phys.list.with.decile <- full.train.data %>% select(Shire_ID, Restasis_decile_group) %>% group_by(Shire_ID) %>% slice(1)
  # partition <- createDataPartition(phys.list.with.decile$Restasis_decile_group, list = FALSE, p=0.8)
  #
  # test.phys <- phys.list.with.decile$Shire_ID[-partition]
  # train.phys <- phys.list.with.decile$Shire_ID[partition]
  #
  # full.test.data <-  full.train.data[which(full.train.data$Shire_ID %in% test.phys),]
  # full.train.data <- full.train.data[which(full.train.data$Shire_ID %in% train.phys),]
  
  phys.list.with.decile <- as.data.frame(full.train.data %>% select(one_of(id.variable.name, y.metric)) %>% group_by_(id.variable.name) %>% slice(1))
  partition <- createDataPartition(phys.list.with.decile[,which(colnames(phys.list.with.decile) == y.metric)], list = FALSE, p = split.ratio)
  
  test.phys <-  phys.list.with.decile[,which(colnames(phys.list.with.decile) == id.variable.name)][-partition]
  train.phys <-  phys.list.with.decile[,which(colnames(phys.list.with.decile) == id.variable.name)][partition]
  
  full.test.data <-  full.train.data[ which(full.train.data[,which(colnames(full.train.data) == id.variable.name)] %in% test.phys),]
  full.train.data <- full.train.data[ which( full.train.data[,which(colnames(full.train.data) == id.variable.name)] %in% train.phys),]
  
  
  
  # Bucket Creation
  
  if( create.buckets == T) {
    bucket.thresholds <- quantile(full.train.data[,y.metric], probs = seq(0,1,1/no.of.buckets) )  # c(0,0.5,0.75,1)
    label.names <- c("L",  "H")
    full.train.data[,(ncol(full.train.data) + 1)] <- as.factor(cut(full.train.data[,y.metric] , breaks = bucket.thresholds,
                                                                   labels = label.names ,include.lowest = T))
    table(full.train.data[,ncol(full.train.data)])
    colnames(full.train.data)[ncol(full.train.data)] <- paste0("Buckets_",y.metric)
    
    full.test.data[,(ncol(full.test.data) + 1)] <- as.factor(cut(full.test.data[,y.metric], breaks = bucket.thresholds,
                                                                 labels = label.names,include.lowest = T ))
    table(full.test.data[,ncol(full.test.data)])
    colnames(full.test.data)[ncol(full.test.data)] <- paste0("Buckets_",y.metric)
    y.metric <- paste0("Buckets_",y.metric)
    
  }
  
  time.period.col.index.train <- which(colnames(full.train.data) == time.period.variable.name)
  time.period.col.index.test <- which(colnames(full.test.data) == time.period.variable.name)
  
  
  # Setting up Parallelization
  
  set.seed(12345)
  variable.type <- ifelse(is.numeric(full.train.data[,which(colnames(full.train.data) %in% y.metric)]) == T,
                          "continous","categorical")
  
  # Optimization
  
  set.seed(12345)
  
  multiple.letters <- paste0("Transformation.var",seq(1,1000))
  #c(letters, c(t(outer(letters, letters, paste, sep = ""))))
  
  list.1 <- list(min.node.size.opt = c(eval( parse ( text = paste0(round(nrow(full.train.data)*0.01),"L")) ),
                                       eval( parse ( text = paste0(round(nrow(full.train.data)*0.05),"L")) )))
  #list.2 <- rep( list(c(1L, 4L)), length(regression.var))
  list.2 <- rep( list(c(1L,  eval( parse ( text = (paste0(length(transformations.used),"L"))))  )), length(regression.var))
  
  names(list.2) <- multiple.letters[1:length(regression.var)]
  
  OPT.2 <- Optimizer.External.kfold.cv(variable.type.optim = variable.type, opt.init.seeds = optimizer.seeds,
                                       opt.iterations = optimizer.iterations,
                                       bounds.hyperparameters = c(list.1, list.2),
                                       transformations.used.optim = transformations.used,
                                       #   list(min.node.size.opt = c(eval( parse ( text = paste0(round(nrow(full.train.data)*0.01),"L")) ),
                                       #                              eval( parse ( text = paste0(round(nrow(full.train.data)*0.05),"L")) )),
                                       #        transformation.opt = c(1L, as.integer(eval( parse( text = paste0("4**",length(regression.var),"L"))) ))),  #2L
                                       train.dat.optim = full.train.data, test.dat.optim = full.test.data,
                                       x.var.optim = regression.var, folds.optim = folds,
                                       cutting.variables.optim = splitting.var, response.variable.optim = y.metric,
                                       adstock.optim = adstock, adstock.bounds.optim = adstock.bounds,
                                       time.period.variable.name.optim = time.period.variable.name, normalize.data.optim = normalize.data,
                                       id.variable.name.optim = id.variable.name, save.results.optim =  write.results.to.disk,
                                       visuals.optim = visualizations, folder.name.optim = folder) #write.results.to.disk
  
  # Building final model on train data
  
  optimized.threshold.validation <- OPT.2$optimized.threshold
  train.data <-  OPT.2$Final.train.set
  test.data <-  OPT.2$Test.set
  
  time.period.col.index.train.df <- which(colnames(train.data)== time.period.variable.name)
  time.period.col.index.test.df <- which(colnames(test.data) == time.period.variable.name)
  
  complete.model <- Model.fitting(x.variables = regression.var,  cutting.variables = splitting.var,
                                  response.variable = y.metric, min.node.size = OPT.2$Best.Parameters[1], data = train.data,
                                  data.type ="Train.Data.final",visuals = visualizations, save.results = write.results.to.disk,
                                  folder.name = folder)
  
  # Evaluating performance on train & test data
  
  if(variable.type=="categorical")
  {
    perf.evaluation.train <- Logistic.evaluation(model.fitted = complete.model$final.model,data = train.data,
                                                 response.variable = y.metric, visuals = visualizations,
                                                 threshold = optimized.threshold.validation, data.type ="Train.Data.final",
                                                 save.results = write.results.to.disk, folder.name = folder)
    
    perf.evaluation.test <- Logistic.evaluation(model.fitted = complete.model$final.model, data = test.data,
                                                response.variable = y.metric, data.type = "Test.Data", visuals = visualizations,
                                                threshold = optimized.threshold.validation, save.results = write.results.to.disk,
                                                folder.name = folder)
  }
  
  if(variable.type=="continous") {
    perf.evaluation.train <-  Linear.evaluation(model.fitted = complete.model$final.model,data = train.data,
                                                response.variable = y.metric, visuals = visualizations,
                                                data.type ="Train.Data.final",save.results = write.results.to.disk, folder.name = folder)
    
    perf.evaluation.test <- Linear.evaluation(model.fitted = complete.model$final.model, data = test.data,
                                              response.variable = y.metric, data.type ="Test.Data",visuals = visualizations,
                                              save.results = write.results.to.disk, folder.name = folder)
  }
  
  # Generating summary on train data
  
  summary.analysis <- Summary.generator(x.variables = regression.var, data = train.data, model.fitted = complete.model,
                                        response.variable = y.metric,data.type="Train.Data.final",visuals = visualizations,
                                        save.results = write.results.to.disk, intercept = intercept.inclusion,
                                        exposure.effectiveness = exposure.vs.effect.plot, size.var =  size.variable,
                                        color.var = color.variable, folder.name = folder)
  
  cat("\n Total time for driver analysis (in mins) : \t",difftime(Sys.time(),Analysis.start.time, units="mins"),"\n")
  cat("\n The final model is stored in final.fitted.model.\n The model summary is stored in Summary \n")
  return(list(predicted.train.data = train.data, predicted.test.data = test.data,
              final.fitted.model = complete.model,Summary = summary.analysis[[1]],
              Test.performance = perf.evaluation.test))
}





### >>>>>>>>>>>> Function for Driver Analysis for sparse data --------------------------

#' Driver.analysis.with.sparse.data.
#' @description Perform Driver Analysis for one variable without any validation and optimization. This is helpful when number of
#' observations are very less.
#'
#' @param train.df Train data.
#' @param test.df Test data.
#' @param y.metric Dependent Variable.
#' @param regression.var Independent variables to be included for regression.
#' @param splitting.var Independent variables to be included for making data cuts (tree).
#' @param intercept.inclusion   Logical. Should the impacts of intercept be calculated. By default it is TRUE.
#' @param normalize.data Logical. If TRUE, all the dependent (continous) variables selected in the 'regression.var' argument  will be
#'                       normalized (Z transformed).By default it is FALSE.
#' @param time.period.variable.name Name of Variable containing the time series values.
#' @param id.variable.name  Name of the variable which uniquely identifies set of records pertaining to an ID/Account.
#' @param create.buckets Logical. If TRUE, the dependent variable is classified into buckets (categorical variable) and this new
#'                       variable becomes the dependent variable for analysis. By default it is FALSE.
#' @param no.of.buckets Used in case argument 'create.buckets' is set to TRUE. It is the number of buckets to be created.  Currently
#'                      supports 2 buckets only. The split is made by  obtaining the data cuts at 50 th percentile on the training data.
#' @param exposure.vs.effect.plot Logical. If TRUE, Exposure v/s Effectiveness plots are generated. By default it is FALSE.
#' @param size.variable Used in case argument 'exposure.effectiveness' is set to TRUE. It is the variable by which the points' size
#'                       will be scaled. By default it is the y.metric. See \link[LaunchAnalyticZS]{Summary.generator}
#' @param color.variable Used in case argument 'exposure.effectiveness' is set to TRUE. It is the variable by which the points  will be
#'                       colored. By default it is the y.metric. See \link[LaunchAnalyticZS]{Summary.generator}
#' @param visualizations  Logical. Should visuals be generated (plots & graphs). By default it is TRUE.
#' @param write.results.to.disk Logical. Should results be saved to disk. By default it is FALSE. If TRUE, a folder name has to
#'                              be specified in the 'folder' argument.
#' @param folder Used in case argument 'write.results.to.disk' is set to TRUE. It is the name of the folder in which the results are saved.
#' 
#' @return A list containing the following elements:
#' \item{predicted.train.data}{Train data after appropriate transformations as per 'adstock' argument and optimized hyperparameters.}
#' \item{predicted.test.data}{Test data after appropriate transformations as per 'adstock' argument and optimized hyperparameters.}
#' \item{final.fitted.model}{Fitted GLMTREE model}
#' \item{Summary}{A detailed summary showing the coefficients, p values, means, predictions and impactables for each segment
#'                 and at an overall level}
#' \item{Test.performance}{Performance results on Test Data}
#' @seealso
#' \link[LaunchAnalyticZS]{Linear.evaluation}
#' \link[LaunchAnalyticZS]{Logistic.evaluation}
#' @examples
#'
#' #  Load US States Production Data from plm package
#' data("Produc", package = "plm")
#'
#' model <- Driver.analysis.with.sparse.data(train.df = Produc[Produc$year <  max(Produc$year),],
#'          test.df = Produc[Produc$year ==  max(Produc$year),], y.metric = "unemp", regression.var
#'           = c("pcap", "hwy","water","util", "pc", "gsp","emp"), splitting.var =  "region",
#'          time.period.variable.name = "year", id.variable.name = "state", 
#'          exposure.vs.effect.plot = T )
#' @export
Driver.analysis.with.sparse.data = function(train.df, test.df, y.metric, regression.var, splitting.var,intercept.inclusion = F, 
                                            normalize.data = F,  time.period.variable.name, id.variable.name,
                                            create.buckets = F, no.of.buckets = 2, exposure.vs.effect.plot = F, size.variable,
                                            color.variable ,   visualizations = T, write.results.to.disk = F, folder = NULL) {
  
  Analysis.start.time <- Sys.time()
  
  Na.check(as.data.frame(train.df))
  Zero.sd.check(as.data.frame(train.df))
  Nan.check(as.data.frame(train.df))
  
  Na.check(as.data.frame(test.df))
  Zero.sd.check(as.data.frame(test.df))
  Nan.check(as.data.frame(test.df))
  
  if(missing(size.variable)) {
    size.variable <- y.metric
  }
  
  if(missing(color.variable)) {
    color.variable <- y.metric
  }
  
  ###  Read data
  
  #  full.train.data <- read.csv(train.df)
  # full.test.data <- read.csv(test.df)
  full.train.data <- train.df
  full.test.data <- test.df
  
  # full.train.data$FinalSegments <- as.factor(full.train.data$FinalSegments)
  # full.test.data$FinalSegments <- as.factor(full.test.data$FinalSegments)
  
  
  
  ### Building Final Model
  
  set.seed(12345)
  variable.type <- ifelse(is.numeric(full.train.data[,which(colnames(full.train.data) %in% y.metric)])==T,
                          "continous","categorical")
  
  
  
  # Bucket Creation
  
  if( create.buckets == T) {
    bucket.thresholds <- quantile(full.train.data[,y.metric], probs = seq(0,1,1/no.of.buckets) )  # c(0,0.5,0.75,1)
    label.names <- c("L",  "H")
    full.train.data[,(ncol(full.train.data) + 1)] <- as.factor(cut(full.train.data[,y.metric], breaks = bucket.thresholds,
                                                                   labels = label.names ,include.lowest = T))
    table(full.train.data[,ncol(full.train.data)])
    colnames(full.train.data)[ncol(full.train.data)] <- paste0("Buckets_",y.metric)
    
    
    full.test.data[,(ncol(full.test.data) + 1)] <- as.factor(cut(full.test.data[,y.metric], breaks = bucket.thresholds,
                                                                 labels = label.names,include.lowest = T ))
    table(full.test.data[,ncol(full.test.data)])
    colnames(full.test.data)[ncol(full.test.data)] <- paste0("Buckets_",y.metric)
    
    y.metric <- paste0("Buckets_",y.metric)
  }
  
  
  
  train.data <- full.train.data
  test.data <- full.test.data
  
  if(normalize.data == T ){
    temp.train<- train.data %>%   select(one_of(x.var.optim, response.variable.optim))   %>%
      mutate_each(funs(range01),vars= one_of(x.var.optim, response.variable.optim) )
    train.data[,c(x.var.optim,response.variable.optim) ] <- temp.train[(ncol(temp.train) + 1 - length(c(x.var.optim,
                                                                                                        response.variable.optim))):ncol(temp.train)]
    
    temp.test<- test.data %>%   select(one_of(x.var.optim, response.variable.optim))   %>%
      mutate_each(funs(range01),vars= one_of(x.var.optim, response.variable.optim) )
    test.data[,c(x.var.optim,response.variable.optim) ] <- temp.test[(ncol(temp.test) + 1 - length(c(x.var.optim,
                                                                                                     response.variable.optim))):ncol(temp.test)]
  }
  
  
  
  ## Modeling
  
  # Building model on train data
  
  complete.model <- Model.fitting(x.variables = regression.var,
                                  cutting.variables = splitting.var,
                                  response.variable = y.metric ,
                                  min.node.size = 10 ,data=train.data,
                                  data.type="Train.Data.final", visuals = visualizations,
                                  save.results = write.results.to.disk, folder.name = folder)
  
  if(variable.type=="categorical")
  {
    # Evaluating performance on train data
    
    perf.evaluation.train <-Logistic.evaluation(model.fitted = complete.model$final.model,
                                                data=train.data,response.variable = y.metric,
                                                visuals = visualizations,threshold = optimized.threshold.validation,
                                                data.type="Train.Data.final", save.results=write.results.to.disk,
                                                folder.name = folder)
    
    # Evaluating performance on test data
    
    perf.evaluation.test <- Logistic.evaluation(model.fitted = complete.model$final.model,
                                                data=test.data,response.variable = y.metric,
                                                data.type="Test.Data",visuals = visualizations,
                                                threshold = optimized.threshold.validation,
                                                save.results=write.results.to.disk, folder.name = folder)
  }
  
  
  if(variable.type=="continous") {
    
    # Evaluating performance on train data
    
    
    perf.evaluation.train <-  Linear.evaluation(model.fitted = complete.model$final.model,
                                                data=train.data,response.variable = y.metric,
                                                visuals = visualizations,data.type="Train.Data.final",
                                                save.results=write.results.to.disk, folder.name = folder)
    
    # Evaluating performance on test data
    
    perf.evaluation.test <-Linear.evaluation(model.fitted = complete.model$final.model,
                                             data=test.data,response.variable = y.metric,
                                             data.type="Test.Data",  visuals = visualizations,
                                             save.results=write.results.to.disk, folder.name = folder)
  }
  
  # Generating summary on train data
  
  summary.analysis <- Summary.generator(x.variables = regression.var, data = train.data, model.fitted = complete.model,
                                        response.variable = y.metric,data.type = "Train.Data.final", visuals = visualizations,
                                        save.results = write.results.to.disk, exposure.effectiveness = exposure.vs.effect.plot,
                                        intercept = intercept.inclusion, size.var =  size.variable, color.var = color.variable,
                                        folder.name = folder)
  
  
  
  cat("\n Total time for driver analysis (in mins) : \t",difftime(Sys.time(),Analysis.start.time, units="mins"),"\n")
  cat("\n The final model is stored in final.fitted.model.\n The model summary is stored in Summary \n")
  return(list(predicted.train.data = train.data, predicted.test.data = test.data,final.fitted.model = complete.model,
              Summary = summary.analysis[[1]],Test.performance = perf.evaluation.test))
}






Summary.generator_V02 <- function (data, model.fitted, response.variable, x.variables,  intercept = T, exposure.effectiveness = F,
                                   size.var , color.var , save.results = F,  folder.name = NULL, data.type)
{
  
  variable.type <- ifelse(is.numeric(data[,which(colnames(data) %in% response.variable)])==T,
                          "continous","categorical")
  if(missing(size.var)) {
    size.var <- response.variable
  }
  
  if(missing(color.var)) {
    color.var <- response.variable
  }
  
  # Adding predictions to the  data
  
  predictions.data <- as.data.frame(cbind(data, "Predicted Value" = predict(model.fitted,data),
                                          "Node"= predict(model.fitted, data,type = "node")))
  coef.data <- coef(model.fitted)
  coef.data[(is.na(coef.data))] <- 0
  if(variable.type == "categorical") {
    coef.data <- exp(coef.data)
  }
  single.node = ifelse(is.null(nrow(coef.data)) == T, T, F)
  unique.nodes <- ifelse(single.node == F, length(unique(predictions.data$Node)), 1)
  
  # Generating empty summary
  
  model.summary <- as.data.frame(matrix(0,ncol = (4 + 5*length(x.variables) + 1 + 3),nrow = unique.nodes))
  x.variables.with.intercept=c("(Intercept)",x.variables)
  p.value.column <- paste0("P.value.",x.variables.with.intercept)
  Mean.value.column <- paste0("Mean.",x.variables.with.intercept)
  pred.columns <- paste0("Prediction.",x.variables.with.intercept)
  pred.columns.all <- c(pred.columns,"Prediction.from.model")
  perc.columns <- paste0("Percentage.Impact.",x.variables.with.intercept)
  
  colnames(model.summary)=c("rule","Observations",x.variables.with.intercept,p.value.column,
                            Mean.value.column,pred.columns.all,perc.columns)
  
  if (single.node == T) {
    model.summary$rule=1
  } else {
    if(is.numeric(unique(predictions.data$Node))){
      model.summary$rule <- sort(as.numeric(unique(predictions.data$Node)))
    } else {
      model.summary$rule <- unique(predictions.data$Node)
    }
    
      #sort(as.numeric(unique(predictions.data$Node)))
  }
  
  summary.model.fitteds <- summary(model.fitted)
  no.of.rules <- unique.nodes
  model.summary$Observations <- as.vector(table(predictions.data$Node))
  
  # Taking required Nodes and corresponding coefficients, pvalues
  
  if(single.node == T) {
    all.nodes <- 1
    coef.data <- as.data.frame(t(coef.data))
  } else {
    all.nodes <- as.numeric(unlist(dimnames(coef(model.fitted))[1]))
    coef.data <- as.data.frame(coef.data)
  }
  
  coef.data[,(ncol(coef.data) +1 )] <- all.nodes
  required.nodes <- all.nodes[which(all.nodes %in% as.numeric(unique(predictions.data$Node)))]
  coef.data <- coef.data[which(coef.data[,ncol(coef.data)] %in% required.nodes),]
  
  # Adding betas * X in data at physician level
  
  coef.data2 <- coef.data
  colnames(coef.data2)[ncol(coef.data2)] <- "Node"
  predictions.data.frame2 <-  predictions.data %>% left_join(coef.data2, by = "Node")
  predictions.data.frame3 <-  predictions.data.frame2[ , c(paste0(x.variables,".x"))] *
    predictions.data.frame2[,  c(paste0(x.variables,".y"))]
  predictions.data.frame2 <- cbind(predictions.data.frame2,predictions.data.frame2$`(Intercept)`, predictions.data.frame3)
  
  colnames(predictions.data.frame2)[(ncol(predictions.data.frame2) - length(x.variables.with.intercept) + 1):(ncol(predictions.data.frame2))] <-
    paste0("predicted_",x.variables.with.intercept)
  
  coef.data <- coef.data[,-ncol(coef.data)]
  model.summary[,3: (3 + length(x.variables) )] <-  coef.data
  
  ###################################################################################################################
  ## Adding % impacts at MD level  
  
  pred.columns_perc_impact <- c(paste0("predicted_", x.variables.with.intercept))
  
  predictions.data.frame_total_pred <- as.data.table(predictions.data.frame2)
  
  predictions.data.frame_total_pred[, Prediction.from.model := Reduce(`+`, .SD), .SD = pred.columns_perc_impact]
  
  predictions.data.frame_perc_impact <- predictions.data.frame_total_pred[,..pred.columns_perc_impact]/predictions.data.frame_total_pred[,Prediction.from.model]
  colnames(predictions.data.frame_perc_impact) <- c(paste0("Perc_Impact_",pred.columns_perc_impact))
  
  predictions.data.frame_perc_impact <- cbind(predictions.data.frame2, predictions.data.frame_perc_impact)
  
  ###################################################################################################################
  ###################################################################################################################
  
  # P values
  
  if (single.node == T) {
    model.summary[,p.value.column] <-  summary.model.fitteds$coefficients[,4]
  } else {
    p.values <- lapply(names(summary.model.fitteds), function(x) as.data.table(t(summary.model.fitteds[[x]]$coefficients[,4])))
    p.values <-  as.data.frame(rbindlist(p.values,fill = T)  )
    p.values[,(ncol(p.values) + 1 )] <- all.nodes
    p.values <- p.values[which(p.values[,ncol(p.values)] %in% required.nodes),]
    p.values <- p.values[,-ncol(p.values)]
    
    model.summary[,p.value.column] <-  p.values
  }
  
  # Adding Means
  
  predictions.data <- as.data.table(predictions.data)
  if(intercept == F) {
    model.summary[,Mean.value.column] <- cbind(0, (setorder(predictions.data[,lapply(.SD, mean, na.rm=TRUE),
                                                                             by=Node,.SDcols=x.variables],Node)[,-1]))
  } else {
    model.summary[,Mean.value.column] <- cbind(1, (setorder(predictions.data[,lapply(.SD, mean, na.rm=TRUE),
                                                                             by=Node,.SDcols=x.variables],Node)[,-1]))
  }
  
  # Adding Predictions from variables
  
  model.summary[,pred.columns] <-    model.summary[, x.variables.with.intercept ] * model.summary[, Mean.value.column]
  model.summary$Prediction.from.model = rowSums(model.summary[pred.columns])

  
  # Adding % impact
  
  model.summary[,perc.columns] <-   model.summary[, pred.columns ]/model.summary$Prediction.from.model
  model.summary[(is.na(model.summary))] <- 0
  
  # Adding overall % impact
  
  if (single.node == F) {
    new.row <- nrow(model.summary) + 1
    model.summary[ new.row,perc.columns] <- (colSums(model.summary$Observations * model.summary[ ,perc.columns ])  /
                        sum(model.summary$Observations[unique(which(model.summary[ ,perc.columns] != 0 , arr.ind =  T)[,1])]))
                                               #sum(model.summary$Observations))
    model.summary[ new.row, pred.columns.all] <-  colSums(model.summary[(1:no.of.rules),pred.columns.all])
    model.summary$Observations[nrow(model.summary)] <- sum(model.summary$Observations[1:no.of.rules])
  }
  
  model.summary$rule[nrow(model.summary)] <-  "Total"
  colnames(model.summary)[which(colnames(model.summary)=="rule")] <- "Node"
  
  
  # Adding Y  mean & % contribution
  
  size.variable.type <- ifelse(is.numeric(data[,which(colnames(data) %in% size.var)])==T,
                               "continous","categorical")
  color.variable.type <- ifelse(is.numeric(data[,which(colnames(data) %in% color.var)])==T,
                                "continous","categorical")
  
  if(size.variable.type == "continous") {
    model.summary$Mean.y <- c(eval(parse(text = paste0("(setorder(predictions.data[,lapply(.SD, mean, na.rm=TRUE),
                                                       by=Node,.SDcols=size.var],Node)[,-1])$",size.var) )),  mean(data[,size.var]) )
  } else {
    temp.df <- predictions.data[predictions.data[,which(colnames(predictions.data) %in% size.var)] ==
                                  levels(data[,which(colnames(data) %in% size.var)])[2],]
    model.summary$Mean.y <- c(eval(parse(text =   paste0("(setorder(temp.df[,lapply(.SD, length),
                                                         by=Node,.SDcols=size.var],Node)[,-1])$",size.var) )),  nrow(temp.df) ) /
      c(eval(parse(text =   paste0("(setorder(predictions.data[,lapply(.SD, length),
                                   by=Node,.SDcols=size.var],Node)[,-1])$",size.var) )),  nrow(predictions.data) )
  }
  
  
  if(color.variable.type == "continous") {
    model.summary$Contribution.y <- c(eval(parse(text =   paste0("(setorder(predictions.data[,lapply(.SD, sum,
                                                                 na.rm=TRUE),by=Node,.SDcols=color.var],Node)[,-1] / sum(data[,color.var]) )$",color.var) )),1)
  } else {
    
    model.summary$Contribution.y <- c(eval(parse(text =   paste0("(setorder(temp.df[,lapply(.SD, length),
                                                                 by=Node,.SDcols=color.var],Node)[,-1])$",color.var) )),  nrow(temp.df) ) /
      length(which(predictions.data[,.SD,.SDcols = color.var] ==
                     levels(data[,color.var])[2] ))
  }
  
  model.summary[(is.na(model.summary))] <- 0
  
  if(save.results==T){
    write.csv(model.summary,file=paste0("./",folder.name,"/Model.summary.",data.type,".", response.variable,".csv"),row.names = F)
    write.csv(predictions.data.frame_perc_impact,file=paste0("./",folder.name,"/Data.with.predictions.",data.type,".",response.variable,
                                                             ".csv"),row.names = F)
    save(model.fitted,file = paste0("./",folder.name,"/Fitted.Model.",data.type,".",response.variable,"..Rda"))
  }
  
  if(exposure.effectiveness==T){
    if(save.results==T) {jpeg(paste(filename=paste0("./",folder.name,"/Exposure.vs.Effectiveness.plot.",data.type,".",
                                                    response.variable,".jpeg")),width = 2000, height = 1000,res = 80,quality=480,pointsize = 14)
    }
    plot(Exposure.vs.effect.plotter(data.summary =  model.summary,  x.var = x.variables,color.var = "Contribution.y",
                                    size.var = "Mean.y",label.var = "Node" ))
    #mtext(paste0("Exposure.vs.Effectiveness.plot. for ",data.type), side = 3, line = -2, outer = TRUE)
    
    if(save.results == T) {
      dev.off()
    }
  }
  
  print(nrow(predictions.data.frame_perc_impact))
  return(list(model.summary ,as.data.frame(predictions.data), as.data.frame(predictions.data.frame_perc_impact)))
  
  }







library(excel.link)

exposure_group <- function(data,x_vars,beta_vars,size_var,colour_var,group_col,names_dict,filesavepath)
  
{
  
  eval(parse(text = paste('data1 = data[,lapply(.SD,mean), .SDcols = c(x_vars,beta_vars,size_var,colour_var),by = ',group_col,']',sep = "")))
  data1 <- cbind(data1, 'N' = as.vector(table(data[, ..group_col])))  ### Added later
  xl.workbook.add()
  lop<-list()
  for (lindex in 1:length(x_vars)) {
    
    # eval(parse(text = paste('med.y = median(data1[,',x_vars[lindex],'])',sep = "")))   #  Initial axis
    # eval(parse(text = paste('med.x = median(data1[,',beta_vars[lindex],'])',sep = "")))
    
    # eval(parse(text = paste('med.y = data1[,',x_vars[lindex],'] %*% data1[, N] / sum(data1[, N])',sep = "")))  Getting different
    #number from current -- probably due to mismatching of indices in matrix multiplication
    #  eval(parse(text = paste('med.x = data1[,',beta_vars[lindex],'] %*% data1[, N] / sum(data1[, N])',sep = "")))
    
    eval(parse(text = paste('med.y = mean(data[,',x_vars[lindex],'])',sep = "")))
    eval(parse(text = paste('med.x = mean(data[,',beta_vars[lindex],'])',sep = "")))
    
    plot <- ggplot(data1,aes_string(x = beta_vars[lindex], y= x_vars[lindex])) + geom_point(aes_string(size = size_var, color = colour_var) )+ 
      geom_text(aes_string(label = group_col), size= 5,color = "white") +
      labs(x = "Betas" ,y = "Means") + ggtitle(paste(names_dict[x_vars[lindex]],sep = " ")) + scale_size_continuous(range = c(8,18)) +  #c(10,20)
      # labs(x = "Betas" ,y = "Means") + ggtitle(paste("Exposure v/s Effectiveness for",names_dict[x_vars[lindex]],sep = " ")) + scale_size_continuous(range = c(10,20)) +  
      
      scale_color_continuous(low = "#56B1F7",high = "#132B43") +
      geom_hline(yintercept = med.y) + geom_vline(xintercept = med.x) +
      theme(plot.title = element_text(face = 'bold', hjust = 0.5 ,size = 15), axis.text.x = element_text(size = 12),
            axis.title.x = element_text(size = 14), axis.text.y = element_text(size = 12),
            axis.title.y = element_text(size = 14),
            legend.position = "bottom") + # geom_abline(slope = 1, intercept= med.y - med.x, lty = 2)
      geom_abline(slope = med.y/med.x, intercept= 0, lty = 2)#    annotation_custom(linesGrob(c(0,1), c(0,1)))
    
    
    # annotate("segment", c(0,1), c(0,1)) ,      colour = "blue")
    # geom_line( lty = 2)
    # 
    #
    
    
    print(plot)
    lop[[lindex]] = current.graphics() 
    
  }
  
  xl[a1] = lop
  xl[k1] = transpose(as.data.frame(colnames(data1)))
  xl[k2] = data1
  
  xl.workbook.save(filename = paste0(filesavepath,'/exposure_charts_by',group_col,'.xlsx'))
  
  return(data1)
  
}




#' Summary.generator.
#' @description Gets the detailed summary with impacts of a model on a dataset and the predicted values (Y and beta*X).
#'
#' @param data Dataset on which the summary (& Impacts) are desired.
#' @param model.fitted Model used for generating summary.
#' @param response.variable Dependent Variable.
#' @param x.variables Independent variables to be included for regression.
#' @param intercept Logical. Should the impacts of intercept be calculated. By default it is TRUE.
#' @param exposure.effectiveness Logical. If TRUE, Exposure v/s Effectiveness plots are generated. By default it is FALSE.
#' @param size.var Used in case argument 'exposure.effectiveness' is set to TRUE. It is the variable by which the points' size
#'                 will be scaled.  If this is a continous variable, ratio of sum of this variable for a segment divided by the total
#'                 sum of this variable (accross all segments) will be used. If this is a categorical variable (Binary), ratio of number
#'                 of values (in a segment) equal to the second level,   divided by the number of total values (in that segment) will be
#'                 used, for each segment. By default it is the response.variable.
#' @param color.var Used in case argument 'exposure.effectiveness' is set to TRUE. It is the variable by which the points  will be
#'                  colored. If this is a continous variable, average value of the variable for each segment will be used. If this is a
#'                  categorical variable (Binary), ratio of number of  values (in a segment) equal to the second level, divided by the
#'                  total number of values (accross all segments) equal to the second level. By default it is the response.variable.#' @param visualizations  Logical. Should visuals be generated (plots & graphs). By default it is TRUE.
#' @param visuals  Logical. Should visuals be generated (plots & graphs). By default it is TRUE.
#' @param save.results Logical. Should results be saved to disk. By default it is FALSE. If TRUE, a folder name has to  be specified in
#'                     the 'folder.name' argument.
#' @param folder.name Used in case argument 'save.results' is set to TRUE. It is the name of the folder in which the results are saved.
#' @param data.type Character describing which data is used (Train/Test/Validation).This is helpful when saving the results to
#'                 disk.
#'                 
#' @return A list containing the following elements:
#' \item{model.summary}{A detailed summary showing the coefficients, p-values, means, predictions and impactables for each
#'                       segment and at an overall level}
#' \item{predictions.data}{Input data with predicted values.}
#' @examples
#'
#' #  Load US States Production Data from plm package
#' data("Produc", package = "plm")
#'
#' # Fit a glmtree model
#' model <- Model.fitting(data = Produc[Produc$year <=  max(Produc$year),], response.variable = 
#'         "unemp", x.variables = c("pcap", "hwy", "water","util", "pc", "gsp","emp"),
#'          cutting.variables = "region", min.node.size = 50,  data.type = "Example")
#'
#'# Generate model sumary
#' model.summary <- Summary.generator(data = Produc[Produc$year <=  max(Produc$year),],
#'                   model.fitted = model, response.variable = "unemp",  x.variables = c("pcap",
#'                   "hwy", "water", "util", "pc", "gsp","emp"), data.type = "Example")
#'
#' @export
summary.generator.group.level <- function (predicted.train.data, group.var.name,
                                           #data,  
                                           response.variable, x.variables,  intercept = T, exposure.effectiveness = F,
                                           size.var , color.var ,visuals = T, save.results = F,  folder.name = NULL, data.type)
{
  summary.df <- as.data.frame(predicted.train.data)
  #(X.Intercept.
  coefficient.cols <-c("(Intercept)", paste0(x.variables,".y"))
  # coefficient.cols <-c("X.Intercept.", paste0(x.variables,".y"))
  
  # which(grepl(".y", colnames(summary.df))==T)
  
  coefficient.data <- as.data.table(summary.df[, c(coefficient.cols, group.var.name)])
  coefficient.data <- coefficient.data[, lapply(.SD, mean, na.rm=TRUE), by = group.var.name, .SDcols = coefficient.cols ]
  summary.df[, (ncol(summary.df) + 1)] <- summary.df[,"Node"]
  colnames(summary.df)[ncol(summary.df)] <- "Terminal.Node"
  #colnames(summary.df)[which(colnames(summary.df) ==  "Node")] <- "Terminal.Node"
  colnames(summary.df)[which(colnames(summary.df) == group.var.name)] <- "Node"
  colnames(coefficient.data)[which(colnames(coefficient.data) == group.var.name)] <- "Node"
  
  
  #colnames(summary.df)[which(colnames(summary.df) %in%  paste0(x.variables,".x"))] <- x.variables
  
  # colnames(summary.df)[which(colnames(summary.df) ==  group.var.name)] <- "Node"
  data <- summary.df
  colnames(coefficient.data) <- c("Node","(Intercept)",x.variables)
  coef.data <- coefficient.data
  
  variable.type <- ifelse(is.numeric(data[,which(colnames(data) %in% response.variable)])==T,
                          "continous","categorical")
  if(missing(size.var)) {
    size.var <- response.variable
  }
  
  if(missing(color.var)) {
    color.var <- response.variable
  }
  
  # Adding predictions to the  data
  
  # predictions.data <- as.data.frame(cbind(data, "Predicted Value" = predict(model.fitted$final.model,data),
  #                                         "Node"= predict(model.fitted$final.model, data,type = "node")))
  predictions.data <- as.data.frame(data)
  
  #coef.data <- coef(model.fitted$final.model)
  coef.data[(is.na(coef.data))] <- 0
  # if(variable.type == "categorical") {
  #   coef.data <- exp(coef.data)
  # }
  single.node = ifelse(is.null(nrow(coef.data)) == T, T, F)
  #unique.nodes <- ifelse(single.node == F, length(unique(predictions.data$Node)), 1)
  unique.nodes <- ifelse(single.node == F, length(unique(predictions.data$Node)), 1)
  
  # Generating empty summary
  
  model.summary <- as.data.frame(matrix(0,ncol = (4 + 5*length(x.variables) + 1 + 3),nrow = unique.nodes))
  x.variables.with.intercept <- c("(Intercept)",x.variables)
  p.value.column <- paste0("P.value.",x.variables.with.intercept)
  Mean.value.column <- paste0("Mean.",x.variables.with.intercept)
  pred.columns <- paste0("Prediction.",x.variables.with.intercept)
  pred.columns.all <- c(pred.columns,"Prediction.from.model")
  perc.columns <- paste0("Percentage.Impact.",x.variables.with.intercept)
  
  colnames(model.summary) <- c("rule","Observations",x.variables.with.intercept,p.value.column,
                            Mean.value.column,pred.columns.all,perc.columns)
  
  model.summary$Observations <- as.vector(table(predictions.data$Node))
  
  if (single.node == T) {
    model.summary$rule=1
  } else {
    if(is.numeric(unique(predictions.data$Node))){
      model.summary$rule <- sort(as.numeric(unique(predictions.data$Node)))
    } else {
      model.summary$rule <- unique(predictions.data$Node)
    }

  #  model.summary$rule= sort(as.numeric(unique(predictions.data$Node)))
    
  }
  
  # summary.model.fitteds <- summary(model.fitted$final.model)
  no.of.rules <- unique.nodes

  
  # Taking required Nodes and corresponding coefficients, pvalues
  
  if(single.node == T) {
   # all.nodes <- 1
    coef.data <- as.data.frame(t(coef.data))
  } else {
   # all.nodes <- sort(as.character(unique(predictions.data$Node))) 
   # all.nodes <- sort(as.numeric(unique(predictions.data$Node))) 
    #as.numeric(unlist(dimnames(coef(model.fitted$final.model))[1]))
    coef.data <- as.data.frame(coef.data)
  }
  
  # coef.data[,(ncol(coef.data) +1 )] <- all.nodes
  # required.nodes <- all.nodes[which(all.nodes %in% as.numeric(unique(predictions.data$Node)))]
  #coef.data <- coef.data[which(coef.data[,ncol(coef.data)] %in% required.nodes),]
  
  # Adding betas * X in data at physician level
  
  # coef.data2 <- coef.data
  # colnames(coef.data2)[ncol(coef.data2)] <- "Node"
  #predictions.data.frame2 <-  predictions.data %>% left_join(coef.data2, by = "Node")
  # predictions.data.frame3 <-  predictions.data.frame2[ , c(paste0(x.variables,".x"))] *
  #   predictions.data.frame2[,  c(paste0(x.variables,".y"))]
  # predictions.data.frame2 <- cbind(predictions.data.frame2, predictions.data.frame3)
  # colnames(predictions.data.frame2)[(ncol(predictions.data.frame2) - length(x.variables) + 1):(ncol(predictions.data.frame2))] <-
  #   paste0("predicted_",x.variables)
  # coef.data <- coef.data[,-ncol(coef.data)]
  coef.data <- coef.data[order(coef.data$Node),]
  model.summary[,3: (3 + length(x.variables) )] <-  coef.data[order(coef.data$Node), -1]
  #  model.summary[,1] <-  coef.data[1]
  
  # P values
  
  if (single.node == T) {
    model.summary[,p.value.column] <- "Not.available" #summary.model.fitteds$coefficients[,4]
  } else {
    # p.values <- lapply(names(summary.model.fitteds), function(x) as.data.table(t(summary.model.fitteds[[x]]$coefficients[,4])))
    # p.values <-  as.data.frame(rbindlist(p.values,fill = T)  )
    # p.values[,(ncol(p.values) + 1 )] <- all.nodes
    # p.values <- p.values[which(p.values[,ncol(p.values)] %in% required.nodes),]
    # p.values <- p.values[,-ncol(p.values)]
    # 
    # model.summary[,p.value.column] <-  p.values
    model.summary[,p.value.column] <- "Not.available"
  }
  
  # Adding Means
  x.variables.observed <- paste0(x.variables,".x")
  predictions.data <- as.data.table(predictions.data)
  if(intercept == F) {
    model.summary[,Mean.value.column] <- cbind(0, (setorder(predictions.data[,lapply(.SD, mean, na.rm=TRUE),
                                                                             by=Node,.SDcols=x.variables.observed],Node)[,-1]))
  } else {
    model.summary[,Mean.value.column] <- cbind(1, (setorder(predictions.data[,lapply(.SD, mean, na.rm=TRUE),
                                                                             by=Node,.SDcols=x.variables.observed],Node)[,-1]))
  }
  
  # Adding Predictions from variables
  
  model.summary[,pred.columns] <-    model.summary[, x.variables.with.intercept ] * model.summary[, Mean.value.column]
  model.summary$Prediction.from.model = rowSums(model.summary[pred.columns])
  
  # Adding % impact
  
  model.summary[,perc.columns] <-   model.summary[, pred.columns ]/model.summary$Prediction.from.model
  model.summary[(is.na(model.summary))] <- 0
  
  # Adding overall % impact
  
  if (single.node == F) {
    new.row <- nrow(model.summary) + 1
    model.summary[ new.row,perc.columns] <- (colSums(model.summary$Observations * model.summary[ ,perc.columns ])  /
                                               sum(model.summary$Observations[unique(which(model.summary[ ,perc.columns] != 0 , arr.ind =  T)[,1])]))
                                               #sum(model.summary$Observations))
    model.summary[ new.row, pred.columns.all] <-  colSums(model.summary[(1:no.of.rules),pred.columns.all])
    model.summary$Observations[nrow(model.summary)] <- sum(model.summary$Observations[1:no.of.rules])
  }
  
  model.summary$rule[nrow(model.summary)] <-  "Total"
  colnames(model.summary)[which(colnames(model.summary)=="rule")] <- "Node"
  
  
  # Adding Y  mean & % contribution
  
  size.variable.type <- ifelse(is.numeric(data[,which(colnames(data) %in% size.var)])==T,
                               "continous","categorical")
  color.variable.type <- ifelse(is.numeric(data[,which(colnames(data) %in% color.var)])==T,
                                "continous","categorical")
  if (single.node == F) {
    if(size.variable.type == "continous") {
      model.summary$Mean.y <- c(eval(parse(text = paste0("(setorder(predictions.data[,lapply(.SD, mean, na.rm=TRUE),
                                                         by=Node,.SDcols=size.var],Node)[,-1])$",size.var) )),  mean(data[,size.var]) )
    } else {
      temp.df <- predictions.data[predictions.data[,which(colnames(predictions.data) %in% size.var)] ==
                                    levels(data[,which(colnames(data) %in% size.var)])[2],]
      model.summary$Mean.y <- c(eval(parse(text =   paste0("(setorder(temp.df[,lapply(.SD, length),
                                                           by=Node,.SDcols=size.var],Node)[,-1])$",size.var) )),  nrow(temp.df) ) /
        c(eval(parse(text =   paste0("(setorder(predictions.data[,lapply(.SD, length),
                                     by=Node,.SDcols=size.var],Node)[,-1])$",size.var) )),  nrow(predictions.data) )
    }
    } else {
      model.summary$Mean.y <- 1
  }
  
  if (single.node == F) {
    if(color.variable.type == "continous") {
      model.summary$Contribution.y <- c(eval(parse(text =   paste0("(setorder(predictions.data[,lapply(.SD, sum,
                                                                   na.rm=TRUE),by=Node,.SDcols=color.var],Node)[,-1] / sum(data[,color.var]) )$",color.var) )),1)
    } else {
      
      model.summary$Contribution.y <- c(eval(parse(text =   paste0("(setorder(temp.df[,lapply(.SD, length),
                                                                   by=Node,.SDcols=color.var],Node)[,-1])$",color.var) )),  nrow(temp.df) ) /
        length(which(predictions.data[,.SD,.SDcols = color.var] ==
                       levels(data[,color.var])[2] ))
    }
  } else {
    model.summary$Contribution.y <- 1
  }
  
  model.summary[(is.na(model.summary))] <- 0
  model.summary2 <-  model.summary[,c("Node","Observations",perc.columns,"Mean.y")]
  
  if(visuals == T){
    if(save.results == T) {
      jpeg(paste(filename=paste0("./",folder.name,"/Impacts.plot.",group.var.name,".",data.type,".",response.variable,".jpeg")),width = 2000,
           height = 1000,res = 80,quality=480,pointsize = 14)
    }
    plot(Impact.plotter(model.summary))
    #mtext(paste0("Tree plot for ",data.type), side = 3, line = -2, outer = TRUE)
    
    if(save.results == T) {
      dev.off()
    }
  }
  
  
  
  if(save.results==T){
    
    write.table(paste0("Mean y & Impacts by ",group.var.name),file=paste0("./",folder.name,"/Model.summary.by.group.",data.type,".",response.variable,".csv"),col.names = F, row.names = F,append = T,sep=",")
    
    write.table(model.summary,file=paste0("./",folder.name,"/Model.summary.by.group.",data.type,".",response.variable,".csv"),col.names = T, row.names = F,append = T,sep=",")
   
    write.table(paste0("Mean y & Impacts by ",group.var.name),file=paste0("./",folder.name,"/Model.impacts.by.group.",data.type,".",response.variable,".csv"),col.names = F, row.names = F,append = T,sep=",")
    
    write.table(model.summary2,file=paste0("./",folder.name,"/Model.impacts.by.group.",data.type,".",response.variable,".csv"),col.names = T, row.names = F,append = T,sep=",")
    
    
     #write.csv(predictions.data.frame2,file=paste0("./",folder.name,"/Data.with.predictions.",data.type,".",response.variable,
    #                                             ".csv"),row.names = F)
    #save(model.fitted,file = paste0("./",folder.name,"/Fitted.Model.",data.type,".",response.variable,"..Rda"))
  }
  
  if(exposure.effectiveness==T){
    if(save.results==T) {jpeg(paste(filename=paste0("./",folder.name,"/Exposure.vs.Effectiveness.plot.",group.var.name,".",data.type,".",
                                                    response.variable,".jpeg")),width = 2000, height = 1000,res = 80,quality=480,pointsize = 14)
    }
    plot(Exposure.vs.effect.plotter(data.summary =  model.summary,  x.var = x.variables,color.var = "Contribution.y",
                                    size.var = "Mean.y",label.var = "Node" ))
    #mtext(paste0("Exposure.vs.Effectiveness.plot. for ",data.type), side = 3, line = -2, outer = TRUE)
    
    if(save.results == T) {
      dev.off()
    }
  }
  
  return(list(model.summary ,as.data.frame(predictions.data)))
  
}





NAtozero <- function(dat,columns)
{
  for (col in columns)
  {
    eval(parse(text = paste('dat[(is.nan(',col,')|is.na(',col,')),',col,':=0]',sep = "")))
    
  }
  
  return(dat)
  
}


NAtoUNK <- function(dat,columns)
{
  for (col in columns)
  {
    eval(parse(text = paste('dat[(',col,'== "" |is.na( ',col,')),',col,':= "UNK"]',sep = "")))
    
  }
  
  return(dat)
  
}



GetBucketedVar <- function(dat, var_inp, var_out, buckets ){
  buckets_start <- eval(parse(text = paste('dat[, min(', var_inp,')]', sep = '')))
  #buckets_end <- eval(parse(text = paste('dat[, max(', var_inp,')]', sep = '')))
  buckets_end <- "+"
  buckets_names <- c()
  for(b in buckets){
    buckets_names <- c(buckets_names, paste(buckets_start, b, sep = '-'))
    buckets_start <- b
  }
  buckets_names <- c(buckets_names, paste(b, buckets_end, sep = ""))
  
  # dat[, PDE_Bucket:=as.factor(findInterval(Pretrial_PDE, buckets))]
  eval(parse(text = paste('dat[, ', var_out,':=as.factor(findInterval(', var_inp,
                          ', buckets))]', sep = '')))
  eval(parse(text = paste('levels(dat$',var_out,')<-buckets_names', sep = '')))
  return(dat)  
}
