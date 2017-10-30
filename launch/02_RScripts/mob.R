
#package_list <- c("data.table", "ggplot2", "scales", "dplyr", "caret", "partykit", "e1071", "rBayesianOptimization",
#                  "gridExtra", "car", "knitr", "plyr")

#install.packages(package_list)

library(data.table)
library(ggplot2)
library(scales)
library(plyr)
library(dplyr)
library(caret)
library(partykit)
library(e1071)
library(rBayesianOptimization)
library(gridExtra)
library(car)
library(knitr)
library(stringr)
theme_set(theme_classic())
set.seed(123)

dat <- data.table::fread(file = 'MOB/data/boston_pre.csv')
#*******************
# input from python
#*******************
formula <- as.formula("MV ~ LSTAT + RM | ZN + RAD + INDUS + TAX + CRIM + NOX + RM + CHAS")

# Separating variables
target <- as.character(formula)[2]
temp <- c(str_split(as.character(formula)[3], " \\| ", simplify = T))
reg <- c(str_split(temp[1]," \\+ ", simplify = T))
part <- c(str_split(temp[2]," \\+ ", simplify = T))
fs <- list(target = target, reg = reg, part = part)

# Functions: Others ------------------------------------------------------------
R2 <- function(y, y_hat){
  1 - (sum((y - y_hat)^2)/sum((y-mean(y))^2))
}

RMSE <- function(y, y_hat){
  sqrt(mean((y - y_hat)^2))
}

TrainTestSplit <- function(dat, train_pct = .8, type = 'random'){
  # dat = dat_phte6m_flt
  
  if (type == 'random'){
    idx_train <- sample(nrow(dat), nrow(dat) * 0.8)   
  }
  idx_test <- setdiff(1:nrow(dat), idx_train)
  idx <- list(train = idx_train, test = idx_test)
  idx
}

LoadModel <- function(fl_name_model){
  load(fl_name_model)
  model <- model.fitted
  return(model)
}


# Functions; Model Training ----------------------------------------------------
TrainMOBModel <- function(dat, formula, idx, max_depth = 8, min_size = 50){
  model <- glmtree(formula, data = dat[idx$train, ], family = "gaussian", 
                   alpha = .05, epsilon = 1e-4, maxit = 25, vcov = 'info',
                   maxdepth = max_depth, minsize = min_size) #, minsplit = 500)
  model
}

# Fuctions: Summaries -----------------------------------------------------------
EvaluateModel <- function(model, dat, formula, idx) {
  # dat = dat_phte6m_flt
  res$train <- R2(dat[idx$train, noquote(target)], predict(model, dat[idx$train]))
  res$test <- R2(dat[idx$test, noquote(target)], predict(model, dat[idx$test]))
  print(paste('R2 - Train:', round(res$train, 3), 'Test:', round(res$test, 3)))
  res
}


GetDatImportance <- function(model, dat, formula){
  # dat = dat_phte6m_flt
  betas <- as.data.frame(coefficients(model))
  if(any(is.na(as.numeric(row.names(betas))))){
    betas <- data.table(t(betas))
    betas$nodes <- 1
  } else{
    betas$nodes <- as.numeric(row.names(betas))
  }
  betas <- as.data.table(betas)
  nodes <- predict(model, dat, type = 'node')
  nodes <- as.data.table(nodes)
  dat_betas <- join(nodes, betas, type = 'left', by = 'nodes')
  dat_betas <- as.data.table(dat_betas)
  rm(nodes, betas)
  dat_imp <- dat_betas[, .SD, .SDcol = noquote(reg)] * dat[,.SD, .SDcol = noquote(reg)]
  dat_imp$intercept <- dat_betas$`(Intercept)`
  dat_imp$node <- dat_betas$nodes
  dat_imp
}

NodeSummary <- function(dat, dat_imp, formula){
  dat$node <- dat_imp$node
  out_node_summ <- dat[, list(round(mean(nbrx_share), 3), count = .N), by = node]
  setnames(out_node_summ, 'V1', target)
  setorder(out_node_summ, node)
  col_names <- colnames(out_node_summ) 
  out_node_summ <- as.data.frame(t(out_node_summ))
  colnames(out_node_summ) <- as.integer(out_node_summ[1,])
  out_node_summ <- out_node_summ[-1,]
  print('Node Summary:')
  print(knitr::kable(out_node_summ))
  # library(formattable)
  # print(formattable(out_node_summ))
}

RelativeContribution <- function(model, dat, formula, dat_imp){
  # dat = dat_phte6m_flt
  out_cont <- dat_imp[, sapply(.SD, mean), .SDcol = c(reg, 'intercept')]
  out_cont <- data.table(Driver = names(out_cont),
                         `Contribution Percentage` = round(out_cont * 100, 3))
  out_cont$`Relative Contribution Percentage` <- 
    round(out_cont$`Contribution Percentage` * 100 / 
            sum(out_cont$`Contribution Percentage`), 3)
  out_cont$Driver <- factor(out_cont$Driver, levels = out_cont$Driver)
  print(knitr::kable(out_cont))
  p <- ggplot(out_cont, aes(1, `Relative Contribution Percentage`, fill=Driver,
                            order = c(reg, 'intercept'))) + 
    geom_bar(stat="identity")  
  print(p)
}

PrintModelSummary <- function(model, dat, formula, idx){
  # dat = dat_phte6m_flt
  dat_imp <- GetDatImportance(model, dat, formula)
  
  print(plot(as.constparty(model)))
  EvaluateModel(model, dat, formula, idx)
  NodeSummary(dat, dat_imp, formula)
  RelativeContribution(model, dat, formula, dat_imp)
}

# Functions: Model Analysis ----------------------------------------------------
create_sensitivity <- function(model, dat, x_vars){
  all_results = NULL
  for (xvar in x_vars) {
    cat('working on :', xvar)
    y = predict(model,as.data.frame(dat))
    eval(parse(text = paste0('s1 = sum(dat[,',xvar,'],na.rm = T)')))
    eval(parse(text = paste0('m = mean(dat[,',xvar,'],na.rm = T)')))
    #print(m1)
    dat2<-as.data.table(dat)
    eval(parse(text = paste0('dat2[,',xvar,' := ',xvar,'+1]')))
    
    eval(parse(text = paste0('s2 = sum(dat2[,',xvar,'],na.rm = T)')))
    y1 = predict(model,as.data.frame(dat2))
    sensitivity_val = mean(y1-y,na.rm = T)
    
    volume_change = (s2-s1)/s1
    all_results = rbind(all_results,c(xvar,sensitivity_val,volume_change,m))
    
  }
  colnames(all_results) = c("X_Var","Sensitivity","Volume change in X_var","Mean")
  return(all_results)
}



# To correct
SensitivityAnalysis <- function(model, dat, formula, res_type = 'overall'){
  # dat = dat_phte6m_flt
  out <- data.table(driver = reg, v1 = NA_real_, volume_diff = NA_real_)
  if (res_type == 'overall'){
    fs$target_diff <- paste0(target, '_pct_change')
  } else{
    fs$target_diff <- paste0(target, '_differance')
  }
  setnames(out, 'v1', fs$target_diff)
  for(var in fs$ind_reg){
    dat_new <- copy(dat)
    value <- 1
    dat_new[, (var) := get(var) + value]
    
    if(res_type == 'overall'){
      target_diff <- (sum(predict(model, dat_new)) - 
                        sum(dat[, get(fs$target)]))/ 
        sum(dat[, get(fs$target)]) * 100
      
    }
    
    vol_diff <- round( (sum(dat_new[, get(var)]) -  sum(dat[, get(var)])) * 100 /
                         sum(dat[, get(var)]), 1)
    out[driver == var, (fs$target_diff) := target_diff]
    out[driver == var, volume_diff := vol_diff]
  }
  print(knitr::kable(out))
  out
}

#********************** RUN *******************
idx <- TrainTestSplit(dat, train_pct = 0.8, type = 'random')
model <- TrainMOBModel(dat, formula, idx)
PrintModelSummary(model, dat, formula, idx)
