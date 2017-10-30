# Notes: -----------------------------------------------------------------------
# Breo Model

# Load Libs
rm(list=ls(all=TRUE))
library(data.table)
library(ggplot2)
library(scales)
library(dplyr)
library(caret)
library(partykit)
library(e1071)
library(rBayesianOptimization)
library(gridExtra)
library(car)
library(knitr)
library(plyr)
theme_set(theme_classic())
set.seed(123)

# User Inputs: -----------------------------------------------------------------
setwd("C:/Users/nk15384/Desktop/Nilesh Files/_Projects/ba_Launch_Analytics/ba_ac_gsk_driver")
# setwd("C:/02 ADS PROJECTS/03_GSK_TerritoryCompressionAnalysis/Data/Aggregate_Data/0610")

fl_name_data <- "c_input_data/data_cut_6mnths_filtered_imp_variables.RData"
fl_name_model <- "c_input_data/Fitted.Model.All_Data.nbrx_share..Rda"

fs_target <- 'nbrx_share'
fs_id <- c('phys_id', 'terr_id')
fs_ind_reg <- c("differential_access_reversed", "rep_rating_reversed", 
               "details", "samples", "claims_filled_pct", "claims_rejected_pct", 
               "lecture", "telecom", "rep_tenure")
fs_ind_seg <- c('speciality', 'LHM_avg_DA_score', 'LHM_avg_NBRx_share', 
                'action_group')
fs_other <- c('nbrx_share', 'trx', 'writer_nonwriter')
fs_all <- c(fs_id, fs_target, fs_ind_seg, fs_ind_reg, fs_other)


# Create Data Structure: -------------------------------------------------------
fs <- list(id = fs_id, target = fs_target, ind_seg = fs_ind_seg, 
           ind_reg = fs_ind_reg, all = fs_all, other = fs_other) 
rm(fs_id, fs_target, fs_ind_seg, fs_ind_reg, fs_all, fs_other)
res <- list(train = NA_real_, test = NA_real_)
mod <- list(dat = data.frame(), beta = data.frame(), impact = data.frame())

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

# Functions: Data Processing ---------------------------------------------------
LoadData <- function(fl_name_data){
  load(fl_name_data)
  dat_phte6m_flt <- data_cut_6mnths_filtered_imp_variables
  rm(data_cut_6mnths_filtered_imp_variables)
  
  # Rename Columns
  setnames(dat_phte6m_flt, 'CURR_TRX_VOL', 'trx')
  setnames(dat_phte6m_flt, 'uniqueDA', 'differential_access_reversed')
  setnames(dat_phte6m_flt, 'PDE', 'details')
  setnames(dat_phte6m_flt, 'Rep_Tenure', 'rep_tenure')
  setnames(dat_phte6m_flt, 'Rep_avg_rating_REVERSE_REGRSN', 'rep_rating_reversed')
  setnames(dat_phte6m_flt, 'PCT_BREO_APPROVED_REGRSN', 'claims_filled_pct')
  setnames(dat_phte6m_flt, 'PCT_BREO_REJECTED_REGRSN', 'claims_rejected_pct')
  setnames(dat_phte6m_flt, 'TERR_ID', 'terr_id')
  setnames(dat_phte6m_flt, 'PHYS_ID', 'phys_id')
  setnames(dat_phte6m_flt, 'LHM_Avg_DA_Score', 'LHM_avg_DA_score')
  setnames(dat_phte6m_flt, 'LHM_Avg_NBRx_Share', 'LHM_avg_NBRx_share')
  setnames(dat_phte6m_flt, 'ACTION_GROUP', 'action_group')
  
  dat_phte6m_flt$writer_nonwriter <- ifelse(dat_phte6m_flt$nbrx_share > 0, 1, 0)
  dat_phte6m_flt$action_group <- factor(dat_phte6m_flt$action_group,
                                        levels = c("D", "C", "B", "A"), 
                                        ordered = TRUE)
  dat_phte6m_flt$speciality <- factor(dat_phte6m_flt$speciality,
                                      levels = c("UNK", "OTH", "PCP", "PUD", "ALG"), 
                                      ordered = TRUE)
  return(dat_phte6m_flt[,.SD, .SDcol = fs$all])
}

LoadModel <- function(fl_name_model){
  load(fl_name_model)
  model <- model.fitted
  return(model)
}

# Functions: Explore Data ------------------------------------------------------
ExploreData <- function(dat){
  # dat = dat_phte6m_flt
  cat('-----------------------------------------------------------------------\n')
  cat('Summary:\n')
  cat('-----------------------------------------------------------------------\n')
  print(summary(dat[, .SD, .SDcol = fs$all]))
  cat('-----------------------------------------------------------------------')
  
  # Explore Target variable
  print(qplot(dat[, get(fs$target)], ylab = 'count', xlab = fs$target))
  print(qplot(log(1+dat[, get(fs$target)]), ylab = 'count', 
              xlab = paste0('log(1+',fs$target,')')))
  
  # Explore independent variables
  for(var in c(fs$ind_reg, fs$ind_seg)){
    print(qplot(dat[, get(var)], ylab = 'count', xlab = var))
  }
  
}



# Functions; Model Training ----------------------------------------------------
TrainMOBModel <- function(dat, fs, idx, max_depth = 4, min_size = 1500){
  # dat = dat_phte6m_flt; max_depth = 4; min_size = 1500
  # dat$nbrx_share <- dat$nbrx_share + 1
  
  formula_mob <- as.formula(paste(fs$target, '~', paste(fs$ind_reg , collapse = '+ '),
                       '|', paste(fs$ind_seg, collapse = '+ ')))

  model <- glmtree(formula_mob, data = dat[idx$train, ], family = "gaussian", 
                   alpha = .05, epsilon = 1e-4, maxit = 25, vcov = 'info',
                   maxdepth = max_depth, minsize = min_size) #, minsplit = 500)
  model
}

TrainMARSModel <- function(){
  library(earth)
  formula_ <- as.formula(paste(fs$target, '~', paste(fs$ind_reg , collapse = '+ '),
                               '+', paste(fs$ind_seg, collapse = '+ ')))
  
  model <- earth(formula_, data = dat_phte6m_flt[idx$train, ])
  plot(model)
  evimp(model)
  plot.earth.models(model)
  plotd(model)
  t <- predict(model, dat_phte6m_flt)
  plot(density(t))
  
}
EvaluateModel <- function(model, dat, fs, idx) {
  # dat = dat_phte6m_flt
  res$train <- R2(dat[idx$train, get(fs$target)], predict(model, dat[idx$train]))
  res$test <- R2(dat[idx$test, get(fs$target)], predict(model, dat[idx$test]))
  
  # mean(dat[, get(fs$target)])
  # mean(predict(model, dat))
  print(paste('R2 - Train:', round(res$train, 3), 'Test:', round(res$test, 3)))
  res
}


GetDatImportance <- function(model, dat, fs){
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
  dat_imp <- dat_betas[, .SD, .SDcol = fs$ind_reg] * 
    dat[, .SD, .SDcol = fs$ind_reg]
  dat_imp$intercept <- dat_betas$`(Intercept)`
  dat_imp$node <- dat_betas$nodes
  dat_imp
}

NodeSummary <- function(dat, dat_imp, fs){
  dat$node <- dat_imp$node
  out_node_summ <- dat[, list(round(mean(nbrx_share), 3), count = .N), by = node]
  setnames(out_node_summ, 'V1', fs$target)
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

RelativeContribution <- function(model, dat, fs, dat_imp){
  # dat = dat_phte6m_flt
  out_cont <- dat_imp[, sapply(.SD, mean), .SDcol = c(fs$ind_reg, 'intercept')]
  out_cont <- data.table(Driver = names(out_cont),
                         `Contribution Percentage` = round(out_cont * 100, 3))
  out_cont$`Relative Contribution Percentage` <- 
    round(out_cont$`Contribution Percentage` * 100 / 
            sum(out_cont$`Contribution Percentage`), 3)
  out_cont$Driver <- factor(out_cont$Driver, levels = out_cont$Driver)
  print(knitr::kable(out_cont))
  p <- ggplot(out_cont, aes(1, `Relative Contribution Percentage`, fill=Driver,
                       order = c(fs$ind_reg, 'intercept'))) + 
    geom_bar(stat="identity")  
  print(p)
}

PrintModelSummary <- function(model, dat, fs, idx){
  # dat = dat_phte6m_flt
  dat_imp <- GetDatImportance(model, dat, fs)
    
  print(plot(as.constparty(model)))
  EvaluateModel(model, dat, fs, idx)
  NodeSummary(dat, dat_imp, fs)
  RelativeContribution(model, dat, fs, dat_imp)
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

SensitivityAnalysis <- function(model, dat, fs, res_type = 'overall'){
  # dat = dat_phte6m_flt
  out <- data.table(driver = fs$ind_reg, v1 = NA_real_, volume_diff = NA_real_)
  if (res_type == 'overall'){
    fs$target_diff <- paste0(fs$target, '_pct_change')
  } else{
    fs$target_diff <- paste0(fs$target, '_differance')
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


# Run Code: --------------------------------------------------------------------
dat_phte6m_flt <- LoadData(fl_name_data)
# model <- LoadModel(fl_name_model)
# ExploreData(dat_phte6m_flt)
idx <- TrainTestSplit(dat_phte6m_flt, train_pct = 0.8, type = 'random')
model <- TrainMOBModel(dat_phte6m_flt, fs, idx)
PrintModelSummary(model, dat_phte6m_flt, fs, idx)
SensitivityAnalysis(model, dat_phte6m_flt, fs)
