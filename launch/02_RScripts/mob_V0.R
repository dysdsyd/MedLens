
# package_list <- c("data.table", "ggplot2", "scales", "dplyr", "caret", "partykit", "e1071", "rBayesianOptimization",
#                  "gridExtra", "car", "knitr", "plyr")

# install.packages(package_list)

# library(data.table)
# library(ggplot2)
# library(scales)
# library(plyr)
# library(dplyr)
# library(caret)
# library(partykit)
# library(e1071)
# library(rBayesianOptimization)
# library(gridExtra)
# library(car)
# library(knitr)
# library(stringr)
# set.seed(12345)

# # ********************
# # Main Run Function
# # ********************
# mob_run <- function(file_path, file_path_all,formula, id_key='IMS_ID', train_split=0.8, max_depth=4, min_size=50, ver_string = 'default', alpha=0.01){
#   result_path <-file.path(paste0('launch/04_Results/', ver_string))
#   dir.create(result_path)
#   dat <- read.csv(file_path)
#   dat <- as.data.table(dat)
  
#   #dat <- fread(file_path)
#   formula <- as.formula(formula)

#   # Separating variables ----------------------------------------------------
#   target <- as.character(formula)[2]
#   temp <- c(str_split(as.character(formula)[3], " \\| ", simplify = T))
#   reg <- c(str_split(temp[1]," \\+ ", simplify = T))
#   part <- c(str_split(temp[2]," \\+ ", simplify = T))
#   fs <- list(target = target, reg = reg, part = part)

#   res <- list(train = NA_real_, test = NA_real_)
#   mod <- list(dat = data.frame(), beta = data.frame(), impact = data.frame())
  
#   # RUN Model ----------------------------------------------------------------
#   idx <- TrainTestSplit(dat, train_pct = train_split, type = 'Split', id_key)
#   system.time(model <- TrainMOBModel(dat, formula, fs, idx, max_depth = max_depth, min_size = min_size, alpha=alpha))
#   saveRDS(model, paste0(result_path, '/MOB_model.rds'))
#   # Plotting Tree ------------------------------------------------------------
#   jpeg(filename = paste0(result_path,'/box_tree.jpg'),width =2500, height = 1500,res = 80,quality=480,pointsize = 20)
#   plot(as.constparty(model))
#   dev.off() 
#   jpeg(filename = paste0(result_path,'/scatter_tree.jpg'),width = 2500, height = 1500,res = 80,quality=480,pointsize = 20)
#   plot(model)
#   dev.off()
  
#   GetMasterData(model, dat, fs, result_path, file_path_all)
#   # Evaluating the model-------------------------------------------------------
#   EvaluateModel(model, dat, fs, idx, res)
  
#   #GetDatImportance(model, dat, fs)
#   #PrintModelSummary(model, dat, fs, idx)
#   #SensitivityAnalysis(model, dat, fs)
# }


# # Functions: Others ------------------------------------------------------------
# R2 <- function(y, y_hat){
#   1 - (sum((y - y_hat)^2)/sum((y-mean(y))^2))
# }

# RMSE <- function(y, y_hat){
#   sqrt(mean((y - y_hat)^2))
# }

# GetMasterData <- function(model, dat, fs, result_path, file_path_all){
#   betas <- as.data.frame(coefficients(model))
#   if(any(is.na(as.numeric(row.names(betas))))){
#     betas <- data.table(t(betas))
#     betas$nodes <- 1
#   } else{
#     betas$nodes <- as.numeric(row.names(betas))
#   }
#   betas <- as.data.table(betas)
#   nodes <- predict(model, dat, type = 'node')
#   nodes <- as.data.table(nodes)
#   dat_betas <- join(nodes, betas, type = 'left', by = 'nodes')
#   dat_betas <- as.data.table(dat_betas)
#   colnames(dat_betas)[2:length(colnames(dat_betas))] <- paste0("beta_", colnames(dat_betas)[2:length(colnames(dat_betas))])
#   data <- cbind(dat, dat_betas)
#   write.csv(data, file = paste0(result_path,'/master_data.csv'), row.names = F)
  
#   full_dat <- read.csv(file_path_all)
#   full_dat <- as.data.table(full_dat)
#   nodes <- predict(model, full_dat, type = 'node')
#   nodes <- as.data.table(nodes)
#   dat_betas <- join(nodes, betas, type = 'left', by = 'nodes')
#   dat_betas <- as.data.table(dat_betas)
#   colnames(dat_betas)[2:length(colnames(dat_betas))] <- paste0("beta_", colnames(dat_betas)[2:length(colnames(dat_betas))])
#   data <- cbind(full_dat, dat_betas)
#   write.csv(data, file = paste0(result_path,'/master_data_full.csv'), row.names = F)
# }

# TrainTestSplit <- function(dat, train_pct = .999, type = 'Split', id_key){
#   # dat = dat_phte6m_flt
  
#   if (type == 'random'){
#     idx_train <- sample(nrow(dat), nrow(dat) * train_pct)   
#     idx_test <- setdiff(1:nrow(dat), idx_train)
#   }
#   if (type == 'Split'){
#     unique_id_key_val <- unique(dat[,get(id_key)])
#     idx_train_list <- sample(length(unique_id_key_val), length(unique_id_key_val) * train_pct)
#     idx_train <- dat[get(id_key) %in% (unique_id_key_val[idx_train_list]),which = TRUE]
#     idx_test <- dat[get(id_key) %in% (unique_id_key_val[-idx_train_list]),which = TRUE]
#   }
  
#   idx <- list(train = idx_train, test = idx_test)
#   idx
# }


# LoadModel <- function(fl_name_model){
#   load(fl_name_model)
#   model <- model.fitted
#   return(model)
# }


# # Functions; Model Training ----------------------------------------------------
# TrainMOBModel <- function(dat, formula, fs, idx, max_depth = 4, min_size = 50, alpha = 0.01){
#   model <- glmtree(formula, data = dat[idx$train, ], family = "gaussian", 
#                    alpha = alpha, epsilon = 1e-4, maxit = 100, vcov = 'info',
#                    maxdepth = max_depth, minsize = min_size ) #, minsplit = 500)
#   model
# }


# EvaluateModel <- function(model, dat, fs, idx, res) {
#   # dat = dat_phte6m_flt
#   res$train <- R2(dat[idx$train, get(fs$target)], predict(model, dat[idx$train,]))
#   res$test <- R2(dat[idx$test, get(fs$target)], predict(model, dat[idx$test,]))
  
#   # mean(dat[, get(fs$target)])
#   # mean(predict(model, dat))
#   print(paste('R2 - Train:', round(res$train, 3), 'Test:', round(res$test, 3)))
#   #res
# }


# GetDatImportance <- function(model, dat, fs){
#   # dat = dat_phte6m_flt
#   betas <- as.data.frame(coefficients(model))
#   if(any(is.na(as.numeric(row.names(betas))))){
#     betas <- data.table(t(betas))
#     betas$nodes <- 1
#   } else{
#     betas$nodes <- as.numeric(row.names(betas))
#   }
#   betas <- as.data.table(betas)
#   nodes <- predict(model, dat, type = 'node')
#   nodes <- as.data.table(nodes)
#   dat_betas <- join(nodes, betas, type = 'left', by = 'nodes')
#   dat_betas <- as.data.table(dat_betas)
#   rm(nodes, betas)
#   dat_imp <- dat_betas[, .SD, .SDcol = fs$reg] * 
#     dat[, .SD, .SDcol = fs$reg]
#   dat_imp$intercept <- dat_betas$`(Intercept)`
#   dat_imp$node <- dat_betas$nodes
#   dat_imp
# }

# NodeSummary <- function(dat, dat_imp, fs){
#   dat$node <- dat_imp$node
#   out_node_summ <- dat[, list(round(mean(fs$target), 3), count = .N), by = node]
#   setnames(out_node_summ, 'V1', fs$target)
#   setorder(out_node_summ, node)
#   col_names <- colnames(out_node_summ) 
#   out_node_summ <- as.data.frame(t(out_node_summ))
#   colnames(out_node_summ) <- as.integer(out_node_summ[1,])
#   out_node_summ <- out_node_summ[-1,]
#   print('Node Summary:')
#   print(knitr::kable(out_node_summ))
#   # library(formattable)
#   # print(formattable(out_node_summ))
# }

# RelativeContribution <- function(model, dat, fs, dat_imp){
#   # dat = dat_phte6m_flt
#   out_cont <- dat_imp[, sapply(.SD, mean), .SDcol = c(fs$reg, 'intercept')]
#   out_cont <- data.table(Driver = names(out_cont),
#                          `Contribution Percentage` = round(out_cont * 100, 3))
#   out_cont$`Relative Contribution Percentage` <- 
#     round(out_cont$`Contribution Percentage` * 100 / 
#             sum(out_cont$`Contribution Percentage`), 3)
#   out_cont$Driver <- factor(out_cont$Driver, levels = out_cont$Driver)
#   print(knitr::kable(out_cont))
#   p <- ggplot(out_cont, aes(1, `Relative Contribution Percentage`, fill=Driver,
#                             order = c(fs$ind_reg, 'intercept'))) + 
#     geom_bar(stat="identity")  
#   print(p)
# }

# PrintModelSummary <- function(model, dat, fs, idx){
#   # dat = dat_phte6m_flt
#   dat_imp <- GetDatImportance(model, dat, fs)
  
#   print(plot(as.constparty(model)))
#   EvaluateModel(model, dat, fs, idx)
#   NodeSummary(dat, dat_imp, fs)
#   RelativeContribution(model, dat, fs, dat_imp)
# }


