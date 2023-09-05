#' @import dplyr
#' @import gtsummary
#' @import car
#' @importFrom tidyselect any_of

## Load package
require(gtsummary,quietly = T)
require(car,quietly = T)
require(dplyr,quietly = T)

## Auxiliary function
check_para <- function(para_name,envir){
  if(base::exists(para_name,envir=envir)==FALSE){message(sprintf('%s missing !',para_name));return(0)}
  if(is.null(base::get(para_name,envir=envir))==TRUE){message(sprintf('%s is NULL !',para_name));return(0)}
  return(1)
}
check_option <- function(para_name,option_list,envir){
  if(!base::get(para_name,envir=envir) %in% option_list){
    message(sprintf('Only accept %s set at: %s !',para_name,base::paste(option_list,collapse=';')));return(0)
  }
  return(1)
}


.detect_variable_types <- function(data) {
  variable_types <- sapply(data, function(x) {
    if (is.factor(x) || is.character(x)) {
      "categorical"
    } else if (is.numeric(x)) {
      "continuous"
    }
  })
  
  return(variable_types)
}



##oneway anova analysis
# The function for testing normality by group   
.shapiro_test_multi <- function(data, value, group)  {
  
  require(magrittr)   
  
  table(data[,group]) %>%   
    data.frame(.) -> a1
  
  a2 <- base::as.vector(a1[,1])
  
  data = data.frame(group = data[,group], 
                    value = data[,value]) 
  
  test.result <- data.frame(No=0,        
                            Name=0,      
                            W=0,         
                            p.value=0,  
                            norm.test=0) 
  
  for (i in (1:length(a2))){
    subset(data,                 
           group == a2[i],       
           select = value) %>%   
      .[,1] %>%                  
      stats::shapiro.test(.) -> t.r     
    
    test.result[i,1] = i              
    test.result[i,2] = a2[i]          
    test.result[i,3] = t.r$statistic  
    test.result[i,4] = t.r$p.value    
    
    if(t.r$p.value > 0.05)           
      test.result[i,5] = "Norm"    
    else
      test.result[i,5] = "Other_situation"
  } 
  
  #test.result[nrow( test.result)+1,1] = "Test Method:"  
  #test.result[nrow( test.result),2] = "Shapiro-Wilk" 
  test.result 
}


.stat_cat <- function(cat_data=NULL,group_var=NULL,cat_var=NULL,pDigits=NULL,includeNA=NULL){
  ### Test for category variables
  # Calculating the expected frequency
  cat_test_list <- cat_args_list <- list()
  for (i in cat_var) {
    sample_count <- nrow(cat_data)
    obs_mat <- stats::xtabs(as.formula(stringr::str_c("~",paste0(c(group_var,i),collapse = '+'))),data=cat_data)
    ka_exp_mat <- stats::chisq.test(obs_mat) %>% .$expected
    if(sample_count<40 | base::min(ka_exp_mat)<1){ #Fisher exact
      cat_test_list[[i]] <- "fisher.test"
    }else if(sample_count>=40 & base::any(ka_exp_mat>=5)){ #Chisq test
      cat_test_list[[i]] <- "chisq.test.no.correct"
    }else if(sample_count>=40 & base::any(ka_exp_mat>=1 & ka_exp_mat<5)){ #Corrected chisq test
      cat_test_list[[i]] <- "chisq.test"
    }
  }
  return(cat_test_list)
}


.stat_cont <- function(cont_data=NULL,group_var=NULL,cont_var=NULL,pDigits=NULL,includeNA=NULL){
  ### Test for continuous variables
  # Consider normality and variance chi-squared
  cont_test_list <- cont_equal_list <- list()
  rs_table <- as.data.frame(table(cont_data[,group_var]))
  for (i in cont_var) {
    # i <- cont_var[1]
    if(nrow(rs_table) == 2){
      if(min(rs_table$Freq) > 3 & max(rs_table$Freq)<5000){
        shapiro_test <- .shapiro_test_multi(cont_data,value = i,group = group_var) # Grouping normality test
        leve_result <- car::leveneTest(y = cont_data[,i],group = cont_data[,group_var])[1,3] # Test for homogeneity of grouped variance
        if (leve_result > 0.05 & base::all(shapiro_test$norm.test == "Norm")) {
          cont_test_list[[i]] <- 't.test'
          cont_equal_list[[i]] <- list(var.equal = TRUE)
        } else if (leve_result < 0.05 & base::all(shapiro_test$norm.test == "Norm")){
          cont_test_list[[i]] <- 't.test'
          cont_equal_list[[i]] <- list(var.equal = FALSE)
        }else{
          cont_test_list[[i]] <- 'kruskal.test'
        }
      } else if (max(rs_table$Freq)>5000){
        cont_test_list[[i]] <- 't.test'
      } else {
        cont_test_list[[i]] <- 't.test'
      }
    } else if (nrow(rs_table) >= 3){
      if(min(rs_table$Freq) > 3 & max(rs_table$Freq)<5000){
        shapiro_test <- .shapiro_test_multi(cont_data,value = i,group = group_var) # Grouping normality test
        leve_result <- car::leveneTest(y = cont_data[,i],group = cont_data[,group_var])[1,3] # Test for homogeneity of grouped variance
        if (leve_result > 0.05 & base::all(shapiro_test$norm.test == "Norm")) {
          cont_test_list[[i]] <- 'oneway.test'
          cont_equal_list[[i]] <- list(var.equal = TRUE)
        } else if (leve_result < 0.05 & base::all(shapiro_test$norm.test == "Norm")){
          cont_test_list[[i]] <- 'oneway.test'
          cont_equal_list[[i]] <- list(var.equal = FALSE)
        } else {
          cont_test_list[[i]] <- 'kruskal.test'
        }
      } else if (max(rs_table$Freq)>5000){
        cont_test_list[[i]] <- 'aov'
      } else {
        cont_test_list[[i]] <- 'kruskal.test'
      }
    }
  }
  return(list(cont_test_list,cont_equal_list))
}



.gt_auto <- function(data=NULL,group_var=NULL,cat_var=NULL,cont_var=NULL,out_format='gtobj',digits=digits,pDigits=pDigits,missing=missing){
  cat_var_type <- cont_var_type <- NULL
  
  if(!is.null(cat_var) && length(cat_var)>0){
    cat_var_type <- as.list(rep("categorical",length(cat_var)));names(cat_var_type) <- cat_var
  }
  if(!is.null(cont_var) && length(cont_var)>0){
    cont_var_type <- as.list(rep("continuous2",length(cont_var)));names(cont_var_type) <- cont_var
  }
  
  if(is.null(group_var) & (!is.null(cat_var) | !is.null(cont_var))){
    gt_object <- data %>% dplyr::select(tidyselect::any_of(c(cont_var,cat_var))) %>%
      gtsummary::tbl_summary(type = c(cat_var_type,cont_var_type),
                  statistic = gtsummary::all_continuous() ~ c("{mean} ({sd})",
                                                   "{median} ({p25}, {p75})",
                                                   "{min}, {max}"),  
                  missing=missing,
                  missing_text='missing',
                  digits = gtsummary::all_continuous() ~ digits)
  }else{
    gt_object <- data %>% dplyr::select(tidyselect::any_of(c(group_var,cont_var,cat_var))) %>%
      gtsummary::tbl_summary(by=group_var,
                  type = c(cat_var_type,cont_var_type),
                  statistic = gtsummary::all_continuous() ~ c("{mean} ({sd})",
                                                   "{median} ({p25}, {p75})",
                                                   "{min}, {max}"),  
                  missing=missing,
                  missing_text='missing',
                  digits = gtsummary::all_continuous() ~ digits) %>%
      gtsummary::add_overall() %>%
      gtsummary::add_stat_label() %>%
      gtsummary::add_p(pvalue_fun = ~ gtsummary::style_pvalue(.x, digits = pDigits)) %>% 
      gtsummary::modify_header(statistic ~ "**Test Statistic**") %>%
      gtsummary::modify_fmt_fun(statistic ~ gtsummary::style_sigfig)
  }
  gt_object <- gt_object %>% 
    gtsummary::bold_labels() %>%
    gtsummary::as_gt()
  
  switch(out_format,
         gtobj = return(gt_object),
         dataframe = return(as.data.frame(gt_object)))
}



.gt_define <- function(data=NULL,group_var=NULL,cat_var=NULL,cont_var=NULL,out_format='gtobj',digits=digits,pDigits=pDigits,missing=missing){
  cat_var_type <- test_list_cat <- cont_var_type <- test_list_cont <- args_list <- NULL
  
  if(!is.null(cat_var) && length(cat_var)>0){
    cat_var_type <- as.list(rep("categorical",length(cat_var)));names(cat_var_type) <- cat_var
  }
  if(!is.null(cont_var) && length(cont_var)>0){
    cont_var_type <- as.list(rep("continuous2",length(cont_var)));names(cont_var_type) <- cont_var
  }
  
  if(is.null(group_var) & (!is.null(cat_var) | !is.null(cont_var))){
    gt_object <- data %>% dplyr::select(tidyselect::any_of(c(cont_var,cat_var))) %>%
      gtsummary::tbl_summary(type = c(cat_var_type,cont_var_type),
                  statistic = gtsummary::all_continuous() ~ c("{mean} ({sd})",
                                                               "{median} ({p25}, {p75})",
                                                               "{min}, {max}"),  
                  missing=missing,
                  missing_text='missing',
                  digits = gtsummary::all_continuous() ~ digits)
  }else{
    test_list_cat <- .stat_cat(cat_data = data,group_var = group_var,cat_var = cat_var)
    stat_cont_list <- .stat_cont(cont_data = data,group_var = group_var,cont_var = cont_var)
    test_list_cont <- stat_cont_list[[1]]
    args_list <- stat_cont_list[[2]]
    
    gt_object <- data %>% dplyr::select(tidyselect::any_of(c(group_var,cont_var,cat_var))) %>%
      gtsummary::tbl_summary(by=group_var,
                  type = c(cat_var_type,cont_var_type),
                  statistic = gtsummary::all_continuous() ~ c("{mean} ({sd})",
                                                   "{median} ({p25}, {p75})",
                                                   "{min}, {max}"),  
                  missing=missing,
                  missing_text='missing',
                  digits = gtsummary::all_continuous() ~ digits) %>%
      gtsummary::add_overall() %>%
      gtsummary::add_stat_label() %>% 
      gtsummary::add_p(pvalue_fun = ~ gtsummary::style_pvalue(.x, digits = pDigits),
                        test=c(test_list_cat,test_list_cont),
                        test.args=args_list) %>%
      gtsummary::modify_header(statistic ~ "**Test Statistic**") %>%
      gtsummary::modify_fmt_fun(statistic ~ gtsummary::style_sigfig)
  }
  gt_object <- gt_object %>% 
    gtsummary::bold_labels() %>%
    gtsummary::as_gt()

  switch(out_format,
         gtobj = return(gt_object),
         dataframe = return(as.data.frame(gt_object)))
}




