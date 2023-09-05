#' Create table1
#'
#' \code{gttable1} is a function to create table1
#'
#' @param data A dataframe.
#' @param group_var A character, a column name (quoted) in data. Summary statistics will be calculated 
#' separately for each level of the by variable. If NULL, summary statistics are calculated using 
#' all observations.
#' @param cat_var A vector, column name (quoted) in data. Indicative categorical variables. 
#' If NULL, all categorical variables will be calculated.
#' @param cont_var A vector, column name (quoted) in data. Indicative continuous variables.
#' If NULL, all continuous variables will be calculated.
#' @param out_format A character, the format of output (e.g. "gtobj", "dataframe"). Default is "gtobj".
#' @param digits A numeric, number of decimal places of continuous variables.
#' @param pDigits A numeric, number of decimal places of p value.
#' @param method character, methods for gttable1 (e.g. "auto","define"). Default is "auto".
#' auto : equal to gtsummary package; define : automatically choose appropriate statistical methods 
#' according to normality and homogeneity of variance.
#' 
#' @return
#' Return a gt object or dataframe. 
#'
#' @examples
#' data(pbc, package = "survival")
#' cont_var <- c("bili","chol","copper","alk.phos","trig")
#' cat_var <- c("sex","status","stage")
#' group_var <- "trt"
#' gttable1(data=pbc,out_format="dataframe",pDigits=3,method="auto")
#' gttable1(data=pbc,cat_var=cat_var,out_format="gtobj",pDigits=3,method="define")
#' gttable1(data=pbc,group_var=group_var,cat_var=cat_var,cont_var=cont_var,out_format="gtobj",pDigits=3,method="auto",missing="ifany")
#' 
#' @export
gttable1 <- function(data=NULL,group_var=NULL,cat_var=NULL,cont_var=NULL,
                              out_format="gtobj",digits=2,pDigits=3,method="auto",missing="ifany"){
  ## Check parameters
  all_input_para <- c('data')
  check_res <- sapply(all_input_para,function(x)check_para(x,envir=environment()))
  if(base::min(check_res)==0){message('Please check and re-try!');return(NULL)}
  if(!is.data.frame(data)){message('data should be a dataframe!');return(NULL)}
  check_res <- c(check_option('out_format',c('gtobj','dataframe'),envir=environment()),
                 check_option('method',c('auto','define'),envir=environment()))
  if(base::min(check_res)==0){message('Please check and re-try!');return(NULL)}
  
  if(!is.null(cat_var) && length(cat_var)>0){
    if(!base::all(cat_var %in% colnames(data))){
      del_cat_var <- base::setdiff(cat_var,colnames(data))
      message(sprintf('Categorical variable %s is null',paste0(del_cat_var,collapse = ',')))
      return(NULL)
    }
  }
  if(!is.null(cont_var) && length(cont_var)>0){
    if(!base::all(cont_var %in% colnames(data))){
      del_cont_var <- base::setdiff(cont_var,colnames(data))
      message(sprintf('Continuous variable %s is null',paste0(del_cont_var,collapse = ',')))
      return(NULL)
    }
  }
  
  if(is.null(cat_var) & is.null(cont_var) || is.null(cat_var) & is.null(cont_var) & is.null(group_var)){
    cat_var <- .detect_variable_types(data) %>% .[.=='categorical'] %>% names()
    cont_var <- .detect_variable_types(data) %>% .[.=='continuous'] %>% names()
  }
  
  if(is.null(group_var) || group_var %in% colnames(data)){
    if(method=='auto'){
      stat_res <- .gt_auto(data=data,group_var=group_var,cat_var=cat_var,cont_var=cont_var,out_format=out_format,digits=digits,pDigits=pDigits,missing=missing)
    }
    if(method=='define'){
      stat_res <- .gt_define(data=data,group_var=group_var,cat_var=cat_var,cont_var=cont_var,out_format=out_format,digits=digits,pDigits=pDigits,missing=missing)
    }
  }else{
    cat("Please check whether the group variable is a column name in the data!\n")
  }
  return(stat_res)
}








