# Make table1 in R<img src="inst/figures/imgfile.png" align="right" height="138"/>

## :writing_hand: Author

Huiyao Chen

------------------------------------------------------------------------

## :arrow_double_down: Installation

Install the gttable1 package via the Github repository:

``` r
# install.packages("remotes")   #In case you have not installed it.
remotes::install_github("chenhy-lab/gttable1")
```

------------------------------------------------------------------------
### Input

<img src="inst/figures/input.png" align="middle" height="150"/>

### Output

<img src="inst/figures/output.png" align="middle" height="400"/>

------------------------------------------------------------------------

### Demo script

``` r
library(gttable1)
data(pbc, package = "survival")
cont_var <- c("bili","chol","copper","alk.phos","trig") # continuous variables
cat_var <- c("sex","status","stage") # category variables
group_var <- "trt" # group variable
tmp <- gttable1(data=pbc,group_var=group_var,cat_var=cat_var,cont_var=cont_var,out_format='gtobj',pDigits=3,method='auto')
tmp <- gttable1(data=pbc,group_var=group_var,cat_var=NULL,cont_var=cont_var,out_format='dataframe',pDigits=3,method='auto')
tmp <- gttable1(data=pbc,cont_var=cont_var,out_format='gtobj',pDigits=3,method='auto')
tmp <- gttable1(data=pbc,cat_var=cat_var,out_format='gtobj',pDigits=3,method='auto')
tmp <- gttable1(data=pbc,out_format='gtobj',pDigits=3,method='auto')

tmp <- gttable1(data=pbc,group_var=group_var,cat_var=cat_var,cont_var=cont_var,out_format='gtobj',pDigits=3,method='define')
tmp <- gttable1(data=pbc,group_var=group_var,cat_var=cat_var,cont_var=cont_var,out_format='dataframe',pDigits=3,method='define')
tmp <- gttable1(data=pbc,group_var=group_var,cont_var=cont_var,out_format='dataframe',pDigits=3,method='define')
tmp <- gttable1(data=pbc,cont_var=cont_var,out_format='gtobj',pDigits=3,method='define')
tmp <- gttable1(data=pbc,cat_var=cat_var,out_format='gtobj',pDigits=3,method='define')
tmp <- gttable1(data=pbc,out_format='dataframe',pDigits=3,method='define')
tmp <- gttable1(data=pbc,out_format='gtobj',pDigits=3,method='define')
tmp <- gttable1(data=pbc,out_format='gtobj',pDigits=3,method='define',missing="always",missing_text="missing")
```


## Related Tools

- [gtsummary](https://www.danieldsjoberg.com/gtsummary/): A package provides 
 an elegant and flexible way to create publication-ready analytical and summary tables using the R programming language.
