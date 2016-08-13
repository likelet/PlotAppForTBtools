# PlotAppForTBtools
A shiny app for TBtools data plot

**Screen shoot of IDEA**
<img src="www/image/screenshot.png">


### Install dependencies
To check the dependencies installed correctly, this command can help users to check the status of each installation<br/>
```R
library("Packages for check")
```
Code for install dependencies R packages 
```R
cDep <- c("ggplot2","shiny","shinyBS","shinydashboard","ggthemes","DT")

###INSTALLED PACKAGES
#get installed list
inst <- packageStatus()$inst

#check and install DEPENDENCIES from CRAN
for(i in 1:length(cDep)){
  tag = which(inst$Package == cDep[i])
  if(length(tag)){
    remove.packages(cDep[i])
  }
  install.packages(cDep[i])
}

###Install shinysky for pretty shiny UI
if (!require("devtools"))
  install.packages("devtools")

```
### Install TBploter
To install the latest development build directly from GitHub, run this:

```R
if (!require("devtools"))
  install.packages("devtools")
devtools::install_github("likelet/PlotAppForTBtools")
```
### Designers:
Qi Zhao, zhaoqi3@mail2.sysu.edu.cn<br/>

### Developers:
Qi Zhao, zhaoqi3@mail2.sysu.edu.cn <br/>
Chenjie Chen,120509419@qq.com

### Maintainer:
Qi Zhao
Please feel free contact us. <br/>

### Copyright
MIT license

### Citation 
During developing