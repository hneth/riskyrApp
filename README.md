# riskyrApp



The `riskyrApp` is an interactive `R Shiny` application that complements the R package `riskyr`. 



## Goal

The goal of the `riskyrApp` is to communicate and explain common risk literacy measures by providing simple, transparent and interactive representations.

The application is written in [R Shiny](https://shiny.rstudio.com/) and allows using the `riskyr` toolbox without any need for coding.


## Information

The current release of `riskyr` is available from [CRAN](https://CRAN.R-project.org/) at  <https://CRAN.R-project.org/package=riskyr>: 

```{r}
install.packages("riskyr")  # install riskyr from CRAN client
library("riskyr")           # load to use the package
```


The most recent development version can be installed from its [GitHub](https://github.com) repository at <https://github.com/hneth/riskyr/>: 

```{r}
# install.packages("devtools")
devtools::install_github("hneth/riskyr")
```

Visit `riskyr` on [GitHub](https://github.com/hneth/riskyr) for additional information. 



## About

<!-- uni.kn logo and link to SPDS: -->  
<!-- ![](./www/uniKn_logo.png) --> 
<a href="https://www.spds.uni-konstanz.de/">
<!--<img src = "./www/uniKn_logo.png" alt = "spds.uni.kn" style = "width: 300px; float: right; border:20;"/> --> 
<img src = "./www/uniKn_logo_s.png" alt = "spds.uni.kn" align = "right" style = "float: right; border:20;"/>
</a>

`riskyr` originated out of a series of lectures and workshops on risk literacy in spring/summer 2017. 

Its primary developers and designers are 
[Hansjörg Neth](https://www.spds.uni-konstanz.de/hans-neth), 
[Felix Gaisbauer](https://www.spds.uni-konstanz.de/felix-gaisbauer), and 
[Nico Gradwohl](https://www.spds.uni-konstanz.de/nico-gradwohl), 
who are researchers at the department of 
[Social Psychology and Decision Sciences](https://www.spds.uni-konstanz.de) at the 
[University of Konstanz](https://www.uni-konstanz.de/en/), Germany. 

Please email at <contact.riskyr@gmail.com>  in case you want to use, adapt, or share this software.


### Contact

We appreciate your feedback, comments, or questions. 

- Please report any `riskyr`-related issues at <https://github.com/hneth/riskyr/issues>.

- For general inquiries, please email us at <contact.riskyr@gmail.com>. 


### Reference

<!-- riskyr logo: -->  
<a href = "https://github.com/hneth/riskyr">

<img src = "./www/riskyr_cube_s.png" alt = "riskyr" align = "right" style = "float: right; border:20;"/>
</a>


To cite `riskyr` in derivations and publications use:

-  Neth, H., Gaisbauer, F., Gradwohl, N., & Gaissmaier, W. (2018).    
    riskyr: A toolbox for rendering risk literacy more transparent.    
    Social Psychology and Decision Sciences, University of Konstanz, Germany.    
    Computer software (R package version 0.1.0, Feb. 19, 2018).    
    Retrieved from <https://CRAN.R-project.org/package=riskyr>.   

A BibTeX entry for LaTeX users is: 

    @Manual{riskyr,
      title = {riskyr: A toolbox for rendering risk literacy more transparent},
      author = {Hansjörg Neth and Felix Gaisbauer and Nico Gradwohl and Wolfgang Gaissmaier},
      year = {2018},
      organization = {Social Psychology and Decision Sciences, University of Konstanz},
      address = {Konstanz, Germany},
      note = {R package (version 0.1.0, Feb. 19, 2018)},
      url = {https://CRAN.R-project.org/package=riskyr},
      }    
    
Calling `citation("riskyr")` in the package also displays this information.

