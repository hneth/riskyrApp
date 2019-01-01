# riskyrApp

The `riskyrApp` is an interactive application that complements the R package `riskyr`. 

## Goal

`riskyrApp` is a teaching tool that showcases some functionality of the R package `riskyr`. 
The application is written in [R Shiny](https://shiny.rstudio.com/) and allows using the `riskyr` toolbox without any need for coding.

The main goal of both `riskyr` and `riskyrApp` is to communicate and explain common risk literacy measures in a transparent fashion by providing a set of powerful and interactive representations. 

## Installation

- The current release of `riskyr` is available from [CRAN](https://CRAN.R-project.org/) at <https://CRAN.R-project.org/package=riskyr>: 

```{r}
install.packages("riskyr")  # install riskyr from CRAN client
library("riskyr")           # load the package
```

- The most recent development versions can be installed from their [GitHub](https://github.com) repositories at 
    - R package code: <https://github.com/hneth/riskyr>
    - R Shiny code:   <https://github.com/hneth/riskyrApp/>

```{r}
# install.packages("devtools")
devtools::install_github("hneth/riskyr")
devtools::install_github("hneth/riskyrApp")
```

## About

<!-- uni.kn logo and link to SPDS: -->  
<!-- ![](./www/uniKn_logo.png) --> 
<a href="https://www.spds.uni-konstanz.de/">
<!--<img src = "./www/uniKn_logo.png" alt = "spds.uni.kn" style = "width: 300px; float: right; border:20;"/> --> 
<img src = "./www/uniKn_logo_s.png" alt = "spds.uni.kn" align = "right" style = "float: right; border:40;"/>
</a>

`riskyr` originated out of a series of lectures and workshops on risk literacy. 
Its primary designers are 
[Hansjörg Neth](https://www.spds.uni-konstanz.de/hans-neth), 
[Felix Gaisbauer](https://www.spds.uni-konstanz.de/felix-gaisbauer), 
[Nico Gradwohl](https://www.spds.uni-konstanz.de/nico-gradwohl), and 
[Wolfgang Gaissmaier](https://www.spds.uni-konstanz.de/prof-dr-wolfgang-gaissmaier), 
who are researchers at the department of 
[Social Psychology and Decision Sciences](https://www.spds.uni-konstanz.de) at the 
[University of Konstanz](https://www.uni-konstanz.de/en/), Germany. 


### Resources

The following resources and versions are currently available:

Type: | Version: | URL: |        
:-------------- |:-------------|:--------------------|
A. `riskyr` (R package): | [Release version](https://CRAN.R-project.org/package=riskyr) | <https://CRAN.R-project.org/package=riskyr> |
\                    | [Development version](https://github.com/hneth/riskyr)       | <https://github.com/hneth/riskyr> | 
B. `riskyrApp` (R Shiny): | [Online version](http://riskyr.org) | <http://riskyr.org> | 
\              | [Development version](https://github.com/hneth/riskyrApp) | <https://github.com/hneth/riskyrApp> | 
C. Online documentation: | [Release version](https://hneth.github.io/riskyr)  | <https://hneth.github.io/riskyr> | 
\                        | [Development version](https://hneth.github.io/riskyr/dev)  | <https://hneth.github.io/riskyr/dev> | 


### Contact

We appreciate your feedback, comments, or questions. 

- Please report any `riskyr`-related issues at <https://github.com/hneth/riskyr/issues>. 

- Email us at <contact.riskyr@gmail.com> if you want to modify or share this software. 

<!-- riskyr logo: -->  
<a href = "https://github.com/hneth/riskyr">
<img src = "./www/riskyr_cube_s.png" alt = "riskyr" align = "right" style = "float: right; border:40;"/>
</a>

### Reference

To cite `riskyr` in derivations and publications use:

-  Neth, H., Gaisbauer, F., Gradwohl, N., & Gaissmaier, W. (2018).    
    riskyr: A toolbox for rendering risk literacy more transparent.    
    Social Psychology and Decision Sciences, University of Konstanz, Germany.    
    Computer software (R package version 0.2.0, Dec. 20, 2018).    
    Retrieved from <https://CRAN.R-project.org/package=riskyr>.   

A BibTeX entry for LaTeX users is: 

    @Manual{riskyr,
      title = {riskyr: A toolbox for rendering risk literacy more transparent},
      author = {Hansjörg Neth and Felix Gaisbauer and Nico Gradwohl and Wolfgang Gaissmaier},
      year = {2018},
      organization = {Social Psychology and Decision Sciences, University of Konstanz},
      address = {Konstanz, Germany},
      note = {R package (version 0.2.0, Dec. 20, 2018)},
      url = {https://CRAN.R-project.org/package=riskyr},
      }    
    
Calling `citation("riskyr")` in the package also displays this information.

<!-- eof. --> 
