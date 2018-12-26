# riskyrApp

The `riskyrApp` is an interactive `R Shiny` application that complements the R package `riskyr`. 

## Goal

`riskyrApp` is a teaching tool that showcases some functionality of the R package `riskyr`. 
The application is written in [R Shiny](https://shiny.rstudio.com/) and allows using the `riskyr` toolbox without any need for coding.

The main goal of both `riskyr` and `riskyrApp` is to communicate and explain common risk literacy measures in a transparent fashion by providing a set of powerful and interactive representations. 

## Information

- The current release of `riskyr` is available from [CRAN](https://CRAN.R-project.org/) at  <https://CRAN.R-project.org/package=riskyr>: 

```{r}
install.packages("riskyr")  # install riskyr from CRAN client
library("riskyr")           # load to use the package
```

- The most recent development version can be installed from its [GitHub](https://github.com) repository at <https://github.com/hneth/riskyr/>: 

```{r}
# install.packages("devtools")
devtools::install_github("hneth/riskyr")
```

- An interactive online version is available at [riskyr.org](http://riskyr.org).


## About

<!-- uni.kn logo and link to SPDS: -->  
<!-- ![](./www/uniKn_logo.png) --> 
<a href="https://www.spds.uni-konstanz.de/">
<!--<img src = "./www/uniKn_logo.png" alt = "spds.uni.kn" style = "width: 300px; float: right; border:20;"/> --> 
<img src = "./www/uniKn_logo_s.png" alt = "spds.uni.kn" align = "right" style = "float: right; border:40;"/>
</a>

`riskyr` is developed at the [University of Konstanz](https://www.uni-konstanz.de/en/) 
and originated out of a series of lectures and workshops on risk literacy. 
Its primary designers are 
[Hansjörg Neth](https://www.spds.uni-konstanz.de/hans-neth), 
[Felix Gaisbauer](https://www.spds.uni-konstanz.de/felix-gaisbauer), 
[Nico Gradwohl](https://www.spds.uni-konstanz.de/nico-gradwohl), and 
[Wolfgang Gaissmaier](https://www.spds.uni-konstanz.de/prof-dr-wolfgang-gaissmaier), 
who are researchers at the department of 
[Social Psychology and Decision Sciences](https://www.spds.uni-konstanz.de) at the 
[University of Konstanz](https://www.uni-konstanz.de/en/), Germany. 


### Contact

We appreciate your feedback, comments, or questions. 

- Please report any `riskyr`-related issues at <https://github.com/hneth/riskyr/issues>.

- For general inquiries, please email us at <contact.riskyr@gmail.com>. 

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
