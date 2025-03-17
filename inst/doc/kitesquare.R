## -----------------------------------------------------------------------------
library(dplyr)
library(knitr)
library(ggplot2)
theme_set(theme_bw())


## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)


## ----setup--------------------------------------------------------------------
# library(kitesquare)
source("../R/kitesquare.R")


## -----------------------------------------------------------------------------
create_data <- function(xm, ym, bias=1){
  
  xlab=names(xm)
  ylab=names(ym)
  
  xm <- xm/sum(xm)
  ym <- ym/sum(ym)
  print(xlab)
  print(ylab)
  expand.grid(Mx=xm, My=ym) %>% 
        as_tibble() %>% print
  
  expand.grid(X=xlab, Y=ylab) %>% 
        as_tibble() %>% print
  
  
  df <-
    bind_cols(
      expand.grid(Mx=xm, My=ym) %>% 
        as_tibble(),
      expand.grid(X=xlab, Y=ylab) %>% 
        as_tibble() 
    ) %>% 
    mutate(expected=Mx*My)%>% 
    
    
    mutate(expected=100*expected) %>%
    mutate(across(c(X, Y), as.factor)) %>% 
    mutate(observed=expected*if_else(as.integer(X)==as.integer(Y), bias, 1)) 
    
  
  df <- df %>%
    select(X,Y,count=observed) %>% 
    # mutate(X=factor(X, labels=xlab, levels=1:length(xlab)),
    #        Y=factor(Y, labels=ylab, levels=1:length(ylab))) %>% 
    arrange(X,Y)
  
  
  df
}




## -----------------------------------------------------------------------------

bias=3
xm <- c(A=1,B=3)
ym <- c(U=2,V=3)

df_indep <- create_data(xm,ym)
df_dep <- create_data(xm,ym,bias)



## ----include=TRUE-------------------------------------------------------------
#| label: fig-dependence
#| fig-cap: Kite-square plots for independent and dependent variables.
#| fig-subcap:
#|   - "Independent variables, the plot resembles a kite inside a square"
#|   - "Dependent, top-left and bottom-right are higher, the others are lower than expected. Notice the patches in grey!"
#| layout-ncol: 2

kitesquare(df_indep, X, Y, count)
kitesquare(df_dep, "X", "Y", "count")


## ----include=TRUE-------------------------------------------------------------
#| label: fig-kite
#| fig-cap: Elements related to joint quantities.
#| fig-subcap:
#|   - Kite, representing $\mathbb{P}(X)\mathbb{P}(Y)$ or $E_{XY}$
#|   - Spars, representing $\mathbb{P}(X,Y)$ or $O_{XY}$
#| layout-ncol: 2


kitesquare(df_dep, "X", "Y", "count", bars=F, spars=F, intersect=F, square=F, chi2=F, normalize=T)
kitesquare(df_dep, "X", "Y", "count", bars=F, kite=F, intersect=F, square=F, chi2=F, normalize=T)


## ----include=TRUE-------------------------------------------------------------
#| label: fig-square
#| fig-cap: Elements related to conditional and marginal probabilities.
#| fig-subcap:
#|   - Square, indicating marginals $\mathbb{P}(X)$ and $\mathbb{P}(Y)$, or $M_X$ and $M_Y$, respectively.
#|   - Bars, indicating conditionals $\mathbb{P}(X|Y)$ and $\mathbb{P}(Y|X)$, or $O_{X|Y}$ and $O_{Y|X}$, respectively
#| layout-ncol: 2


kitesquare(df_dep, "X", "Y", "count", kite=F, bars=F, spars=F, intersect=F, square=T, chi2=F, normalize=T)
kitesquare(df_dep, "X", "Y", "count", kite=F, bars=T, spars=F, intersect=F, square=F, chi2=F, normalize=T)


## ----include=TRUE-------------------------------------------------------------
#| label: fig-itspatch
#| fig-cap: Additional plot elements.
#| fig-subcap:
#|   - Intersections, indicating marginals $\mathbb{P}(X)$ and $\mathbb{P}(Y)$, or $M_X$ and $M_Y$, respectively.
#|   - Patches, indicating $\chi^2$ for counts, and $\frac{\chi^2}{N}$ for normalized data.
#| layout-ncol: 2


kitesquare(df_dep, "X", "Y", "count", kite=F, bars=F, spars=F, intersect=T, square=F, chi2=F, normalize=T)
kitesquare(df_dep, "X", "Y", "count", kite=F, bars=F, spars=F, intersect=F, square=F, chi2=T, normalize=T)


## -----------------------------------------------------------------------------
df <- df_dep


## -----------------------------------------------------------------------------
#| eval: false
#| echo: true
#| include: true
## kitesquare(df, X, Y, count)


## ----include=TRUE-------------------------------------------------------------
#| label: tbl-contingency
#| tbl-cap: Contingency tables with counts for variables $X\in \{A,B\}$ and $Y \in {U,V}$.
#| tbl-subcap:
#|   - Independent variables
#|   - Dependent variables
#| layout-ncol: 2

kable(df_indep)
kable(df_dep)


## -----------------------------------------------------------------------------
xm_large <- c(A=3,B=2,C=4,D=1)
ym_large <- c(U=1,V=2,W=1)
df_3x4 <- create_data(xm_large, ym_large, bias=3)
df_2x4 <- create_data(xm_large, ym_large[1:2], bias=3)



## -----------------------------------------------------------------------------
#| include: true
#| echo: true
#| result: true
#| fig-cap: Kite-square plot for a 2x4 matrix, with the binary variable centered.
#| label: fig-centered

kitesquare(df_2x4, X, Y, count, fill=TRUE)


## -----------------------------------------------------------------------------
#| include: true
#| echo: true
#| result: true
#| fig-cap: Kite-square plot for a 2x4 matrix, with the binary variable non-centered.
#| label: fig-noncentered

kitesquare(df_2x4, X, Y, count, fill=TRUE, center=FALSE)

