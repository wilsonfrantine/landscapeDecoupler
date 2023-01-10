# landscapeDecoupler

An R package to help with landscape analisis

## How to install

```{r}
remotes::install_github("wilsonfrantine/landscapeDecoupler")
```
> The dependency `broom.Extra` has been removed from CRAN and the package will fail to install. You can workarround by installing the last vertion through the archive <a href="https://cran.r-project.org/src/contrib/Archive/broomExtra/"> broomExtra archive <\a>.

## How to use it

### Getting started
For the first time, you might find helpful to follow <a href="https://wilsonfrantine.github.io/landscapeDecoupler/"> <b>this vignette. </b> </a>

### Multifiting / Nested vs Decoupled
How to use **multifit** or **Nested vs Decoupled** approaches, see <a href="https://wilsonfrantine.github.io/landscapeDecoupler/Nested_vs_Decoupled.html"> <b> this vignette </b> </a>

## Paralellal computation

Based on our tests so far, parallelization has not returned significatively faster runs. However, you might want to run some large dataset in parallel strategies. The package is ready to parallelization with the **future** framework. We're still working on due to some overhead issues. To run in parallel: 

```{r}
library("landscapeDecoupler")
plan("multisession")

#then run your code...
```
To get back to the defaul you can either:

```{r}
plan("default")
#or
plan("sequential")
```
To get more help about paralellization strategies, you can do:

```{r}
?future::plan
```

## Reporting bugs

This is a prototype package. If you get any crash, please post here, or mail to wilsonfrantine@gmail.com .

Have fun!
