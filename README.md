# landscapeDecoupler

An R package to help with landscape decoupling

## How to install

If you wanna a simple installation

```{r}
library("devtools")
install_github("wilsonfrantine/landscapeDecoupler")
```

If you also wanna vignettes locally

```{r}
library("devtools")
install_github("wilsonfrantine/landscapeDecoupler", vignettes=TRUE)
```

## How to use it

For the first time, you might find helpful to follow <a href="https://wilsonfrantine.github.io/landscapeDecoupler/"> <b>this vignette. </b> </a>

If you wanna see how to use **regressions with multifit** function and a simple comparison between **Nested** and **Decoupled** approaches, see <a href="https://wilsonfrantine.github.io/landscapeDecoupler/Nested_vs_Decoupled.html"> this vignette </b> </a>

Their you will find more information about the pacakge and how to use it.

## Paralellal computation

Based on our test so far, parallelization has not been returned signficativelly faster runs. However, you might want to run some really large dataset in parallel strategies. The package is prepared to paralellization with future framework. We've still working on that because we still having some overhead issues. Nonetheless, you might try it out on your own machine/cluster by using any strategy available in _future_ framework.

```{r}
library("landscapeDecoupler")
plan("multisession")

#then run your code...
```
To get back to the defaul you might either:

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

This is a prototype package. If you get any crash, please post here, or mail me.

Have fun!
