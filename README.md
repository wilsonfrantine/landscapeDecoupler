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

### Getting started
For the first time, you might find helpful to follow <a href="https://wilsonfrantine.github.io/landscapeDecoupler/"> <b>this vignette. </b> </a>

### Multifiting / Nested vs Decoupled
How to use **multifit** or **Nested vs Decoupled** approaches, see <a href="https://wilsonfrantine.github.io/landscapeDecoupler/Nested_vs_Decoupled.html"> <b> this vignette </b> </a>

## Paralellal computation

Based on our tests so far, parallelization has not returned significatively faster runs. However, you might want to run some large dataset in parallel strategies. The package is prepared to parallelization with the **future's** framework. We've still working on that because we still having some overhead issues. Nonetheless, you might try it out on your own machine/cluster by using any strategy available on the **future's** framework. 

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

This is a prototype package. If you get any crash, please post here, or mail to wilsonfrantine@gmail.com .

Have fun!
