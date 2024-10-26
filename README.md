# landscapeDecoupler

`landscapeDecoupler` is an R package designed to perform multi-scale landscape analysis. It provides tools to decouple nested spatial patterns and assess landscape metrics, helping landscape ecologists and conservation scientists analyze spatial data efficiently.

## Features

- Tools for calculating landscape metrics across multiple scales.
- Nested and decoupled approaches for landscape analysis.
- Integration with `raster`, `sf`, and `landscapemetrics` for handling spatial and raster data.
- Experimental support for parallel processing using the `future` package.

## Installation

You can install the development version of `landscapeDecoupler` from GitHub:

```r
# Install from GitHub
remotes::install_github("wilsonfrantine/landscapeDecoupler")
```

## How to Use

### Example

```r
# Load the package
library(landscapeDecoupler)

# Load example data
data(example_landscape)

# Calculate landscape metrics
metrics <- calculate_metrics(example_landscape)

# Plot metrics
plot(metrics)
```

For detailed usage examples and tutorials, please check out our [vignettes](https://wilsonfrantine.github.io/landscapeDecoupler/).

## Parallel Computation

The package offers support for parallel computation using the `future` package. To enable parallel processing, set up the plan as follows:

```r
library(future)
plan(multisession)

# Then run your code...
metrics <- calculate_metrics(large_landscape)
```

To return to sequential processing, use:

```r
plan(sequential)
```

## How to Cite

If you use the `landscapeDecoupler` package in your research, please cite it as follows:

```
Frantine-Silva, W. (2024). landscapeDecoupler: A package for multi-scale landscape analysis. R package version 0.3.0. https://doi.org/10.5281/zenodo.13997058
```

**BibTeX citation:**
```bibtex
@Manual{,
  title = {landscapeDecoupler: A package for multi-scale landscape analysis},
  author = {Wilson Frantine-Silva},
  year = {2024},
  note = {R package version 0.3.0},
  doi = {10.5281/zenodo.13997058},
  url = {https://doi.org/10.5281/zenodo.13997058}
}
```

## Reporting Bugs

This is a preview version. If you encounter any bugs or issues, please report them on the [GitHub issue tracker](https://github.com/wilsonfrantine/landscapeDecoupler/issues), or contact me at wilsonfrantine@gmail.com.
