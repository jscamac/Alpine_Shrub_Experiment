# Australian Alpine shrub warming experimental and field surveys
Climate change is increasing fire frequency and severity worldwide, but it is not clear how the interaction between increases in temperatures and fire will affect threatened ecosystems. In this project we examine alpine shrub growth and survival at the critical seedling establishment stage using manipulative and natural experiments.

**Using a Open Top Chamber warming experiment we examine:**
* How experimental warming effects on seedling growth and mortality data of four dominant Australian alpine shrubs
* How the interactive effects of inter-tussock gap size and experimental warming on shrub seedling growth and mortality
* How experimental warming influences rates of inter-tussock gap closure

**Using field surveys of shrub seedlings in burnt and unburnt alpine heathland we examine:**
* How fire, fire severity, altitude, adult density and topographic wetness influence shrub seedling occurrence and density and across the landscape
* How fire severity, altitude and topographic wetness influence maximum seedling heights 10 years post-fire.

We are committed to reproducible science. As such, this repository contains all the data and code necessary to fully reproduce our results. To facilitate this reproducibility the entire workflow has been written in [remake](https://github.com/richfitz/remake). Below outlines the instructions on how to clone this repository and build the entire analysis and figures.

## Rebuilding repository
First copy the repository to your local computer.
Once this is done we must install `remake` dependencies that are not on CRAN.
To do this open an R session and install [devtools](https://github.com/hadley/devtools) if you haven't already.
```
install.packages("devtools")
```
Now install [storr](https://github.com/richfitz/storr) a `remake` dependency not on CRAN.
```
devtools::install_github("richfitz/storr", dependencies=TRUE)
```
Now we can install `remake` (also not on CRAN)
```
devtools::install_github("richfitz/remake", dependencies=TRUE)
```

This project also depends on several packages. Now that `remake` is installed we can install them all by simply running:

```
remake::install_missing_packages()
```
Now we have everything we need to reprocess the raw data, run the models and produce the figures. We can do all of this using a single command in R.

```
remake::make() #NOTE: This involves running 10 stan models and can take up to 1 hour
```
