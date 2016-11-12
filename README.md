# Australian Alpine shrub warming experimental and field surveys
Climate change is increasing fire frequency and severity worldwide, but it is not clear how the interaction between increases in temperatures and fire will affect threatened ecosystems. In this project we examine alpine shrub growth and survival at the critical seedling establishment stage using manipulative and natural experiments.

**Using a Open Top Chamber warming experiment we examine:**
* How experimental warming effects *Grevillea australis* recruitment
* How experimental warming effects on seedling growth and mortality data of four dominant Australian alpine shrubs
* How the interactive effects of inter-tussock gap size and experimental warming on shrub seedling growth and mortality
* How experimental warming influences rates of inter-tussock gap closure

**Using field surveys of shrub seedlings in burnt and unburnt alpine heathland we examine:**
* How fire, fire severity, altitude, adult density and topographic wetness influence shrub seedling occurrence and density and across the landscape
* How fire severity, altitude and topographic wetness influence maximum seedling heights 10 years post-fire.

## Rebuilding repository
We are committed to reproducible science. As such, this repository contains all the data and code necessary to fully reproduce our results. To facilitate this reproducibility the entire workflow has been written in [remake](https://github.com/richfitz/remake). Below outlines the instructions on how to clone this repository and build the entire analysis and figures.

First copy the repository to your local computer. Then open R in this directory.

**NOTE** *This project is currently under peer review and subject to change. If you wish to replicate the manuscript as found at [bioRxiv](http://biorxiv.org/content/early/2016/03/16/043919) please download the release version.*

Once this is done we must install `remake` dependencies that are not on CRAN.
To do this install [devtools](https://github.com/hadley/devtools) if you haven't already by running the following in R:
```
install.packages("devtools")
```
Now we can install `remake` (Not on CRAN)
```
devtools::install_github("richfitz/remake", dependencies=TRUE)
```

This project also depends on several packages. Now that `remake` is installed we can install them all by simply running:

```
remake::install_missing_packages()
```
Note: This will only install packages you haven't already installed. Some packages (e.g. `dplyr`, `ggplot2`, `tidyr`) have had recent updates that are not backwards compatible. Please check your package versions meet the software requirements mentioned below.


This project uses [rstan](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started), a package that provides a probabilistic programming language for Bayesian inference. Because this project runs 10 Bayesian models, we are using stan's inbuilt chain parallelisation to reduce computing time. rstan has undergone substantial changes recently and as such, we require that you have `rstan` 2.12.0 or greater.
For installing or upgrading rstan following the instructions [here](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started)


Lastly, in order to compile a pdf of the manuscript we require the installation of latex:

For Windows users install [MiKTeX](http://miktex.org/download)

For Mac users install [MacTeX](https://tug.org/mactex/mactex-download.html)

For Linux users install [TeX Live](https://www.tug.org/texlive/quickinstall.html)

NOTE: TeX Live users require installing additional add ons. This can be achieved by running the following in the terminal:
```
apt-get install texlive texlive-latex-extra texlive-humanities
```
Now we have everything we need to process the raw data, run the models, produce the figures and manuscript. We can do all of this using a single command in R.

```
remake::make() #NOTE: This involves running 11 stan models and can take up to 1 hour or longer depending on computer hardware specifications.
```

If you only wish to extract the processed (i.e. errors removed) datasets just run:

```
remake::make("export_processed_data") # Note this still requires some models to run in order to estimate missing poa distances.
```

A list of all available targets can be found within the `remake.yml` file. You can examine any one of them by simply using:

```
object <- remake::make("target_name")
```

##Software requirements

Below are the system requirements this workflow has been tested on:

`R` 3.3.2

**R packages tested with this workflow:**

 `knitr` v1.15 <br />
 `plyr` v1.8.4 <br />
 `dplyr` v0.5.0 <br />
 `tidyr` v0.6.0 <br />
 `rstan` v2.12.1 <br />
 `cowplot` v0.7.0 <br />
 `ggplot2` v2.2.0 <br />
 `grid` v3.3.1 <br />
 `lubridate` v1.6.0 <br />
 `reshape2` v1.4.2 <br />
 
##HAVING ISSUES?

Some packages are continually being upgraded which may result in some parts of the workflow failing. If this happens please report it to me and I will fix it ASAP. In the near future I will create a docker image with the relavant package versions to avoid this problem.

