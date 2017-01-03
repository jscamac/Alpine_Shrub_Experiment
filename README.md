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

## Reproducing analysis
We are committed to reproducible science. As such, this repository contains all the data and code necessary to fully reproduce our results. To facilitate the reproducibility of this work, we have created a docker image and set up the entire workflow using [remake](https://github.com/richfitz/remake). Below we outline the steps required to reproduce the analyses, figures and manuscript.

### Copy repository
First copy the repository to your a desired directory on you local computer. 

This can either be done using the terminal (assuming git is installed)

```
git clone git@github.com:jscamac/Alpine_Shrub_Experiment.git
```

Or can be downloaded manually by clicking [here](https://github.com/jscamac/Alpine_Shrub_Experiment/archive/master.zip).

**NOTE:** *This step isn't strictly necessary, but is useful so that docker has a local directory that it can copy results to.*

## Setting up Docker
Next we set up a Docker virtual machine. If you haven't installed docker please see [here](https://www.docker.com/products/overview). Here we use Docker because it can readily be used across platforms and is set to install the appropriate software, and software versions used in the original analysis. As such it hopefully safeguards this work from potential changes in software and cross platform issues.

We can set up docker two ways. The simplest, fastest and *preferred approach* is to pull docker image we have already created:

```
docker pull jscamac/alpine_shrub_experiment
```
This image contains all required software (e.g. R, Latex, R packages). Furthermore, it contains the software versions used to originally run these analyses.


We can also rebuild it from scratch, although this option is much slower as it requires recompiling the entire image**. To do this open a terminal, navigate to the repository and run:

```
docker build -t Alpine_Shrub_Experiment .

```

## Rerunning workflow

Now we are all set to reproduce this manuscript!

Start up the Docker container (i.e. the virtual machine containing the environment) by opening a terminal and running:

**For Mac & Linux users**

```
docker run -v /Users/path/to/Alpine_Shrub_Experiment:/home/Alpine_Shrub_Experiment  -it jscamac/alpine_shrub_experiment
```


**For Windows users**

```
docker run -v c:\path\to\Alpine_Shrub_Experiment:/home/Alpine_Shrub_Experiment  -it jscamac/alpine_shrub_experiment
```

The above creates a Docker container (i.e. a virtual machine) and opens the terminal in `R`. The flag `-v` mounts the host directory `/Users/path/to/Alpine_Shrub_Experiment`, into the container at `/home/Alpine_Shrub_Experiment`. What this allows is for any results produced in the container to automatically be saved onto the local directory. This means that you can play with the results, data and figures outside the docker container later.

Now the final stage is to rerun the entire workflow by simply running:

```
remake::make()
```
**NOTE:** *The above function will process the data, run 11 [stan](http://mc-stan.org) models, produce the figures and compile a pdf of the manuscript. Depending on the local machine this can take anywhere from 1 to 2 hours.*


You don't have to rerun all components of this project. If you are interested in a particular component you can simply look at the `remake.yml` file find the appropriate component you want to run and simply run the relevant target name. It will build all the relevant dependencies needed to produce that particular component.

For example. Lets say you were just interested in exporting a particular dataset - say the complete/processed OTC experiment dataset. This could be extracted within the docker container by running:

```
otc_data <- remake::make("complete_otc_seedling_data")
```
 
## Docker Image metadata

| Docker Hub Build Status and URL                                | Image Size
| :-----------------------------------------                     | :--------------
| [good](https://registry.hub.docker.com/u/jscamac/alpine_shrub_experiment/)  | [![Layers and Size](https://images.microbadger.com/badges/image/jscamac/alpine_shrub_experiment.svg)](https://registry.hub.docker.com/u/jscamac/alpine_shrub_experiment/)

## Problems?
If you have any problems getting the workflow to run please create an issue and I will endevour to remedy it ASAP.
