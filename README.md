# Smart-Space Multi-Sensor Calibration Planner and Simulator for R

This introduction page only covers the installation of R and its packages needed for this project.

Details on the use of this project are coming soon...

## Important notice about Git LFS

This project leverages [Git LFS](https://github.com/git-lfs/git-lfs/) to manage archives of old scripts, results, and plots.
In order to clone or pull this project in full, make sure Git LFS is correctly installed and initialized.

If you do not plan to contribute to this project, you may also choose to download a **manually** created project archive in [Downloads](https://bitbucket.org/bfrgbit/r-sc/downloads/).
Note that automatically created archives for branches and tags will not contain Git LFS files due to Bitbucket limitations.

## Preparing environment

### Installation of R

Refer to [Download and Install R](https://cloud.r-project.org/).
Note that if you are using Linux (e.g. Ubuntu), you will need to add the official repo of the R project. The Linux distribution's repo does not always have the latest versions of R.
How this can be done is also described in the installation procedures (the link above).

This project has been tested working with R versions **3.5.0**, **3.5.1**, and **3.5.2**.
It is also known now that R 3.2.3 will not make this project work because OMPR (see dependencies) does not have a build for 3.2.3.

Note that the Gurobi MIP solver may require specific versions of R installation. If you plan to test out the Gurobi solver, please refer to the Gurobi documentation for the requirements.

Test if your R is installed correctly: both commands below should run without any error.

```sh
$ R --version
$ Rscript --version
```

### Dependencies

R packages can be installed in an interactive R session, using
```r
install.packages()
```

This project generally depends on the following R packages:

- `optparse`: Command-line argument parser.
- `dplyr`: Data manipulation.
- `magrittr`: Operators that improve code readability.
- `igraph`: Graph theory and algorithms (e.g. shortest path).
- `GA`: Genetic algorithm.
- `ompr`: [OMPR](https://dirkschumacher.github.io/ompr/index.html) (optimization modelling package).
- `ompr.roi`: ROI interface for OMPR.
- `ROI.plugin.glpk`: ROI plugin for GLPK (MIP solver).

Gurobi will need additional packages to be installed, including its own R library that is not currently available in CRAN, as well as `ROI.plugin.gurobi`. Given that Gurobi is not required for general-purpose simulations, we skip this section for now.

Plotting batch simulation results requires the following R packages:

- `ggplot2`: Data visualization (i.e. generation of good-looking plots).
- `reshape2`: Data pre-processing.
- `Cairo`: More advanced vector graphics, used to generate PDF files with embedded fonts.

## Important notice on running R scripts

All the R scripts in this project need to be run from the **project root**.
The project root is the folder you get when you run `git clone` or when you extract this project from an archive.

Change your working directory (a.k.a. PWD) to the **project root** and run all R scripts there, even if the script itself does not reside in the project root.

## Simulations

The main script for running simulations is `simu/simu.R`.

If you are using Linux or macOS and have cloned this project with Git, this script should be executable already. Simply change into the project root and run it with `simu/simu.R`.
If it is not yet executable, you can manually change its mode or run it with `Rscript simu/simu.R`.

Let's assume that you have installed all necessary packages.
Running the script without parameters will likely result in its usage page, where you can learn how to provide the simulation with datasets.

### Datasets

Simulations need at least **four** (4) data files to run:

- Sensor type specification: Calibration period and time.
- Distance (movement cost) matrix: Shortest paths between pairs of spots.
- Node deployment: Location of the nodes.
- Sensor presence: Sensor type availability on the nodes.

There are two ways to get these data:

1. Generate them using synthetic data generators.
2. Parse real-world traces.

The following commands can generate the dataset for the Paris scenario, as long as the raw data files (`route` and `nodes`) are available in `scenarios/raw_paris_tsv`.

```sh
$ scenarios/prep_types_paris.R
$ scenarios/prep_graph_paris_full.R
$ scenarios/prep_nodes_paris_example.R
```

### Single-case simulation

Run a single-case simulation to make sure the environment is configured correctly and the current code-base is intact.
You may also run a single-case simulation to **create individual paths** for mobile calibrators, which can be exported to interface with other applications.

The following command runs a single-case simulation using the dataset generated above.

```sh
$ simu/simu.R \
--sensor_file=scenarios/types_paris.RData \
--location_file=scenarios/location_paris_example.RData \
--presence_file=scenarios/presence_paris_example.RData \
--distance_file=scenarios/graph_paris_full.RData \
--selector=interval_1 \
--paranoid \
-x 1e+4 -y 1 -z 5 -w 0 \
--max_cost_worker=14400 \
--verbose \
--num_iters=5 \
--path_planner=combined_1 \
--output_file=tmp/example_paris_short.RData \
--keep_history
```

Note that `--keep_history` is required if you would like to export paths later.
The paths will be stored in the output file specified in the command-line.

To export the paths, use

```sh
$ ./parse_simu_output.R \
-I tmp/example_paris_short.RData \
--output_directory=tmp/example_paris_short_txt
```
