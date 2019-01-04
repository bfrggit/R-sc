# Smart-Space Multi-Sensor Calibration Planner and Simulator for R

This introduction page only covers the installation of R and its packages needed for this project.

Details on the use of this project are coming soon...

### Installation of R

Refer to [Download and Install R](https://cloud.r-project.org/).

This project has been tested working with R versions 3.5.0, 3.5.1, and 3.5.2.

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
