This repository is for the R package `thepack`, which includes all the necessary functions to produce the result of [my thesis project](https://github.com/AJV304/Thesis.git). This package was created under `R` version 4.4.2. 

The goal of this package is to simulate data according to the specific data-generating mechanism as described in the thesis: "Busy Making Other Plans: A Simulation Study on the Effects of Deviations from Preregistrations". This is done by generating data according to a baseline condition which follows a simple linear regression and then modifying that data to approximate 9 common deviations from preregistrations. Deviation conditions were simulated in three domains: Sample Size, Outlier Exclusion Criteria, and the Statistical Model. The `R` directory includes all functions written for this package. The functions will be discussed below in order of how the data was shaped. For more information on their specific input and output, please refer to the individual R scripts. For more information on the analyses and how to reproduce them, please see [my thesis repository](https://github.com/AJV304/Thesis.git).

- `dgm.R`: This function generates a set of synthetic data points based on specific parameters. This includes a dependent variable, an independent variable, covariates and more. 
- `baseline.R`: This function analyses a dataset (one generated using the dgm function) based on the baseline condition.
- `samplesize.R`: This function analyses a dataset (one generated using the dgm function) based on the four Sample Size deviation conditions.
- `outlier.R`: This function analyses a dataset (one generated using the dgm function) based on the two Outlier Exclusion Criteria deviation conditions.
- `models.R`: This function analyses a dataset (one generated using the dgm function) based on the three Statistical Model deviation conditions.
- `extr.R`: This function extracts the regression coefficient b1, the p-value and the confidence interval from an lm() model summary.
- `conditions.R`: This function runs `baseline.R`, `samplesize.R`, `outlier.R`, and `models.R` simultaneously.
- `analysis.R`: This fucntion generates data using the `dgm.R` and analyzes it for all conditions using `conditions.R`, combining the output into one dataframe.
- `choice.R`: This function selects one condition per iteration (from a dataframe as output by `analysis.R`), based on whether p-values are significant or not.
- `nsig.R`: This function counts the number of significant p-values per condition (from a dataframe as output by `analysis.R`).

This package is licensed under the MIT License. This license allows anyone to use, modify, and distribute this code freely, as long as the original copyright notice and license text are retained. For more information, see [LICENSE](https://github.com/AJV304/thepack/blob/751e0530e4082e49da51b1e73a5f0b3710a0b3f3/LICENSE).
