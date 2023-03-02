# MC-based-power-evaluation

*project based on computational statistics, using Monte-Carlo simulation techniques to solve a problem dealing with the differences in statistical power and size of two statistical tests (parametric Z-test and non-parametric χ2-test) under different conditions. It is strongly recommended to read the executive summary alongside the code.*

## Abstract (extract from executive summary)

The size and power of a statistical test define their reliability under certain conditions. This paper examines how a Z-test and a χ2-test perform in different scenarios through empirical simulation methods. Gender- grouped proportions of students admitted to a higher-education institute are compared, as the admission proportions of UC Berkeley from 1973 serve as a loose framework for data generation. The analysis shows that both tests are mainly affected by the number of applicants and the difference in proportions of acceptance. Given a distinct difference in proportions, significantly less applicants are needed for reliable results. In contrast, both tests show a lower level of reliability (expressed as power) if a large majority of applicants belong to one gender. For a given gender bias, the parametric test performs better for few applicants compared to the non-parametric test, while this difference decreases with increasing sample size. In a scenario where admission is not subject to a gender bias, test performance (expressed as size) shows no affection by the number of applicants and is generally in line with the underlying α-level used for testing.

## Overview / Problem  (extract from executive summary)

This project is an empirical analysis of the statistical power and size of two appropriate tests to answer the research question whether UC Berkeley’s student admission rate from 1973 was subject to a gender bias. Although the 1973 admission is a famous example for the Simpson paradox, this work will not answer the presence of a gender bias, but rather focus on the quality of statistical tests which could be used to answer such research question. As the statistical size and power of tests are a crucial factor in survey design , the conducted analysis gives valuable information about the sample size and the minimum effect size which are required for reliable results from both statistical tests.

## Data

The project is based on the 1973 admission data from UC Berkeley. The data is included in the **dslabs** library in R and can be accessed with **dslabs::admissions**.

## Code and Scripts

*The whole project is  written in R. The R packages on which each script depends are stated at the beginning of each script.*

**CODE.R**: The main R file, containing data wrangling and the analysis.

**Functions.R**: This file is loaded within the main file and contains functions for the parametric and non-parametric test, a function for the simulation of data sets and a function combining the whole testing process in one function.

