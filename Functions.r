# Functions



# Dataset_sim: Generating data sets for testing procedure
# INPUT: number of sets/simulations (n_sets),
# probability of success/acceptance (p)
# sample size (n_applicants)
#OUTPUT: matrix, one sample per column, storing binary variable
# containing information about acceptance/rejection per observation/applicant
dataset_sim <- function(n_sets, p, n_applicants) {
  
  # error checks for p
  if (is.numeric(p) == FALSE) { 
    stop("invalid arguments")
  }
  
  if (length(p) > 1 || length(p) > 1) {
    stop("invalid arguments")
  }
  
  # error checks for n_sets & n_applicants
  if (!(n_sets %% 1 %in% 0)) { 
    stop("invalid arguments")
  }
  
  if (length(n_sets) > 1) {
    stop("invalid arguments")
  }
  
  if (!(n_applicants %% 1 %in% 0)) { 
    stop("invalid arguments")
  }
  
  if (length(n_applicants) > 1) {
    stop("invalid arguments")
  }

  q = 1 - p # reciprocal of p(acceptance)
  
  
  # generating empty matrix
  matr <- matrix(nrow = n_sets, ncol = n_applicants) 
  
  # generating binary variable (0,1) through sampling with given probability
    matr<- sapply(1 : n_sets, function(i){
      sample(x = c(0,1),
             size = n_applicants,
             replace = TRUE,
             prob = c(q, p))
    })

    
  return(matr)
}




# Z-test
# Function, as no defaut function is available in R stats package
# INPUT: 
# two vectors, containing binary data, for comparison
# alpha level
#OUTPUT:
# observed probabilities, z-score and p-value
z_test <- function(vec1, vec2, alpha = 0.05) {

  # input checks for alpha
  if (is.numeric(alpha) == FALSE) { 
    stop("invalid arguments")
  }
  
  
  if (alpha < 0 || alpha > 1) { 
    stop("invalid arguments")
  }
  
  # calculating properties 
  n1 <- length(vec1)
  n2 <- length(vec2) 
  p1 <- sum(vec1 == 1) / length(vec1) #observed proportions
  p2 <- sum(vec2 == 1) / length(vec2)
  q1 = 1 - p1
  q2 = 1 - p2
  
  
  delta = abs(p1 - p2) # difference in sample proportions
  hypoth_value = 0
  
  # standard error for proportions
  se = sqrt(((p1 * q1)/ n1) + ((p2 * q2)/ n2))
  
  # using absolute value to curve  lower-tail issues
  # as we conduct a two-sided test
  z_stat <- ((delta - hypoth_value) / se) 
  z_crit = qnorm(1 - (alpha / 2)) # defining critical value
  
  
  # two-sided
  p_value = 2 * (1 - pnorm(z_stat)) # checking
  

  # binding together results
  results <- list("p1" = p1, 
                  "p2" = p2,
                  "p_value" = p_value, 
                  "Z-score" = z_stat)
  
  return(results)
}





# Chi-squared test which tests if two proportions come from the
# same distribution. Comparing both proportions with the combined weighted mean
# proportion. 
# INPUT:
# two vectors, containing binary data
# underlying alpha level
# OUTPUT:
# p value and chi-squared score
chisquared_test <- function(vec1, vec2, alpha = 0.05) {
  
  # error checks for alpha
  if (is.numeric(alpha) == FALSE) { 
    stop("invalid arguments")
  }
  
  
  if (alpha < 0 || alpha > 1) { 
    stop("invalid arguments")
  }
  
  # calculating combined weighted mean proportion 
  total_vec <- c(vec1, vec2)
  p_hat <- sum(total_vec==1) / length(total_vec)
  q_hat <- 1 - p_hat
  
  # calculating properties for Chi Squared testing
  n_male <- length(vec1)
  n_female <- length(vec2)
  m_adm = sum(vec1 == 1)
  m_rej = sum(vec1 == 0)
  f_adm = sum(vec2 == 1)
  f_rej = sum(vec2 == 0)
  
  
  # Calculating Chi Squared score
  Chi_sq_score = sum((m_adm - (p_hat*n_male))^2 / (p_hat*n_male),
                     (m_rej - (q_hat*n_male))^2 / (q_hat*n_male),
                     (f_adm - (p_hat*n_female))^2 / (p_hat*n_female),
                     (f_rej - (q_hat*n_female))^2 / (q_hat*n_female))
  
  # df = (n_col-1 * n_row-1)
  # 2x2 table, resulting in df = (2-1)*(2-1)
  p_value <- pchisq(q = Chi_sq_score, df = 1, lower.tail = FALSE)
  
  # binding together the output
  output <- list(p_value = p_value,
                 Chi_score = Chi_sq_score)
  
  return(output)
  
}





# Function for running the tests (parametric & non-parametric) and conducting 
# power and size.
# INPUT:
# two matrices, one for male and one for female.
# underlying alpha level for testing
# Accepting/rejecting H=
# PROCEDURE:
# 1. Conducting p-values, using z_test() and chisquared_test() function
# 2. "wrangling" data sets for further calculations -> removing NaNs which 
# can occure if both p1 and p2 is so low that standard error calculated
# in tests equals zero.
# 3. Depending on whether H0 is TRUE or FALSE, calculating size and power.
# 4. Binding together output
# OUTPUT:
# p-values
# size/power
# wrangling information (i.e., how many NaN got removed)
simulation_testing <- function(matrix1, 
                               matrix2,
                               alpha_lvl = 0.05, 
                               NullHyp = c(TRUE, FALSE)) {
  
  # checking if input is in matrix format
  if (is.matrix(matrix1) == FALSE) {
    stop("invalid argument: input is not a matrix")
  }
  
  if (is.matrix(matrix2) == FALSE) {
    stop("invalid argument: input is not a matrix")
  }
  
  # NOTE: no need to check if alpha is in [0;1] as this is checked in 
  # z-test and chisqared test
  
  results_parametric <- rep(NA, ncol(matrix1))
  results_nonparametric <- rep(NA, ncol(matrix1))
  
  # z-test
  results_parametric <-   sapply(1 : ncol(matrix1), function(i){
   z_test(vec1 = matrix1[,i], vec2 = matrix2[,i], alpha = alpha_lvl)[3]
    })
  
  # Chi-squared test
  results_nonparametric <-   sapply(1 : ncol(matrix1), function(i){
    chisquared_test(vec1 = matrix1[,i], vec2 = matrix2[,i], alpha = alpha_lvl)[1]
  })
  
  
  # Wrangling
  results_parametric <- unlist(results_parametric) # converting data to vector
  results_nonparametric <-  unlist(results_nonparametric)
  
  # in case p1 and p2 are both 0, both tests are unable to calculate a p-value.
  # in such case, the missing p-value (= NA)is removed from the vector of p-values.
  # the amount of removed p-values is shown in the Output of the function.
  NaN_in_par <- sum(is.nan(results_parametric)) # count of NA values
  results_parametric <- results_parametric[!is.nan(results_parametric)] # removing NA values
  
  NaN_in_nonpar <- sum(is.nan(results_nonparametric))
  results_nonparametric <- results_nonparametric[!is.nan(results_nonparametric)]
  

  # testing for size
  # a test for size is only possible if Ho is true. 
  size_parametric <- NA
  size_nonparametric <- NA
  
  if (NullHyp == TRUE) {
    
    size_parametric = sum(results_parametric  < alpha_lvl) / length(results_parametric)
    size_nonparametric = sum(results_nonparametric < alpha_lvl) / length(results_nonparametric)
  }
  

  # power
  # testing if H0 is FALSE
  power_parametric <- NA
  power_nonparametric <- NA
  
  if (NullHyp == FALSE) {
    power_parametric <- sum(results_parametric < alpha_lvl) /
      length(results_parametric)
    
    
    power_nonparametric <- sum(results_nonparametric < alpha_lvl) /
      length(results_nonparametric)
  }
  
  # binding together outputs
  output <- list(p_value_parametric = results_parametric,
                 p_value_nonparametric = results_nonparametric,
                 size_par = size_parametric,
                 size_nonpar = size_nonparametric,
                 power_par = power_parametric,
                 power_nonpar = power_nonparametric,
                 removed_NaN_parametric = NaN_in_par,
                 removed_NaN_nonparametric = NaN_in_nonpar)
  
  
  return(output)
}

