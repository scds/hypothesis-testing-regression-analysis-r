# WORKSHOP: HYPOTHESIS TESTING AND REGRESSION ANALYSIS IN R
# March 30th, 2022
# DASH, McMaster University




  # Load library
  library(stats)
  library(datasets)
  library(datarium)
  library(BSDA)
  library(PairedData)
  library(mlbench)
  
  # Install packages
  install.packages("stats")
  install.packages("datasets")
  install.packages("datarium")
  install.packages("BSDA")
  install.packages("PairedData")
  install.packages("mlbench")


  
  
  
  
  
  
  
  
  
  # Z test
  # We need to know the standard deviations
  
  # One sample
  a = c(88, 92, 94, 94, 96, 97, 97, 98, 99, 99,
        105, 109, 109, 109, 110, 111, 111, 113, 114, 115)
  
  sd_a = sd(a)
  
  One_Z = z.test(a, alternative = "two.sided", mu = 100, sigma.x = sd)
  One_Z
  
  # Two sample 
  
  b = c(90, 91, 91, 91, 95, 95, 99, 99, 100, 109,
        109, 114, 115, 116, 117, 119, 128, 129, 130, 133)
  
  sd_b = sd(b)
  
  Two_Z = z.test(x=a, y=b, alternative = "greater", mu = 0, sigma.x = sd_a,
                 sigma.y = sd_b)

  
  
  
  
  
  
  
  
  
  
  
  
  # T-test
  
  # One sample T test
  x = rnorm(50)
  
  One_ttest = t.test(x, mu = 5, alternative = "two.sided")
  
  # Two sample T test
  #create series of random numbers
  
  
  x1 = rnorm(40)
  y1 = rnorm(40)
  
  Two_ttest = t.test(x1,y1, mu = 0, alternative = "two.sided")
  
  
  
  
  
  
  
  
  # Alternative way
  datarium::genderweight
  
  tt = t.test(weight ~ group, data = genderweight)
  
  # t.test(x, y = NULL,
  #    alternative = c("two.sided", "less", "greater"),
  #    mu = 0, paired = FALSE, var.equal = FALSE,
  #    conf.level = 0.95, .)
  
  
  
  
  
  
  
  
  
  # Paired T test
  # Weight of mice before and after the treatment
  
  Paired_ttest = t.test(mice2$before, mice2$after, paired = TRUE)
  Paired_ttest
  
  
  
  
  
  
  
  
  
  
  # Exercise 1
  # Run the following tests and analyse the results
  # 1.1 Create two series of random numbers and run a One tailed Independent sample t test.
  
  
  
  # 1.2 Using the following data set run a paired t test
  data("Anorexia")
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # ANOVA
  # One way ANOVA
  data <- read.csv("crop.data.csv", header = TRUE)
  
  OneANOVA = aov(yield ~ fertilizer, data = data)
  summary(OneANOVA)
  
  #Two way ANOVA
  TwoANOVA = aov(yield ~ fertilizer + density , data = data)
  summary(TwoANOVA)
  
  # Exercise 2: Using the data set "stress" in package "datarium" perform
  # two way ANOVA
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # Regression
  
  # Linear regression
  data("BostonHousing")
  
  ols = lm(medv ~ rm, data = BostonHousing)
  summary(ols)
  
  
  # Multiple linear regression
  ols1 = lm(medv ~ crim + zn + indus + rm + dis + rad 
            + tax, data = BostonHousing)
  summary(ols1)
  
  
  data("longley")
  
  ols3 = lm(GNP ~ Unemployed + longley$Armed.Forces + Population + Employed,
            data = longley)
  summary(ols3)
  
  
  
  
  
  
  
  
  
  # Correlation
  
  datasets::mtcars
  
  corr = cor.test(mtcars$mpg, mtcars$hp, method = "pearson")
  
  
  
  
  
  
  
  
  # Exercise 3: Run regression and test for assumption of normality 
  # and multicolinearity
  # Using command library(help="datasets") look at the list of datasets available
  # pick one data set and run regression analysis
  
  
  
  
  
  