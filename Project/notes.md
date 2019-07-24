
# https://rstudio-pubs-static.s3.amazonaws.com/431668_94857a2237f24827b3fad12d6d9d851c.html
# http://blog.elliotmarsden.com/us_election_2016.html
# https://brilliantmaps.com/2016-county-election-map/
 
# Literature Review 

## Independent Relationship of Changes in Death Rates with Changes in US Presidential Voting
Link: https://link.springer.com/article/10.1007/s11606-018-4568-6
Very good resource to reference and take ideas from

- file:///Users/mayrop/Downloads/pols_w3245_2009_brown.pdf
- https://www.jstor.org/stable/pdf/2771413.pdf?refreqid=excelsior%3Ae489f6147ca41ef0465307bb41a41004
# What aspects make a change in either change parties or to vote for first time
# Talks about mainly religion & income
# New York elections 1940/44


## Participation Rates, Socioeconomic Class Biases, and Congressional Elections: A Crossvalidation
Link: https://www.jstor.org/stable/2111783 
Hypothesis: Class biases in voter turnout have not substantially increased since the 1960s
>> income is the appropriate measure of economic class bias

- https://essay.utwente.nl/73364/1/Drewer_BA_BMS.pdf
# Thesis about Comparing the influence of socioeconomic factors on participation in national elections and referendums
# Talks more about participation than selecting parties
# Check for references

- https://scholarworks.wmich.edu/cgi/viewcontent.cgi?referer=https://www.google.com/&httpsredir=1&article=2044&context=dissertations
# Dissertation: Who Voted?: Social Class and Participation in United States Presidential Elections
# Talks about participation over the years
# Might have good references. Check for references.

- https://www.demos.org/sites/default/files/publications/Why%20Voting%20Matters_0.pdf
# Talks about turnout
# Some nice graphs about comparing turnout by race and inciome
# Might have "ok" references

- file:///Users/mayrop/Downloads/Socioeconomic_Status_and_Nonvoting_A_Cross-Nationa.pdf
- https://www4.stat.ncsu.edu/~reich/ABA/code/election2016data
# Nice analysis of previous elections
# Check for EDA and for important columns

# Notes
- Two votes for every state because each has two senators and 1 vote for each congress person 
- Smallest states have at minimum 3 electoral college votes
- This allocation gives disproportionate weight to smaller rural states with low populations
- That also, more often then note, are VERY white demographically
- white folk are on average older then total population and markedly wealthier than other groups. 
- Both attributes also make them far more reliable voters

- https://medium.com/@urban_institute/how-to-create-state-and-county-maps-easily-in-r-577d29300bb2
# How to create maps in R

- https://www.kaggle.com/kirandipta/do-we-vote-in-our-own-best-interest
# Clustering of vars for presidential data
# Uses PCA for analysis

- https://www.kaggle.com/mpoegel/i-love-the-poorly-educated?scriptVersionId=171731
# Quick analysis regression about fraction of voters of trump & education

- https://www.analyticsvidhya.com/blog/2018/05/improve-model-performance-cross-validation-in-python-r/
# Quick blog post about cross validation
# Check!

- https://www.datacamp.com/community/tutorials/logistic-regression-R
# Logistic Regression in R Tutorial
# Really nice explanation about logistic regression
# Check for code examples

- https://web.stanford.edu/~hastie/glmnet/glmnet_alpha.html
# Really nice blog post about glmnet and the logistic penalized regression

- https://www4.stat.ncsu.edu/~reich/BigData/code/glmnet.html
# R exmaples for doing classifications
# K nearest neighbors
# Discriminant analysis
# Comparison of methods using Brier scores, classification accuracy
# Nice to check for learning about classification methods

- https://bookdown.org/roback/bookdown-bysh/ch-logreg.html
# Book about logistic regression in R
# Has formulas
# 6.5 Case Study: Reconstructing Alabama
# 6.5.4 Tests for significance of model coefficients
# 6.5.5 Confidence intervals for model coefficients
# 6.5.6 Testing for goodness of fit
# 6.5.7 Residuals for Binomial Regression
## CHECK!!!

- http://www.sthda.com/english/articles/36-classification-methods-essentials/149-penalized-logistic-regression-essentials-in-r-ridge-lasso-and-elastic-net/
# R code with lasso regression

- http://docs.h2o.ai/h2o/latest-stable/h2o-docs/data-science/algo-params/remove_collinear_columns.html
# remove_collinear_columns

http://www.chrisbilder.com/categorical/Chapter5/AllGOFTests.R
# Functions from class

https://www.r-bloggers.com/evaluating-logistic-regression-models/
# CHECK!
Evaluating Logistic Regression Models
- Goodness of Fit
- Likelihood Ratio Test
- Hosmer-Lemeshow Test
- Statistical Tests for Individual Predictors
- Wald Test
- Variable Importance
- Validation of Predicted Values
- Classification Rate
- K-Fold Cross Validation

file:///Users/mayrop/Downloads/275_LR_ACoP_poster_2015-10-01%20(1).pdf
- Explanation about multicollinearity and ridge regression - for SAS - check again


https://rcompanion.org/handbook/G_14.html
# Accuracy and Errors for Models
>> Not sure if it's only for regression or also logistic

https://cran.r-project.org/web/packages/MLmetrics/MLmetrics.pdf
# Package "MLmetrics"
# R functions RMSPE etc

https://pdfs.semanticscholar.org/5a89/1e686fedc226942727b76ba568602ac94006.pdf?_ga=2.208120274.1742245575.1562854939-375471410.1562854939
Title: Multicollinearity: What Is It, Why Should We Care, and How Can It Be
Controlled?
SAS article - may not be good reference but content is good

Collinearity is especially problematic when a model's purpose is explanation rather than prediction. In the case of explanation, it is more difficult for a model containing collinear variables to achieve significance of the different parameters. In the case of prediction, if the estimates end up being statistically significant, they are still only as reliable as any other variable in the model, and if they are not significant, then the sum of the coefficient is likely to be reliable. In summary if collinearity is found in a model testing
prediction, then one need only increase the sample size of the model. However, if collinearity is found in a model seeking to explain, then more intense measures are needed. The primary concern resulting from multicollinearity is that as the degree of collinearity increases, the regression model estimates of the coefficients become unstable and the standard errors for the coefficients become wildly inflated. 

Checking collinearity through tolerance (nothing below 0.1) and VIF


https://onlinelibrary.wiley.com/doi/pdf/10.1002/for.3980030206
Collinearity and Forecasting



http://www.sthda.com/english/wiki/print.php?id=237
>>>> Need to check for clustering variables


###########################################################################
###########################################################################



###########################################################################
###########################################################################

Code Examples

http://www.milanor.net/blog/cross-validation-for-predictive-analytics-using-r/
Cross-Validation for Predictive Analytics Using R
MUST try code for learning cross-validation
>> DONE

https://www.datacamp.com/community/tutorials/k-means-clustering-r
K-Means Clustering in R Tutorial
- MUST try code in case centroid is used

http://www.flutterbys.com.au/stats/tut/tut10.5a.html
Tutorial 10.5a - Logistic regression and proportional and percentage data
Really nice tutorial for logistic regression
- MUST try code



###########################################################################
###########################################################################

Books

https://onlinelibrary-wiley-com.ezproxy.lib.gla.ac.uk/doi/pdf/10.1002/9781118548387
Applied Logistic Regression
- > Pending to check


###########################################################################
###########################################################################

Articles

Title: Classification of gene microarrays by penalized logistic regression
https://academic.oup.com/biostatistics/article/5/3/427/310192
- Penalized logistic regression (PLR) vs SVM for classification

02-Article-Collinearity.pdf
Title: Collinearity: a review of methods to deal with it and a simulation study evaluating their performance
Cited by: 1771
https://onlinelibrary.wiley.com/doi/full/10.1111/j.1600-0587.2012.07348.x
https://onlinelibrary.wiley.com/doi/epdf/10.1111/j.1600-0587.2012.07348.x
# Prob not good resource for logistic
# Part I. When is collinearity a problem? 
# Part II. Spatio-temporal patterns in collinearity
# Part III. Methods for dealing with collinearity 
## Detect it: diagnostics 
## Removing collinearity prior to analysis 
>> Before analysis
- Clustering, PCA, K-means clustering
  - Central variable from cluster (biased)
  - Best regressor -> increase of Type 1 errors
- Cluster-independent methods
  - Select variables correlated |r| < 0.7
  - Sequential regression
>> Modelling with latent variables
- PCR (principal components regression)
- DR (Dimension reduction)
http://www.ecography.org/sites/ecography.org/files/appendix/e7348.pdf #CHECKKKK

>>> Could be used as a reference to say that for prediction collinearity is not an issue
=====
1) When the correct form of the functional relationship is known, collinearity does not harm the fitting and therefore prediction to changing collinearity structures. 
=====
Under very high collinearity, penalised methods are somewhat more robust, but here the issue of changes in collinearity structure also becomes graver. For predictions, our results indicate sensitivity to the way predictors correlate: small changes will affect predictions only moderately, but substantial changes lead to a dramatic loss of prediction accuracy.
=====
There are some situations in which the effects of collinearity have limited impact. If the main use of the model is to predict new cases within the range of the sampled data (i.e. to interpolate), the model will do this reliably as long as the collinearity between variables remains constant (Harrell 2001).
=====
 Harrell, F. E. Jr 2001. Regression modeling strategies  -  with appli-cations to linear models, logistic regression, and survival analysis.  -   Springer. 
 https://link-springer-com.ezproxy.lib.gla.ac.uk/content/pdf/10.1007%2F978-3-319-19425-7.pdf
 
 Collinearity does not affect predictions made on the same dataset used to
estimate the model parameters or on new data that have the same degree
of collinearity as the original data [470, pp. 379-381] as long as extreme
extrapolation is not attempted. Consider as two predictors the total and LDL
cholesterols that are highly correlated. If predictions are made at the same
combinations of total and LDL cholesterol that occurred in the training data,
no problem will arise. However, if one makes a prediction at an inconsistent
combination of these two variables, the predictions may be inaccurate and
have high standard errors.
Classical and Modern Regression with Applications 
https://lib.ugent.be/en/catalog/rug01:000851135

=========================

03-Slide-comparison-machine-learning.pdf
https://www.researchgate.net/publication/284190532_Comparison_of_Machine-Learning_Techniques_for_Handling_Multicollinearity_in_Big_Data_Analytics_and_High_Performance_Data_Mining
https://pdfs.semanticscholar.org/f15a/d84de4cecb278dab7e7d7b7e149dcee6861d.pdf
>> Quick comparison between logistic with multicollinear methods

04-Article-filzmoser_preprint.pdf
Title: Robust and sparse estimation methods for high dimensional linear and logistic regression
https://arxiv.org/pdf/1703.04951.pdf
- Nice article about lasso, ridge & elastic net
- Good resource to link to
- Has resources about robust logistic regression
>> Friedman et al. [13] suggested to minimize a penalized negative log-likelihood function

16-Article-logistic-regression-multicollinear
Robust and sparse estimation methods for high-dimensional linear andlogistic regression
- Comparison between elastic net, enet-LTS raw and enet-LTS

05-Thesis-forecast-presidency.pdf

#https://bookdown.org/roback/bookdown-bysh/ch-logreg.html#learning-objectives-5

# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/VOQCHQ

06-proportions-model.pdf
Title: A New Robust Regression Model for Proportions
https://projecteuclid.org/download/pdfview_1/euclid.ba/1354024464
> Beta regression model
# Check after done with logistic

07-Regularization.pdf
Title: Regularization and variable selection via the elastic net
https://web.stanford.edu/~hastie/Papers/B67.2%20(2005)%20301-320%20Zou%20&%20Hastie.pdf
- Seems a very good resource about elastic net
- Something to check when implementing, however, seems not to talk much about logistic regression

08-ridge-regression.pdf
Title: Lecture notes on ridge regression
https://arxiv.org/abs/1509.09169
- This is like a book on lasso/ridge etc
- Something worth checking is: Penalty parameter selection
- Meijer and Goeman (2013) describe a computationally efficient approximation of the leave-one-out crossvalidated loglikelihood

09-ShortCourseBetaRegression.pdf

10-variable-selection-in-logistic-regression.pdf
Title: Variable Selection of Correlated Predictors in Logistic Regression: Investigating the Diet-Heart Hypothesis
Electronic Theses, Treatises and Dissertations The Graduate School
- Pending to read

11-classificaiton-techniques.pdf
Title: Supervised Machine Learning: A Review of Classification Techniques
- Really nice resource about classification techniques
- There's a nice table at the end about comparison on classification techniques

12-logistic-a-brief-primer.pdf
Title: Logistic Regression: A Brief Primer
- One of my favs

13-logistic-check.pdf
Title: The practical value of logistic regression
- Very old paper
- Seems to use SAS

14-logistic-example.pdf
15-svn-vs-logistic.pdf
Title: Comparison between SVM and Logistic Regression: Which One is Better to Discriminate?
http://www.kurims.kyoto-u.ac.jp/EMIS/journals/RCE/V35/v35n2a03.pdf