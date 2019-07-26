# Notes

## Literature Review 

### The Design and Analysis of Benchmark Experiments
- **Link**: https://homepage.boku.ac.at/leisch/papers/Hothorn+Leisch+Zeileis-2005.pdf
- **Link (Code)**: https://topepo.github.io/caret/model-training-and-tuning.html
- **Link (Code)**: http://matthewalanham.com/Students/2018_MWDSI_R%20caret%20paper.pdf
- **Link (Code)**: https://www.rdocumentation.org/packages/bestglm/versions/0.37/topics/bestglm

### Independent Relationship of Changes in Death Rates with Changes in US Presidential Voting
- **Link**: https://link.springer.com/article/10.1007/s11606-018-4568-6  
- Very good resource to reference and take ideas from  

### Social Status and Political Behavior
- **Link**: https://www.jstor.org/stable/pdf/2771413.pdf?refreqid=excelsior%3Ae489f6147ca41ef0465307bb41a41004
- What aspects make a change in either change parties or to vote for first time  
- Talks about mainly religion & income  
- New York elections 1940/44  

### Participation Rates, Socioeconomic Class Biases, and Congressional Elections: A Crossvalidation
- **Link**: https://www.jstor.org/stable/2111783    
- **Hypothesis**: Class biases in voter turnout have not substantially increased since the 1960s income is the appropriate measure of economic class bias

### Comparing the influence of socioeconomic factors on participation in national elections and referendums
- Type: Thesis
- Link: https://essay.utwente.nl/73364/1/Drewer_BA_BMS.pdf
- Talks more about participation than selecting parties
- Check for references

### Who Voted?: Social Class and Participation in United States Presidential Elections
- Type: Dissertation
- **Link**: https://scholarworks.wmich.edu/cgi/viewcontent.cgi?referer=https://www.google.com/&httpsredir=1&article=2044&context=dissertations
- Talks about participation over the years
- Might have good references. Check for references.

### Why Voting Matters: Large disparities in turnout benefit the donor class
- **Link**: https://www.demos.org/sites/default/files/publications/Why%20Voting%20Matters_0.pdf
- Talks about turnout
- Some nice graphs about comparing turnout by race and inciome
- Might have "ok" references

### Analysis of county-level voting patterns in the 2016 Presidential elections
- Type: Blog post
- **Link**: https://www4.stat.ncsu.edu/~reich/ABA/code/election2016data
- Nice analysis of previous elections
- Check for EDA and for important columns

### How to Create State and County Maps Easily in R
- Type: Blog post
- **Link**: https://medium.com/@urban_institute/how-to-create-state-and-county-maps-easily-in-r-577d29300bb2

### Do we vote in our own best interest?
- Type: Kaggle
- **Link**: https://www.kaggle.com/kirandipta/do-we-vote-in-our-own-best-interest
- Clustering of vars for presidential data
- Uses PCA for analysis

### I love the poorly educated
- Type: Kaggle
- **Link**: https://www.kaggle.com/mpoegel/i-love-the-poorly-educated?scriptVersionId=171731
- Quick analysis regression about fraction of voters of trump & education

### Improve Your Model Performance using Cross Validation (in Python and R)
- **Link**: https://www.analyticsvidhya.com/blog/2018/05/improve-model-performance-cross-validation-in-python-r/
- Quick blog post about cross validation
- Check!

### Logistic Regression in R Tutorial
- Type: Tutorial
- **Link**: https://www.datacamp.com/community/tutorials/logistic-regression-R
- Really nice explanation about logistic regression
- TODO: Check for code examples

### Glmnet Vignette
- **Link**: https://web.stanford.edu/~hastie/glmnet/glmnet_alpha.html
- Really nice blog post about glmnet and the logistic penalized regression

### Comparison of classification methods for the homes data
- Type: Blog post
- **Link**: https://www4.stat.ncsu.edu/~reich/BigData/code/glmnet.html
- R exmaples for doing classifications
- K nearest neighbors
- Discriminant analysis
- Comparison of methods using Brier scores, classification accuracy
- Nice to check for learning about classification methods

### Chapter 6 Logistic Regression
- **Link**: https://bookdown.org/roback/bookdown-bysh/ch-logreg.html
- Book about logistic regression in R
- Has formulas
- 6.5 Case Study: Reconstructing Alabama
- 6.5.4 Tests for significance of model coefficients
- 6.5.5 Confidence intervals for model coefficients
- 6.5.6 Testing for goodness of fit
- 6.5.7 Residuals for Binomial Regression
- CHECK!!!

### Penalized Logistic Regression Essentials in R: Ridge, Lasso and Elastic Net
- **Link**: http://www.sthda.com/english/articles/36-classification-methods-essentials/149-penalized-logistic-regression-essentials-in-r-ridge-lasso-and-elastic-net/
- R code with lasso regression



### Evaluating Logistic Regression Models
- **Link**: https://www.r-bloggers.com/evaluating-logistic-regression-models/
- Goodness of Fit
- Likelihood Ratio Test
- Hosmer-Lemeshow Test
- Statistical Tests for Individual Predictors
- Wald Test
- Variable Importance
- Validation of Predicted Values
- Classification Rate
- K-Fold Cross Validation
- TODO: Check

### Systemization of Logistic Regression Analysis for Pharmacometric Applications
- **Link**: https://www.researchgate.net/publication/292785254_Systemization_of_Logistic_Regression_Analysis_for_Pharmacometric_Applications
- Explanation about multicollinearity and ridge regression - for SAS - check again

- **Link**: https://rcompanion.org/handbook/G_14.html
- Accuracy and Errors for Models
- Not sure if it's only for regression or also logistic

- **Link**: https://cran.r-project.org/web/packages/MLmetrics/MLmetrics.pdf
- Package "MLmetrics"
- R functions RMSPE etc

### Multicollinearity: What Is It, Why Should We Care, and How Can It Be Controlled?
- **Link**: https://pdfs.semanticscholar.org/5a89/1e686fedc226942727b76ba568602ac94006.pdf?_ga=2.208120274.1742245575.1562854939-375471410.1562854939
- SAS article - may not be good reference but content is good

```
Collinearity is especially problematic when a model's purpose is explanation rather than prediction. In the case of explanation, it is more difficult for a model containing collinear variables to achieve significance of the different parameters. In the case of prediction, if the estimates end up being statistically significant, they are still only as reliable as any other variable in the model, and if they are not significant, then the sum of the coefficient is likely to be reliable. In summary if collinearity is found in a model testing prediction, then one need only increase the sample size of the model. However, if collinearity is found in a model seeking to explain, then more intense measures are needed. The primary concern resulting from multicollinearity is that as the degree of collinearity increases, the regression model estimates of the coefficients become unstable and the standard errors for the coefficients become wildly inflated. 

Checking collinearity through tolerance (nothing below 0.1) and VIF
```

https://onlinelibrary.wiley.com/doi/pdf/10.1002/for.3980030206
Collinearity and Forecasting

http://www.sthda.com/english/wiki/print.php?id=237
- Need to check for clustering variables

---------------------

## Code Examples

### Cross-Validation for Predictive Analytics Using R
- **Link**: http://www.milanor.net/blog/cross-validation-for-predictive-analytics-using-r/
- MUST try code for learning cross-validation (DONE)

### K-Means Clustering in R Tutorial
**Link**: https://www.datacamp.com/community/tutorials/k-means-clustering-r
- MUST try code in case centroid is used

### Tutorial 10.5a - Logistic regression and proportional and percentage data
**Link**: http://www.flutterbys.com.au/stats/tut/tut10.5a.html
Really nice tutorial for logistic regression
- MUST try code

---------------------------------------------

## Books

**Link**: https://onlinelibrary-wiley-com.ezproxy.lib.gla.ac.uk/doi/pdf/10.1002/9781118548387
Applied Logistic Regression
- Pending to check

---------------------------------------------

## Articles

### Classification of gene microarrays by penalized logistic regression
**Link**: https://academic.oup.com/biostatistics/article/5/3/427/310192
- Penalized logistic regression (PLR) vs SVM for classification

### Collinearity: a review of methods to deal with it and a simulation study evaluating their performance
- **Note**: Cited by: 1771  
- **Link**: https://onlinelibrary.wiley.com/doi/full/10.1111/j.1600-0587.2012.07348.x  
- **Link**: http://www.ecography.org/sites/ecography.org/files/appendix/e7348.pdf 

Notes:  
- Prob not good resource for logistic
- Part I. When is collinearity a problem? 
- Part II. Spatio-temporal patterns in collinearity
- Part III. Methods for dealing with collinearity 
    - Detect it: diagnostics 
    - Removing collinearity prior to analysis 
- Before analysis
    - Clustering, PCA, K-means clustering
        - Central variable from cluster (biased)
        - Best regressor -> increase of Type 1 errors
    - Cluster-independent methods
        - Select variables correlated |r| < 0.7
        - Sequential regression
- Modelling with latent variables
    - PCR (principal components regression)
    - DR (Dimension reduction)

TODO: 
- Check

```
Note: Could be used as a reference to say that for prediction collinearity is not an issue

1) When the correct form of the functional relationship is known, collinearity does not harm the fitting and therefore prediction to changing collinearity structures. 

Under very high collinearity, penalised methods are somewhat more robust, but here the issue of changes in collinearity structure also becomes graver. For predictions, our results indicate sensitivity to the way predictors correlate: small changes will affect predictions only moderately, but substantial changes lead to a dramatic loss of prediction accuracy.

There are some situations in which the effects of collinearity have limited impact. If the main use of the model is to predict new cases within the range of the sampled data (i.e. to interpolate), the model will do this reliably as long as the collinearity between variables remains constant (Harrell 2001).

Harrell, F. E. Jr 2001. Regression modeling strategies  -  with appli-cations to linear models, logistic regression, and survival analysis. -  Springer. 
https://link-springer-com.ezproxy.lib.gla.ac.uk/content/pdf/10.1007%2F978-3-319-19425-7.pdf
 
Collinearity does not affect predictions made on the same dataset used to estimate the model parameters or on new data that have the same degree of collinearity as the original data [470, pp. 379-381] as long as extreme extrapolation is not attempted. Consider as two predictors the total and LDL cholesterols that are highly correlated. If predictions are made at the same combinations of total and LDL cholesterol that occurred in the training data,
no problem will arise. However, if one makes a prediction at an inconsistent combination of these two variables, the predictions may be inaccurate and have high standard errors.

Classical and Modern Regression with Applications 
https://lib.ugent.be/en/catalog/rug01:000851135
```

### Comparison of Machine-Learning Techniques for Handling Multicollinearity in Big Data Analytics and High Performance Data Mining
- **Link**: https://www.researchgate.net/publication/284190532_Comparison_of_Machine-Learning_Techniques_for_Handling_Multicollinearity_in_Big_Data_Analytics_and_High_Performance_Data_Mining
- **Link**: https://pdfs.semanticscholar.org/f15a/d84de4cecb278dab7e7d7b7e149dcee6861d.pdf

Notes:
- Quick comparison between logistic with multicollinear methods


### Robust and sparse estimation methods for high dimensional linear and logistic regression
- **Link**: https://arxiv.org/pdf/1703.04951.pdf

Notes: 
- Nice article about lasso, ridge & elastic net
- Good resource to link to
- Has resources about robust logistic regression
- Friedman et al. [13] suggested to minimize a penalized negative log-likelihood function
- Comparison between elastic net, enet-LTS raw and enet-LTS

### Retrospective Forecasts Of The 2016 U.S. Primary Elections
- **Link**: http://www.scriptiesonline.uba.uva.nl/document/658181


### A New Robust Regression Model for Proportions
- **Link**: https://projecteuclid.org/download/pdfview_1/euclid.ba/1354024464
Notes: 
- Beta regression model
- Check after done with logistic

### Regularization and variable selection via the elastic net
- **Link**: https://web.stanford.edu/~hastie/Papers/B67.2%20(2005)%20301-320%20Zou%20&%20Hastie.pdf
Notes: 
- Seems a very good resource about elastic net
- Something to check when implementing, however, seems not to talk much about logistic regression

### Lecture notes on ridge regression
- **Link**: https://arxiv.org/abs/1509.09169
- This is like a book on lasso/ridge etc
- Something worth checking is: Penalty parameter selection
- Meijer and Goeman (2013) describe a computationally efficient approximation of the leave-one-out crossvalidated loglikelihood

09-ShortCourseBetaRegression.pdf

10-variable-selection-in-logistic-regression.pdf
### Variable Selection of Correlated Predictors in Logistic Regression: Investigating the Diet-Heart Hypothesis
Electronic Theses, Treatises and Dissertations The Graduate School
- Pending to read

11-classificaiton-techniques.pdf
### Supervised Machine Learning: A Review of Classification Techniques
- Really nice resource about classification techniques
- There's a nice table at the end about comparison on classification techniques

### Logistic Regression: A Brief Primer
- **Link**: https://www.ncbi.nlm.nih.gov/pubmed/21996075

### The practical value of logistic regression
- **Link**: https://pdfs.semanticscholar.org/2f1a/c6401855dd86aacd47d8eb85b1c6b3f21615.pdf
- Very old paper
- Seems to use SAS

14-logistic-example.pdf
### Comparison between SVM and Logistic Regression: Which One is Better to Discriminate?
- **Link**: http://www.kurims.kyoto-u.ac.jp/EMIS/journals/RCE/V35/v35n2a03.pdf

- https://rstudio-pubs-static.s3.amazonaws.com/431668_94857a2237f24827b3fad12d6d9d851c.html
- http://blog.elliotmarsden.com/us_election_2016.html
- https://brilliantmaps.com/2016-county-election-map/

--------------------------

## Resources
- **Link**: http://www.chrisbilder.com/categorical/Chapter5/AllGOFTests.R
- Functions from class
- **Link**: http://docs.h2o.ai/h2o/latest-stable/h2o-docs/data-science/algo-params/remove_collinear_columns.html
- remove_collinear_columns
- **Link**: http://www.medicine.mcgill.ca/epidemiology/joseph/courses/EPIB-621/logselect.pdf
Model Selection in Logistic Regression


# Notes
- Two votes for every state because each has two senators and 1 vote for each congress person 
- Smallest states have at minimum 3 electoral college votes
- This allocation gives disproportionate weight to smaller rural states with low populations
- That also, more often then note, are VERY white demographically
- white folk are on average older then total population and markedly wealthier than other groups. 
- Both attributes also make them far more reliable voters