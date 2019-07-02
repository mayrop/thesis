
# Statistical analysis of US presidential election data

## Overall project description
In the two recent US Elections, there have been much speculation on whether various socio- economic and demographic factors, such as race, age and income levels (to name a few) played a role in the preference for political parties or candidates. Presidential elections in the USA occur every four years, with registered voters casting their ballots on Election Day, which is the first Tuesday after November 1 that year. The modern political system in the U.S. is a two-party system dominated by the Democratic Party and the Republican Party. These two parties have won every United States presidential election since 1852, alternating on a fairly regular basis. In this set of projects, you will be asked to make use of presidential election data and demographic data from the US Census Bureau to analyse potential associations between socio-economic-demographic groups and electoral results in various states and counties in the USA.

## Assessing the impact of socio-economic factors on Presidential pri- mary election voting in the USA in 2016

### Data available
You are provided a data set on Presidential election results for each US county in 2016 `PresElect2016R.csv` and socio-economic data from the US Census Bureau (until 2014), in the file `UScounty-facts.csv`. An additional file, `UScounty-dictionary.csv`, is provided, which lists the detailed descriptions of variables available in the county facts file. For the purposes of this analysis, you may assume that there was an election involving only two parties in each county: Republican and Democratic. A brief description of the variables in the files are listed below:

**File 1**: `PresElect2016R.csv`  
* **Column 1**: state  
* **Column 2**: state.po (2-letter state abbreviation)  
* **Column 3**: county (county name)  
* **Column 4**: FIPS (unique ID for county from US Census records)  
* **Column 5**: candidatevotesR (number of votes cast for Republican presidential candi- date)  
* **Column 6**: totalvotes (total number of votes cast in the county)  
* **Column 7**: fracvotesR (fraction of total votes received by the Republican Presidential candidate)  
* **Column 8**: partywonR (binary variable that takes the value 1 if the Republican candidate won in that county; is otherwise zero)  

**File 2**: `UScounty-facts.csv`

The columns of this file correspond to measurements on several variables for each county, described in `UScounty-dictionary.csv`. Variables 1-18 correspond to demographic variables relating to the population and racial composition of counties. Variables 19 and 20 correspond to educational attainment; variable 21 to the number of war veterans in the county; variables 22-28 relate to housing; variables 29-42 to income and employment; variables 43-47 to sales; and variables 48-50 to building permits, land area and population per square mile, respectively.

## Question(s) of interest
The main questions of interest are twofold: first, are there any discernible associations between various socio-economic and other factors and the propensity of the county population to vote for a particular party? Second, can the relationship between various factors and primary election results by county be consolidated into a model that can forecast the actual 2016 presidential election results, by state? In particular, you may want to consider:
* Are there specific socio-economic or demographic factors that are associated with an increased or decreased preference for a political party, in a county?
* Is there an association between specific socio-economic or demographic factors and the fraction of people voting for a Republican Presidential candidate in a county?
* Are there state-wide factors that are associated with a preference for one political party over another?
* How well can your model associating socioeconomic factors with 2016 election results be used to predict the final state-wide outcome of the presidential elections in 2016? (For this question you might want to locate a data set listing the winning party in each state- this is available on numerous internet news sites, such as CNN.com or NPR.org; alternatively, you can consolidate data from within your existing data set.)

## Relevant courses
We strongly recommend that you have taken the following courses to undertake this project:
* Generalised linear models, Regression Models, Data Analysis, Multivariate methods or Machine Learning (main dissertation).
* Big Data Analytics, Machine Learning or Bayesian Statistics (advanced chapter).

## Getting started
Assessing the impact of socio-economic factors on Presidential election voting in the USA in 2016
The problem, to start with, is manipulating data sets with large numbers of variables and merging records across different data sets (electoral outcomes and socio-economic variables). It will be useful to keep in mind that certain counties listed in one file may or may not appear in the other; also there may be counties in which either electoral or census data may have to be discarded for some reason, for example, missingness in some variables. 

Groups of demographic/economic variables may be related by nature and must be handled carefully (e.g. you may not be able to use them simultaneously in a regression-type model). Some variables measured are on widely different scales of magnitude from others and care must be taken to ensure that any analysis undertaken is not affected by this. Preliminary exploratory analyses would be invaluable in determining which variables to look at more closely for the later statistical analysis. Note that you might want to use either the data on winning party, or proportion of votes won, as a response- they may not give identical results. Use of the maps package might be useful for visualisation of the distribution of variables across counties/states. 

For the advanced chapter, one option would be to source data from the earlier election (see, for example, US Presidential election data, 2012, from https://electionlab.mit.edu/data) and corresponding census data (http://www.census.gov) and determine whether socio- economic changes over the intervening period can explain the dierences in election results between the two years.

## Reading material
1. Presidential Election Process. https://www.usa.gov/election
2. A Comprehensive Survey of Clustering Algorithms. Xu, D. and Tian, Y. (2015) Annals of Data Science 2 (2), 165–193. https://link.springer.com/article/10.1007/s40745-015-0040-1
3. Supervised machine learning: A review of classification techniques. Kotsiantis, S. B. (2007) Informatica, 31, 249-268. http://www.informatica.si/ojs-2.4.3/index.php/informatica/article/view/148/140

## Dictionary
`PST045214` - Population, 2014 estimate  
`PST120214` - Population, percent change - April 1, 2010 to July 1, 2014  
`POP010210` - Population, 2010  
`AGE135214` - Persons under 5 years, percent, 2014  
`AGE295214` - Persons under 18 years, percent, 2014  
`AGE775214` - Persons 65 years and over, percent, 2014  
`SEX255214` - Female persons, percent, 2014  
`RHI125214` - White alone, percent, 2014  
`RHI225214` - Black or African American alone, percent, 2014  
`RHI325214` - American Indian and Alaska Native alone, percent, 2014  
`RHI425214` - Asian alone, percent, 2014  
`RHI525214` - Native Hawaiian and Other Pacific Islander alone, percent, 2014  
`RHI625214` - Two or More Races, percent, 2014  
`RHI725214` - Hispanic or Latino, percent, 2014  
`RHI825214` - White alone, not Hispanic or Latino, percent, 2014  
`POP715213` - Living in same house 1 year & over, percent, 2009-2013  
`POP645213` - Foreign born persons, percent, 2009-2013  
`POP815213` - Language other than English spoken at home, pct age 5+, 2009-2013  
`EDU635213` - High school graduate or higher, percent of persons age 25+, 2009-2013  
`EDU685213` - Bachelor's degree or higher, percent of persons age 25+, 2009-2013  
`VET605213` - Veterans, 2009-2013  
`LFE305213` - Mean travel time to work (minutes), workers age 16+, 2009-2013  
`HSG010214` - Housing units, 2014  
`HSG445213` - Homeownership rate, 2009-2013  
`HSG096213` - Housing units in multi-unit structures, percent, 2009-2013  
`HSG495213` - Median value of owner-occupied housing units, 2009-2013  
`HSD410213` - Households, 2009-2013  
`HSD310213` - Persons per household, 2009-2013  
`INC910213` - Per capita money income in past 12 months (2013 dollars), 2009-2013  
`INC110213` - Median household income, 2009-2013  
`PVY020213` - Persons below poverty level, percent, 2009-2013  
`BZA010213` - Private nonfarm establishments, 2013  
`BZA110213` - Private nonfarm employment,  2013  
`BZA115213` - Private nonfarm employment, percent change, 2012-2013  
`NES010213` - Nonemployer establishments, 2013  
`SBO001207` - Total number of firms, 2007  
`SBO315207` - Black-owned firms, percent, 2007  
`SBO115207` - American Indian- and Alaska Native-owned firms, percent, 2007  
`SBO215207` - Asian-owned firms, percent, 2007  
`SBO515207` - Native Hawaiian- and Other Pacific Islander-owned firms, percent, 2007  
`SBO415207` - Hispanic-owned firms, percent, 2007  
`SBO015207` - Women-owned firms, percent, 2007  
`MAN450207` - Manufacturers shipments, 2007 ($1,000)  
`WTN220207` - Merchant wholesaler sales, 2007 ($1,000)  
`RTN130207` - Retail sales, 2007 ($1,000)  
`RTN131207` - Retail sales per capita, 2007  
`AFN120207` - Accommodation and food services sales, 2007 ($1,000)  
`BPS030214` - Building permits, 2014  
`LND110210` - Land area in square miles, 2010  
`POP060210` - Population per square mile, 2010  
