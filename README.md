# Discrimination Reporting
## Employment Discrimination Reporting in California


### Update July 18, 2022

Two way of theorizing changes in Republican vote share:

Within operationalization:
1) compare the change in vote share within each census tract between 2012 and 2016
2) examine the distribution of the change in vote share among the census tracts
3) group the sample by sd (mean+-1sd, 1sd+, 1sd-)
4) or group the sample by simple changes (increase, decrease, constant)

Between operationalization:
1) examine the distribution of 2012 census tract level Republican vote share, group the sample by sd (mean+-1sd, 1sd+, 1sd-) or by simple majority
2) examine the distribution of 2016 census tract level Republican vote share, group the sample by sd (mean+-1sd, 1sd+, 1sd-) or by simple majority
3) compare group membership change

Either way, little variation, see figures.

Q&A:

> Simple majority in between estimation?

Answer: We included this, but in 2012 mean =0.32; sd = 0.16; In 2016, mean=0.26; sd = 0.15. See summary tables under the Political tab, too.

> Finding a "threshold" of change in Republican vote share?

Answer: It won't solve our problem, since the raw changes of increase (or decrease) did not show enough variation


### Hosting interactive descriptives for Meeting on May 23rd, 2022.

Features of this application:

1. Time series figures by bases/harms, record type, year range, and by count/percentage
2. Correlation matrices and co-occurence counts tables by bases/harms, record type, and year range 
3. Links to resources

## Flowchart of DFEH Employment Cases
![flowchart](https://github.com/yx1441/Discrimination_Reporting/blob/fe1aee5cf619c811acd7601256f6f1ed8b22bc51/flowchart.png)


## How to run
Running this application from Github:

```
runGitHub(repo = 'Discrimination_Reporting',username ='yao-mxu')
```      

Running this application from local:
```
shinyApp(ui = ui, server = server)
```  
Alternatively, hitting the "Run App" button in Rstudio or type in 
```
runApp("app.R")
```  

**Note: starting up the applicaiton may take around 10 minutes**

## Notes

Bases,
Clean Harms,
Time Series Plots,
Correlation Plots,
Table

[DFEH Official Website](https://www.dfeh.ca.gov).

Note: Removed 1 rows containing missing values (position_stack). occurs when calculating percentages for 2014/11 "Autocorrelation Plots", substantively fine

**Slow however we run this, inevitable**


Monday May 16th:

*Implemented reactive UI fields, observed events and adjusted*

~~1. Break down the co-occurence table by record type~~
~~2. Push to repo~~
~~3. Fix parts of about page~~

> Let's stay focused on what we discussed on Monday.







