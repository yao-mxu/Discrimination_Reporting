# Discrimination Reporting
## Employment Discrimination Reporting in California

Hosting interactive descriptives for Meeting on May 23rd, 2022.

Features of this application:

1. Time series figures by bases/harms, record type, year range, and by count/percentage
2. Correlation matrices and co-occurence counts tables by bases/harms, record type, and year range 
3. Links to resources

## Flowchart of Employment Complaints (aka Cases)
![flowchart](/assets/images/flowchart.png)


## How to run
Running this application from Github:

```
runGitHub(repo = 'Discrimination_Reporting',username ='yx1441')
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







