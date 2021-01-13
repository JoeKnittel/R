# R

## Shiny App for Fitting a Distribution to a Discrete Random Variable

*This project was completed in my spare time as a fun coding experiment.*

Various phenomena, from an individual's number of doctor visits per year to the frequency of hurricane strikes in a given region, can be represented by <a href = "https://courses.lumenlearning.com/boundless-statistics/chapter/discrete-random-variables/">discrete random variables</a>. With some sample data, a distribution can be fit to such phenomena, and it can be used to make predictions about future events. 

The concept of discrete random variables plays a <a href = "https://openacttexts.github.io/Loss-Data-Analytics/C-Frequency-Modeling.html">crucial role in the realm of insurance</a>; namely, by way of claims frequency. Insurers seek to model claim frequency and severity in order to predict aggregate losses, which allows them to price premiums and perform loss reserving for the company. But **the process of fitting a distribution often requires writing a bit of code and analyzing a number of statistics and plots. What if the entire process could be automated such that a model could be fit be simply clicking a few buttons? That was the question I had in mind when conceptualizing this project**.

This project is coded in R, and it leverages the <a href = "https://shiny.rstudio.com/">Shiny</a> package, which brings the app to life. The whole process of modeling a fitting a distribution to a discrete random variable is reduced to uploading a data file (preferably CSV), selecting the structure of the file, clicking a button to estimate parameters, and compare selection criteria for a collection of model choices. The app even allows users to download their favorite model for easy usage within R.  


<img src = "https://raw.githubusercontent.com/JoeKnittel/R/main/Images/demo2.gif">

<hr>

**Live Demo**: <a href = "https://joe-knittel.shinyapps.io/shiny_app/" target = "_blank">https://joe-knittel.shinyapps.io/shiny_app/</a>

<hr>

### Contained in this repository:

- Code written in R
- Sample dataset used in demo
