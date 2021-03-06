# Shiny-App-2

## Fitting a Distribution to a Discrete Random Variable (2021)

*Though this project was completed as a fun coding experiment, it's a tool with potential real-world actuarial utility.*

Various phenomena, from an individual's number of doctor visits per year to how often a given region is struck by a hurricane, can be represented by <a href = "https://courses.lumenlearning.com/boundless-statistics/chapter/discrete-random-variables/">discrete random variables</a>. With some sample data, a distribution can be fit to such phenomena, and it can be used to make predictions about future events.

The concept of discrete random variables plays a crucial role in the realm of insurance; namely, by way of claim frequency. Insurers seek to <a href = "https://openacttexts.github.io/Loss-Data-Analytics/C-Frequency-Modeling.html">model claim frequency</a> and severity in order to predict aggregate losses, which allows them to price premiums and perform loss reserving for the company. But **the process of fitting a distribution often requires writing a bunch of code and analyzing a number of statistics and plots. What if the entire process could be automated such that a model could be fit with just a few clicks? That was the question I had in mind when conceptualizing this project**.

This project is coded in R, and it leverages the <a href = "https://shiny.rstudio.com/">Shiny</a> package, which brings the app to life. The whole process of fitting a distribution to a discrete random variable is reduced to uploading a CSV file, selecting the structure of the file, clicking a button to estimate parameters, and comparing selection criteria for a collection of model choices. The app even allows users to download their favorite model to facilitate further analysis within R.  


<img src = "https://raw.githubusercontent.com/JoeKnittel/R/main/Images/app_demo.gif">

<hr>

### Live Link: <a href = "https://joe-knittel.shinyapps.io/fitting-a-discrete-distribution/" target = "_blank">https://joe-knittel.shinyapps.io/fitting-a-discrete-distribution</a>

<hr>

### Contained in this repository:

- Code written in R
- Sample dataset used in demo
