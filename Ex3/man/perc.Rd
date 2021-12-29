\name{Functions of Exercise3}
\alias{Ex3}
\title{Functions of Exercise 3}
\description{
We have 3 functions: perc, regression and statistics.}
\usage{
perc(fcountry,decade)
regression(fcountry)
statistics(fcountry)}

\details{
perc function:  Calculate the percentage increase between one decade and the following decade (eg. between 1990-2000 and 2000-2010) for co2, methane and nitrous oxide together by each required country and decade, offer a graph per country required and the average of all years studied. Moreover it indicates what decades or countries are represented.

regression function: Calculate the projection of Co2, methane and nitrous oxide by country. Use a linear model to this computation (lm): For this, decade 1990-2000 has the value 1, 2000-2010 the value of 2 and so on, consecutively it can be transformed into a regression model. Moreover in the plot it is indicated what decades or countries are represented.

statistics function: It projects the future of Co2, methane and nitrous oxide and calculate statistics by country giving the following information: Will Increment their Co2?, Expected increase in million tonnes, Expected percentage of the increase and Standard deviation and R-squared value of the regression model.
}

\examples{
perc(c("China", "Spain"), c("1920-1930","1980-1990", "2000-2010", "2010-2020"))

regression("China")

statistics(c("Spain", "China", "France", "Andorra"))  }
