# HMG Meeting Cost Calculator

The *HMG Meeting Cost Calculator* is an unofficial calculator for estimating the cost of Civil Service meetings.

It uses published data from the [Civil Service Statistics 2020](https://www.gov.uk/government/statistics/civil-service-statistics-2020) on salary levels to estimate the hourly rate of UK civil servants by department and grade. It is purely for illustrative purposes and may not reflect the true costs of the meeting as other factors (e.g. length of service, profession, location) have a strong influence on individual salaries. Similarly variations in contractual arrangements (working hours, annual leave allowance) might result in an individual having longer or shorter working hours than has been assumed which will affect the calculation of the hourly rate from annual salary figures.

It is inspired by the Canadian [Meeting Cost Calculator](https://meetingcostcalculator.ca).

## Disclaimer
This is a personal project using published information and is not an official product of the Cabinet Office or HM Government.

## Estimating hourly rates
The fundamental unit in the calculations is an estimated hourly rate for civil servants based on their department/agency and their grade. The published statistics only provide annual salaries, therefore an estimate must be calculated. The estimate is calculated by assuming that the median salary is for a full-time employee and they work a 37 hour week and have 30 days annual leave plus 8 public holidays and 1 privilege day (1635.4 working hours per year).

This estimate is purely indicative and unlikely to be representative of any particular individual as several factors (e.g. length of service, profession,location, etc) will influence their salary, while contractual arrangements will mean they have longer or shorter annual working hours than the figure used for the calculation.

## Build
The calculator is built in [`R`](https://www.r-project.org) and [`{shiny}`](https://shiny.rstudio.com).

