#NOTES

#think about running random forest within days and then using that to develop a forecast


https://www.sciencedirect.com/science/article/pii/S2666498422000898

" For example, AT on the sample day (abbreviated as AT.0)
and average AT for the days before the sampling day (including 3, 7, 14, 30 days; AT.3,
AT.7, AT.14, AT.30). The same procedure was repeated to create the variable sets
of sunshine duration and wind speed. On the other hand, precipitation
variables were created as the precipitation on the sample day and
the total precipitation for the days before the sampling day. Therefore,
four variable sets, with five each, were created to measure meteorological
conditions (20 in total, Table S2)."

Use less nutrient data and more met data

#additional things to consider including:
#hydrological factors:
# Inflow discharge (log)	Inflow	m3 s−1	4.4–7.8	5.9 ± 0.6
# Outflow discharge (log)	Outflow	m3 s−1	1.0–7.3	6.0 ± 0.7
# Discharge difference (log)	Q.Diff	m3 s−1	−1.1–3.8	−0.1 ± 0.5
The flow discharge data were all on a logarithmic scale to reduce
the variance of raw data for facilitating modeling.

#meteorological
#sunshine duration (Sun.0, Sun.3, Sun. 7, Sun.14, Sun.30) the day of, 3 days prior, etc. 
#total precipitation (Precipitation.0, Precipitation.3, etc)
#wind speed
#update data to include 2024 data

#fix the way the nutrients are input (total nitrogen vs. total phosphorus)

categorical variables

#additional things to consider:
calculating variable importance based on mean decrease in node purity 

including interactive variables?
  minimal depth / conditional minimal depth

Standard variable importance has limits
Mean decrease in node purity (or accuracy) tells you how important each variable is on its own.
But it doesn’t explain how variables interact.
Example: Temperature might not seem super important by itself,
but if it always matters when nutrients are also high,
then the interaction is crucial — and standard importance scores won’t show that.

more to read: 
  (Cutler et al., 2007) - https://esajournals.onlinelibrary.wiley.com/doi/abs/10.1890/07-0539.1


calibration and validation (80/20 split)
then 10 years of cross validation

