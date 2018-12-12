# EDAV_final_shiny
R Shiny app for interactive visualization, EDAV final project

# Link:

For our interactive component, we created a Shiny app, which can be found at: https://ojdajuiceman.shinyapps.io/test_app/.


### Instructions for use:

1. For the first figure, data is displayed only for the selected year. 
2. For the second figure, data is displayed for all years including the selected year through present day. For example, for the second plot, selecting 1980 will produce an average of all years between 1980 and 2016, inclusive.
3. Both figures respond to the "Agencies" selector in the same manner.


### Future direction for visualization:

Performance is actually a serious issue for this plot. If the bars adjusted in real time as the slider moved, I think the plot would be significantly more insightful. I believe a ground-up implementation using D3 could achieve this.

One limitation of this plot is that the user cannot flexibly select date ranges for the second plot. This would be more illustrative than the current format, where users can only modify the lower limit of a date range; the upper limit is fixed at 2016. This proposed feature would allow users to zoom in on adjacent pairs of administrations (e.g. Clinton/Bush or Bush/Obama). 
