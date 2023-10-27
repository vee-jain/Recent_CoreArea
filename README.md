# Recent core area range and points

MoveApps

Github repository: (https://github.com/vee-jain/Recent_CoreArea-MoveApps)

## Description
This app generates a Kernel Density Estimate (KDE) at the 50% contour (i.e., core area) at user-defined time period prior to the last date in the dataset. The app visualizes the core areas on a map along with raw tracking data, and provides a summary (per hour and day) on the frequency of points within the core area.
This app replaces the deprecated 'Estimate weekly KDE ranges' App that was developed for the 'EMAC23: Equip MoveApps for Conservation'. 

## Documentation
For each weekly or monthly interval with more than 10 points, the KDE estimate is calculated at the 0.50 level. This app calls on the 'hr_kde' function in the 'amt' package to do so. It does not account for autocorrelation and users should be mindful of the sampling rates and outliers in their datasets. Some concepts are outlined below, however users should refer to Signer et al (2011) for more details.

**Kernel Density Estimation (KDE)**: *KDE is a non-parametric way to estimate the probability density function of a continuous random variable. It is often used in spatial analysis and data visualization to estimate the distribution of points in space or time.*

**Kernel Function**: *At the heart of KDE is the kernel function, typically a probability density function like the Gaussian (normal) distribution. This kernel is centered at each data point and is used to smooth the data. The shape of the kernel determines the smoothness of the estimated density.*

**Calculating the Estimate**: *For each point in the dataset, the kernel is centered at that point, and its contribution is spread around that point based on the kernel shape and bandwidth. The contributions from all data points are summed to estimate the density at any given point in space or time.*

### Input data
move2 location object

### Output data
move2 location object (same as input)

### Artefacts
The app creates the following artefacts:

`core_time_plots.pdf`: A PDF file containing the frequency of fixes within the core area by hour and date.
`map_core_plot.html`: HTML file for 0.50 KDE maps with the GPS fixes.
`map_html_files.zip`: A ZIP archive containing HTML map files.

### Settings 
`Days prior`: Integer value

### Most common errors
Please create an issue on the linked GitHub should any arise.

### Null or error handling
**Setting `Days prior`:** Default is 14 days
