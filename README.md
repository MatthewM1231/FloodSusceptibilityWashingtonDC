# Flood Susceptibility and Socioeconomic & Health Vulnerability in Washington D.C.

Flooding is an ongoing problem for residents of Washington D.C. This is due to close proximity to the Anacostia and Potomac Rivers as well as the many low-lying areas with impermeable surfaces that are susceptibile to flash floods known as Blue Spots. In this analysis, we collected data from various sources such as the American Community Survey (ACS), OpenDataDC, CDC Places Data, and the USGS Digital Elevation Model (DEMS). Then we utilized this data to compare flood susceptibility to socioeconomic vulnerability and did a census-tract based analysis to compare the two vulnerabilities against one another. Regarding flood susceptibility, these were some of the variables of interest:


**Percent 100, 500, and Tidal Floodplain, Blue Spots**: Percentage of each census tract that falls within each of the FEMA defined floodplains

**Base Flood Elevation**: elevation of flooding expected during 1% annual chance event 

**Ground Elevation**: Calculates the average elevation over singular cell (a fixed square of 30 meters x 30 meters)

**Elevation Difference**: Ground Elevation - Base Elevation

**Distance to Water**: Euclidean distance to the nearest body of water


Additionally, our socioeconomic vulnerability includes economic, health, and infrastructure variables as follows:


Socioeconomic Variables:

**Poverty Rates**: % families that fall below the official poverty thresholds

**Minority Rates**: % population that does not identify as single-race non-Hispanic white

**Unemployment Rates**: % population that is of working age but not currently employed

**Percent Vulnerable Age Group**: % population above 65 or below the age of 14


Health Variables:

**Percent of Population Afflicted with Asthma**: Rate of asthma in a given census tract

**Percent of Population Afflicted with Diabetes**: Rate of diabetes in a given census tract


Infrastructure Variables:

**Access to Public Facilities & Emergency Services**: # of School Crossing Guards, Fire stations, Police Stations, & Hospitals

**Percent of Buildings within ~407.5 m to nearest body of water**

**Percent of Houses within a Census Tract built before 1980**


With these variables, I have conducted a principal component analysis and have used the results of this analysis to create a comprehensive dashboard using R-Shiny that allows the user to directly compare flood susceptibility, socioeconomic vulnerability, and infrastructure presence in Washington D.C. in the form of a map. This also allows the user to view the binned or raw indices produced by the principal component analysis in order to assess which census tracts in the district are the most/least vulnerable.

PIT-UN contributed funding for this work. PIT-UN is a project of the New Venture Fund (NVF), a 501(c)(3) public charity that supports innovative and effective public interest projects.
