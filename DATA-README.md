# Data Readme

This README explains how to make best use of the data from the Stanford Open Policing Project. We provide an overview of the data, and a list of best practices for working with the data. 

Our analysis code and further documentation are available at [https://github.com/5harad/openpolicing](https://github.com/5harad/openpolicing).

### Overview of the data file structure

For each state in the dataset, we provide data in three formats: 

1. The data converted to our standardized format, with a single CSV for each state. We describe the meaning of each column below. It will be easiest to start with the standardized format for most analyses. 
2. The raw data as originally received from the state. Raw data may be in a variety of formats — CSV, XLS, etc — and may contain multiple files. 
3. The raw data converted to CSV format, with no other processing. There may be multiple CSV files. 

The clean data are available for direct download from the Stanford Open Policing Project website; please contact us for access to the raw data.

### Description of standardized data

Each row in the standardized data for each state provides information for one state patrol stop. All standardized data files contain the following columns. If a column cannot be computed using the data a state has provided, it is set to NA. Some states also have additional columns (e.g., an ID for the officer making the stop), which we do not use in our analysis, but which we include here because they might be useful to other researchers. These extra columns are explained in the state notes. 

For several fields (e.g., driver_race) we include a "raw" column which records the original data values from which we infer standardized values. For example, driver_race_raw might be “White Hispanic” which we code as “Hispanic” in the standardized driver_race field. We include the raw columns because our data processing pipeline is extensive, requiring judgment calls and subjective decisions. We aim to make our data processing as transparent as possible. Other analysts may choose to process the raw data differently if their needs or judgments differ.

<table>
  <tr>
    <td>Column name</td>
    <td>Column meaning</td>
    <td>Example value</td>
  </tr>
  <tr>
    <td>id</td>
    <td>The unique ID we assign to each stop. Contains the state and year. </td>
    <td>VT-2011-00012</td>
  </tr>
  <tr>
    <td>state</td>
    <td>The two-letter code for the state in which the stop occurred.</td>
    <td>VT</td>
  </tr>
  <tr>
    <td>stop_date</td>
    <td>The date of the stop, in YYYY-MM-DD format. Some states do not provide the exact stop date: for example, they only provide the year or quarter in which the stop occurred. For these states, stop_date is set to the date at the beginning of the period: for example, January 1 if only year is provided.</td>
    <td>2011-11-27</td>
  </tr>
  <tr>
    <td>stop_time</td>
    <td>The 24-hour time of the stop, in HH:MM format.</td>
    <td>20:15</td>
  </tr>
  <tr>
    <td>location_raw</td>
    <td>The original data value from which we compute the county (or comparably granular location) in which the stop occurred. Not in a standardized format across states. </td>
    <td>Winooski
</td>
  </tr>
  <tr>
    <td>county_name</td>
    <td>The standardized name of the county in which the stop occurred.</td>
    <td>Chittenden County</td>
  </tr>
  <tr>
    <td>county_fips</td>
    <td>The standardized 5-digit FIPS code in which the stop occurred.</td>
    <td>50007</td>
  </tr>
  <tr>
    <td>district</td>
    <td>In several states (e.g., Illinois) the stop county cannot be inferred, but a comparably granular location can. This comparably granular location is stored in the district column. Most states do not have this column. </td>
    <td>ILLINOIS STATE POLICE 01</td>
  </tr>
  <tr>
    <td>fine_grained_location</td>
    <td>Any higher-resolution data about where the stop occurred: e.g., milepost or address. Not standardized across states. </td>
    <td>90400 I 89 N; EXIT 15 MM90/40</td>
  </tr>
  <tr>
    <td>police_department</td>
    <td>The police department or agency that made the stop. Not in a standard format across states.  </td>
    <td>WILLISTON VSP</td>
  </tr>
  <tr>
    <td>driver_gender</td>
    <td>The driver’s gender, as recorded by the trooper. M, F, or NA.</td>
    <td>M</td>
  </tr>
  <tr>
    <td>driver_age_raw</td>
    <td>The original data value from which we compute the driver’s age when they were stopped. May be age, birth year, or birth date. Not in a standard format across states.  </td>
    <td>1988</td>
  </tr>
  <tr>
    <td>driver_age</td>
    <td>The driver’s age when they were stopped. Set to NA if less than 15 or greater than or equal to 100. </td>
    <td>23</td>
  </tr>
  <tr>
    <td>driver_race_raw</td>
    <td>The original data value from which the driver’s standardized race is computed. Not in a standard format across states.</td>
    <td>African American</td>
  </tr>
  <tr>
    <td>driver_race</td>
    <td>The standardized driver race. Possible values are White, Black, Hispanic, Asian, Other, and NA, with NA denoting values which are unknown. Asian refers to Asian, Pacific Islander, and Indian. Native Americans/American Indians are included in the "other" category. Anyone with Hispanic ethnicity is classified as Hispanic, regardless of their recorded race.</td>
    <td>Black</td>
  </tr>
  <tr>
    <td>violation_raw</td>
    <td>The violation committed by the driver, in the language of the original data. Not in a standard format across states. Some stops have multiple violations. </td>
    <td>Speeding (10–19 MPH Over Prima Facie Limit *)</td>
  </tr>
  <tr>
    <td>violation</td>
    <td>The violation committed by the driver, standardized into categories which are consistent across states. </td>
    <td>Speeding</td>
  </tr>
  <tr>
    <td>search_conducted</td>
    <td>A TRUE/FALSE value indicating whether a search was performed. </td>
    <td>TRUE</td>
  </tr>
  <tr>
    <td>search_type_raw</td>
    <td>The justification for the search, in the language of the original data. NA if no search was performed. Not in a standard format across states. Some states have multiple justifications for a search. </td>
    <td>CONSENT SEARCH CONDUCTED
</td>
  </tr>
  <tr>
    <td>search_type</td>
    <td>The normalized justification for the search. Where possible, this is standardized into categories which are consistent across states. For example, if something is clearly a consent search, search_type is referred to as “Consent”. </td>
    <td>Consent</td>
  </tr>
  <tr>
    <td>contraband_found</td>
    <td>A TRUE/FALSE value indicating whether a search was performed and contraband was found. FALSE if no search was performed. </td>
    <td>TRUE</td>
  </tr>
  <tr>
    <td>stop_outcome</td>
    <td>The outcome of the stop. Many states have idiosyncratic outcomes — for example, “CHP 215” in California — so this column is not standardized across states. “Citation” and “Warning” are the values which occur most commonly across states. If the stop has multiple outcomes, the most severe outcome is used. For example, if a stop resulted in a citation and a warning, stop_outcome would be “Citation”. </td>
    <td>Citation</td>
  </tr>
  <tr>
    <td>is_arrested</td>
    <td>A TRUE/FALSE value indicating whether an arrest was made.</td>
    <td>TRUE</td>
  </tr>
</table>


### Best practices

We provide some lessons we’ve learned from working with this rich, but complicated data. 

1. Read over the state notes and the state processing code if you are going to focus on a particular state, so you’re aware of the judgment calls we made in processing the data. Taking a look at the original raw data is also wise (and may uncover additional fields of interest). 
2. Start with the cleaned data from a single small state to get a feel for the data. Rhode Island, Vermont, and Connecticut are all load quickly. 
3. Note that loading and analyzing every state simultaneously takes significant time and computing resources. One way to get around this is to compute aggregate statistics from each state. For example, you can compute search rates for each age, gender, and race group in each state, save those rates, and then quickly load them to compute national-level statistics broken down by age, race, and gender. 
4. Take care when making direct comparisons between states. For example, if one state has a far higher consent search rate than another state, that may reflect a difference in search recording policy across states, as opposed to an actual difference in consent search rates. 
5. Examine counts over time in each state: for example, total numbers of stops and searches by month or year. This will help you find years for which data is very sparse (which you may not want to include in analysis). 
6. Do not assume that all disparities are due to discrimination. For example, if young men are more likely to receive citations after being stopped for speeding, this might simply reflect the fact that they are driving faster.  
7. Do not assume the standardized data are absolutely clean. We discovered and corrected numerous errors in the original data, which were often very sparsely documented and changed from year to year, requiring us to make educated guesses. This messy nature of the original data makes it unlikely the cleaned data are perfectly correct. 
8. Do not read too much into very high stop, search, or other rates in locations with very small populations or numbers of stops. For example, if a county has only 100 stops of Hispanic drivers, estimates of search rates for Hispanic drivers will be very noisy and hit rates will be even noisier. Similarly, if a county with very few residents has a very large number of stops, it may be that the stops are not of county residents, making stop rate computations misleading. 

Our analysis only scratches the surface of what’s possible with these data. We’re excited to see what you come up with!

Below are notes on the data for each state. They are not intended to be a comprehensive description of all the data features in every state, since this would be prohibitively lengthy. Rather, they are brief observations we made while processing the data. We hope they will be useful to others. They are worth reading prior to performing detailed analysis of a state. 


### Texas

**Original format**: MDB

**Time period**: 2006–2015

**Columns with no data**:
- `police_department`
- `driver_age`
- `is_arrested`

**Data notes**:
- There is evidence that minority drivers are labeled as white in the data. For example, see [this](http://kxan.com/investigative-story/texas-troopers-ticketing-hispanics-motorists-as-white/) report from KXAN. We remapped the driver race field as provided using the 2000 surnames dataset released by the U.S. Census. See the processing script or paper for details.
- We asked whether there was a field which provided arrest data, but received no clarification. There is data on incident to arrest searches, but this does not necessarily identify all arrests. 
- Based on the provided data dictionary as well as clarification from DPS via email, we classify THP6 and TLE6 in `HA_TICKET_TYPE` as citations and HP3 as warnings.
- The data only records when citations and warnings were issued, but not arrests.

**Extra fields**:
- `officer_id`
- `lat`
- `lon`
- `driver_race_original`


