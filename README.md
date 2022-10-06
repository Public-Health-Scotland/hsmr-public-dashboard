# Code to produce the [HSMR public dashboard](https://publichealthscotland.scot/publications/hospital-standardised-mortality-ratios/).

This is a summary level dashboard of the key points from the HSMR publication, available on 
the PHS wesbite. It presents the HSMR funnel plot and crude mortality trends for the acute 
hospitals in Scotland, as well as some additional mortality indicators.


### Data preparation

- The smr, crude trends and lookup data files required for this dashboard have been produced 
during the HSMR publication process. 
- save_app_files: script that includes all the data files used by the app and the right version 
(datestamped) of each one. The publication date should be updated and the script run 
to save the files locally in order to update the dashboard and deploy.

### shiny_app folder
Folder includes all the code used to produce the Shiny app. In the root folder, you can find 
the key scripts for the app (ui, server and global) and the google analytics tracking code.

- www folder: Includes images and logos used in the dashboard and the CSS stylesheet.
- lookups folder: contains the geography lookup used in the dashboard.
- Other folders: When working with the app other folder should be present: data and admin. 
These won't be commited to Git. Another folder called rsconnect could be present if you have 
previously deployed the dashboard.

### open data
[Open data](https://www.opendata.nhs.scot/dataset/hospital-standardised-mortality-ratios) is 
produced as part of the HSMR publication process.
