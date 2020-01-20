# Scrape of Municipality Council Members

Presented is a script to automatically identify the URLS of Dutch municipalities that host information on the members of the all 355 municipal councils.

## Purpose

To provide a complete and up-to-date register of all Dutch politicians, this repository fills in on the municipality level of Open States Foundation's project [Allmanak](https://www.allmanak.nl/).
The initial goal of this code was a generic scraper that would provide information on each council member. This goal was reached for municipalities that use iBabs or notubiz as their Content Management System (CMS). An identification of URLS that host information of council members is given for the other municipalities. 

## Method

The regular municpality websites are extracted from the government's [almanak](https://almanak.overheid.nl/Gemeenten/). The data is seperated in structured (CMS = iBabs | Notibuz) and unstructured websites (no popular CMS). Next, for iBabs and Notubiz the websites of council members are fetched and stored in ```gemeentes$ibabs_info``` and ```gemeentes$notubiz_info```.

For the unstructured websites, possible external URLS are tested on their existence for each municiality (```gemeenterad.municipality, gemeenteraadmunicipalty, raad.municipality, ris.municipality, bestuur.municipality, raadsinformatie.municipality, besluitvorming.municipality```). Then, by looping through each external and the internal website of each municipality, the URL of the council members is identified. If multiple URLs are found for one municipilaty, the URL with the highest structure count on "mailto" of all href nodes is used. The URLS per municipality are stored in ```gemeentes$use_url```. To actually be able to fetch the URLs, they are seperated in URLs that contain information on all members on the same site (```gemeentes$one_page = 1```), and URLs that only contain the links for information of each member (```gemeentes$one_page = 0```). For municipalities with more than one page, a first attempt was made to determine all council member URLs by computing a similarity between all links per municipality. The link with the highest mean of similarities can be used as a reference to indentify the council member URLs.

## Important Files

* scrape.R
    * contains the code
* municipalities-scrape.Rproj
    * the R project
* gemeentes.json
    * ouput file
    
## Author

[Nele Wensauer](https://github.com/nelewnsr)

## Copyright and License

The Scrape of Municipality Council Members is licensed under CC0.
