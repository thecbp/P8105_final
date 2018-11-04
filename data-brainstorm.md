Data Ideas
================
11/4/2018

Potential Data Source:
======================

Last updated: 11-4 by Christian

openFDA has data on adverse events on drugs from 2014 to the present day. It is updated quarterly and can be found [here](https://open.fda.gov/apis/drug/event). We get the data by requesting it from an API.

An example API call would go like:

<https://api.fda.gov/drug/event.json?search=receivedate>:\[20040101+TO+20081231\]&limit=3

Important data pieces:

-   Time of adverse event report
-   Some patient information (age, sex)
-   What the adverse reaction was (head pain, sepsis, etc)
-   Why was the drug administered (drug indication)
-   Manufacturer information
-   Severity information (hospitalization? death?)
-   Drug information (start date, end date, what kind of drug is it, route of administration)

Possible interesting questions:

-   What are the most prevalent adverse events by state?
-   What types of drugs are the most responsible for certain events?
-   What about super serious events?
-   Are there certain companies that have more adverse events than others?
-   Are there trends over time?
