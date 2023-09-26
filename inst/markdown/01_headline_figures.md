## Article

### Introduction

In this article we provide unique insight into primary care prescribing patterns for care home patients aged 65 years and over in England during 2020/21, 2021/22 and 2022/23.

Patient address classification is based on experimental data linkage work. We welcome feedback, collaboration, and refinement of the methodology to see if it can be used in NHSBSA information systems in the future and how we can develop additional analyses.

### Key Findings

The estimated number of care home patients aged 65 years and over who received prescribing increased each financial year, from 2020/21 to 2022/23. The number of prescription items and associated drug cost also increased.

We estimated a monthly average __of 301,000 care home patients aged 65 years__ and over receiving prescriptions in 2022/23. They received an estimated 37 million prescription items at drug cost of £361 million during 2022/23.

As might be expected, __care home patients aged 65 years and over received  a higher rate of prescribing__ than non-care home patients, including prescribing of drugs associated with a falls risk. Care home patients also received a different range of medicines to non-care home patients and were more likely to receive prescribing of Vitamin D and for pain relief. They received less prescribing of medicines associated with kidney injury compared with non-care home patients.

Almost two-thirds of care home patients aged 65 years and over who received prescriptions were female. Just over in 10 were females aged 85 years and over. Monthly prescribing costs and volumes varied by age, gender, care home type and geography.

### Data Linkage

Prescribing estimates are based on a sophisticated methodology which includes linking primary care prescription address data to care home addresses in [AddressBase Plus](https://www.ordnancesurvey.co.uk/business-government/products/addressbase) and [CQC data](https://anypoint.mulesoft.com/exchange/portals/care-quality-commission-5/4d36bd23-127d-4acf-8903-ba292ea615d4/cqc-syndication-1/). The address matching method has been made available as an R package named [<code>{<u>addressMatchR</u>}</code>](https://github.com/nhsbsa-data-analytics/addressMatchR). In addition, the R package [<code>{<u>nhsbsaR</u>}</code>](https://github.com/nhsbsa-data-analytics/nhsbsaR) is used. Many of the functions within the <code>{nhsbsaR}</code> and <code>{addressMatchR}</code> packages are scripted to work specifically with an Oracle database. The source code behind these functions may have to be edited for different database architectures, for them to work correctly.

This analysis addresses a key gap in knowledge and gives valuable insights which can inform the use and management of medicines in care homes to help improve health outcomes, quality of care and ensure value. Points to note:

- __Prescribing patterns will be impacted by the COVID-19 pandemic and beyond. The pandemic started in March 2020 and included a series of national lockdowns during 2020/21.__
- There are numerous published sources of data on Adult Social Care home resident numbers that have differing coverage and scope. The data in this publication relate to residents receiving prescriptions and therefore will differ from other estimates. See the <a onclick="internalLink('Annex');">Annex</a> for further sources of data available on the care home population.
- Drug costs are particularly influenced by prices which have been rising rapidly over the past year due to price concessions and possibly general inflation.

See the <a onclick="internalLink('Data Linkage');">Data Linkage</a> page for further details or the  data linkage methodology described in full on a [blog post on RPubs](https://rpubs.com/nhsbsa-data-analytics/methodology).


### Overall prescribing for care home patients aged 65 and over

__The number of patients aged 65 years and over who received prescription items in care homes increased each year, as did the number of prescription items and drug cost.__

There was an estimated monthly average of 301,000 care home patients aged 65 years and over receiving at least one prescription item in 2022/23. They received around 37 million prescription items at a drug cost of £361 million.

This accounts for around __6.2%__ of all items prescribed to patients aged 65 years and over and around __7.8%__ of the drug cost. This represents increases from 2020/21, where there was a monthly average of 289,000 patients, 35 million prescription items and a drug cost of £324 million.

The total number of care home patients who received at least one prescription item in 2022/23 was 481,000. The difference in the monthly average and annual estimates is explained by two key factors:

* The population is not stable – some patients turn 65 years old during the year, some move in or out of the care home and others may die;
* Not all care home patients receive a prescription in every month they are in a care home.

For this reason <a onclick="internalLink('Metrics', 'prescribing-metrics');">Metrics</a> were calculated on a <a onclick="internalLink('Metrics', 'prescribing-per-patient-month-ppm');">patient-month</a> basis.

The number of patients who received prescriptions in a care home peaked in April 2020, at 314,000 patients which was at the start of the COVID-19 pandemic. April 2020 also had the highest number of care home prescription items, at 3.3 million items.
   
These numbers dropped during the pandemic then steadily rose again from March 2021. Since the last national lockdown, November 2022 had the greatest number of patients, prescription items and drug cost, before seeing a slight decline from December 2022 onwards. There may be some seasonality, with a declining care home population during and around winter months.
