## Article

### Introduction

In this article we provide unique insight into primary care prescribing patterns for care home patients aged 65 years and over in England from 2020/21 to 2023/24.

Patient address classification is based on experimental data linkage work. As such values are indicative. We welcome feedback, collaboration, and refinement of the methodology to see if it can be used in NHSBSA information systems in the future and how we can develop additional analyses.

### Key findings

The estimated number of care home patients aged 65 years and over who received prescribing increased each financial year. The number of prescription items and associated drug cost also increased.

We estimated a monthly average __of 323,000 care home patients aged 65 years__ and over receiving prescriptions in 2023/24. They received an estimated 40 million prescription items at a drug cost of £396 million during 2023/24.

As might be expected, __care home patients aged 65 years and over received a higher rate of prescribing__ than non-care home patients, including prescribing of drugs associated with a falls risk. Care home patients also received a different range of medicines to non-care home patients and were more likely to receive prescribing of Vitamin D and for pain relief. They received less prescribing of medicines associated with kidney injury compared with non-care home patients.

Almost two-thirds of care home patients aged 65 years and over who received prescriptions were female. Just over 4 in 10 were females aged 85 years and over. Monthly prescribing costs and volumes varied by age, gender, care home type and geography.

### Methodology

Prescribing estimates are based on a sophisticated methodology which includes linking primary care prescription address data to care home addresses in [AddressBase Plus](https://www.ordnancesurvey.co.uk/business-government/products/addressbase) and [CQC data](https://anypoint.mulesoft.com/exchange/portals/care-quality-commission-5/4d36bd23-127d-4acf-8903-ba292ea615d4/cqc-syndication-1/). The address matching method has been made available as an R package named [{addressMatchR}](https://github.com/nhsbsa-data-analytics/addressMatchR). In addition, the R package [{nhsbsaR}](https://github.com/nhsbsa-data-analytics/nhsbsaR) is used. Many of the functions within the {nhsbsaR} and {addressMatchR} packages are scripted to work specifically with an Oracle database. The source code behind these functions may have to be edited for different database architectures, for them to work correctly.

This analysis addresses a key gap in knowledge and gives valuable insights which can inform the use and management of medicines in care homes to help improve health outcomes, quality of care and ensure value. Points to note:

- Patient count estimates are of care home residents aged 65 years and over __receiving prescriptions__. A care home resident that received no prescriptions would not appear in this data, meaning care home patient count estimates will be lower than the monthly actual care home population. These monthly estimates will be higher than 2021 Census estimates which are based on occupancy on Census day.
- Values in this report are __indicative__. Since care home prescribing is identified through address matching, some non-care home prescriptions may be allocated to a care home and vice versa. Accuracy of identified care home matches is estimated to be 99.6%.
- Prescribing patterns will be impacted by the COVID-19 pandemic and beyond. The pandemic started in March 2020 and included a series of national lockdowns during 2020/21.
- There are numerous published sources of data on Adult Social Care home resident numbers that have differing coverage and scope. The data in this publication relate to residents receiving prescriptions and therefore will differ from other estimates. See the Annex for further sources of data available on the care home population.
- Drug costs are particularly influenced by prices which have been rising rapidly over the past year due to price concessions and possibly general inflation.

See the Address Matching page for further details or the methodology described in full on a [blog post on RPubs](https://rpubs.com/nhsbsa-data-analytics/methodology).

### Overall prescribing for care home patients aged 65 and over

__The number of patients aged 65 years and over who received prescription items in care homes increased each year, as did the number of prescription items and drug cost.__

There was an estimated monthly average of 323,000 care home patients aged 65 years and over receiving at least one prescription item in 2023/24. They received around 40 million prescription items at a drug cost of £396 million.

This accounts for around __6.3%__ of all items prescribed to patients aged 65 years and over and around __7.9%__ of the drug cost. This represents increases from 2020/21, where there was a monthly average of 289,000 patients, 35 million prescription items and a drug cost of £324 million.

The total number of care home patients who received at least one prescription item in 2023/24 was 496,000. The difference in the monthly average and annual estimates is explained by two key factors.

- The population is not stable – some patients turn 65 years old during the year, some move in or out of the care home and others may die.
- Not all care home patients receive a prescription in every month they are in a care home.

For this reason most Metrics were calculated on a patient-month basis.

The number of patients who received prescriptions in a care home peaked in January 2024, at 328,000 patients. January 2024 also had the highest number of care home prescription items, at 3.6 million items. Prior to 2024, patient counts peaked around the start of the COVID-19 pandemic.

Patient counts initially dropped during the pandemic then steadily rose again from March 2021. There may be some seasonality, with a declining care home population during and around winter months.
