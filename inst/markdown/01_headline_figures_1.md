## Article

### Introduction

In this article we provide unique insight into NHS primary care prescribing patterns for care home patients aged 65 years and over in England from 2020/21 to £>latest_figures$latest_fy<£.

Insights are provided at a national and local level, showing trends over time and variation for key prescribing metrics. The insights can help to inform policies, planning and interventions in relation to the use and management of medicines in care homes. They may be particularly useful to local authorities, ICSs, Chief Pharmacists and pharmacy teams and policy makers.

Patient address classification is based on experimental data linkage work. As such values are indicative. We welcome feedback, collaboration, and refinement of the methodology to see if it can be used in NHSBSA information systems in the future and how we can develop additional analyses.

### Key findings

The estimated number of care home patients aged 65 years and over who received prescribing increased each financial year. The number of prescription items and associated drug cost also increased.

In £>latest_figures$latest_fy<£, we estimated a monthly average __of £>latest_figures$ch_monthly_pats<£ care home patients aged 65 years__ and over receiving prescriptions. They received an estimated £>latest_figures$ch_annual_items_m<£ million prescription items at a drug cost of ££>latest_figures$ch_annual_cost_m<£ million.

As might be expected, __care home patients aged 65 years and over received a higher rate of prescribing__ than non-care home patients, including prescribing of drugs associated with a falls risk. Care home patients also received a different range of medicines to non-care home patients and were more likely to receive Vitamin D and drugs for pain relief. They received less prescribing of medicines associated with kidney injury compared with non-care home patients.

Almost two-thirds of care home patients aged 65 years and over who received prescriptions were female. Just over 4 in 10 were females aged 85 years and over. 

Monthly prescribing costs and volumes varied by age, gender, care home type and geography.

### Methodology

Prescribing estimates are based on a sophisticated methodology which includes linking primary care prescription address data to care home addresses in [AddressBase Plus](https://www.ordnancesurvey.co.uk/business-government/products/addressbase) and [CQC data](https://anypoint.mulesoft.com/exchange/portals/care-quality-commission-5/4d36bd23-127d-4acf-8903-ba292ea615d4/cqc-syndication-1/). The address matching method has been made available as an R package named [{addressMatchR}](https://github.com/nhsbsa-data-analytics/addressMatchR). In addition, the R package [{nhsbsaR}](https://github.com/nhsbsa-data-analytics/nhsbsaR) is used. Many of the functions within the {nhsbsaR} and {addressMatchR} packages are scripted to work specifically with an Oracle database. The source code behind these functions may have to be edited for different database architectures, for them to work correctly.

See the [Address Matching](http://127.0.0.1/Address_Matching) page for further details or the methodology described in full on a [blog post on RPubs](https://rpubs.com/nhsbsa-data-analytics/methodology).

This analysis addresses a key gap in knowledge and gives valuable insights which can inform the use and management of medicines in care homes to help improve health outcomes, quality of care and ensure value. Points to note:

- Patient count estimates are of care home residents aged 65 years and over __receiving prescriptions__. A care home resident that received no prescriptions would not appear in this data, meaning care home patient count estimates will be lower than the monthly actual care home population. These monthly estimates will be higher than 2021 Census estimates which are based on occupancy on Census day.
- Values in this report are __indicative__. Since care home prescribing is identified through address matching, some non-care home prescriptions may be allocated to a care home and vice versa. Accuracy of identified care home matches is estimated to be 99.7%.
- Prescribing patterns will be impacted by the COVID-19 pandemic and beyond. The pandemic started in March 2020 and included a series of national lockdowns during 2020/21.
- There are numerous published sources of data on Adult Social Care home resident numbers that have differing coverage and scope. The data in this publication relate to residents receiving prescriptions and therefore will differ from other estimates. See the [Annex](http://127.0.0.1/Annex) for further sources of data available on the care home population.

See the [Datasets](http://127.0.0.1/Datasets) page for further information about the datasets used and any caveats.
