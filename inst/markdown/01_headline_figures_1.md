## Article

### Introduction

In this article we provide unique insight into NHS primary care prescribing patterns for care home patients aged 65 years and over in England from 2020/21 to £>latest_figures$latest_fy<£.

Insights are provided at a national and local level, showing trends over time and variation for key prescribing metrics. The insights can help to inform policies, planning and interventions on medicines use in care homes. They may be particularly useful to local authorities, ICSs, Chief Pharmacists and pharmacy teams and policy makers.

New additions for this year include a geographical breakdown of overall prescribing, a new anticoagulant prescribing metric and a comparison of prescribing metrics by length of stay. 

Patient address classification is based on experimental data linkage work. As such values are indicative. We welcome feedback, collaboration, and refinement of the methodology to continuously improve our methods and analysis. 

### Key findings

The estimated number of care home patients aged 65 years and over who received prescribing has increased since 2020/21. The number of prescription items and associated drug cost also increased. 

In £>latest_figures$latest_fy<£, we estimated a monthly average __of £>latest_figures$ch_monthly_pats<£ care home patients aged 65 years__ and over receiving prescriptions. They received an estimated £>latest_figures$ch_annual_items_m<£ million prescription items at a drug cost of ££>latest_figures$ch_annual_cost_m<£ million.

__Care home patients aged 65 years and over received a higher rate of prescribing__ than non-care home patients. This includes prescribing of drugs associated with a falls risk and anti-cholinergic burden. They received less prescribing of medicines associated with kidney injury.

Care home patients also received a different range of medicines to non-care home patients. They were more likely to receive Vitamin D and drugs for pain relief. Non-care home patients were more likely to receive Atorvastatin. The total drug cost for Paracetamol for care home patients has doubled in the last five years to an estimated £22.4 million. 

Almost two-thirds of care home patients aged 65 years and over who received prescriptions were female. Just over 4 in 10 were females aged 85 years and over.

Monthly prescribing costs and volumes varied by age, gender, care home type and geography. These insights will benefit from local interpretation.

### Methodology

Prescribing estimates are based on a sophisticated methodology which includes linking primary care prescription address data to care home addresses in [AddressBase Plus](https://www.ordnancesurvey.co.uk/business-government/products/addressbase) and [CQC data](https://anypoint.mulesoft.com/exchange/portals/care-quality-commission-5/4d36bd23-127d-4acf-8903-ba292ea615d4/cqc-syndication-1/). The address matching method has been made available as an R package named [{addressMatchR}](https://github.com/nhsbsa-data-analytics/addressMatchR).

See the [Address Matching](http://127.0.0.1/Address_Matching) page for further details or the methodology described in full on a [blog post on RPubs](https://rpubs.com/nhsbsa-data-analytics/methodology).

We continue to work closely with domain experts across the Department of Health and Social Care, NHS England and Community Pharmacy to validate our approach, assess user need, and understand how the insights are being used to inform decisions and drive impact.

This analysis addresses a key gap in knowledge and gives valuable insights to inform medicines use in care homes - improving health outcomes, quality of care and value. Key points to note:

- Patient count estimates are of care home residents aged 65 years and over __receiving prescriptions__. A care home resident that received no prescriptions would not appear in this data, meaning care home patient count estimates will be lower than the monthly actual care home population. These monthly estimates will be higher than 2021 Census estimates which are based on occupancy on Census day.
- Values in this report are __indicative__. 
- Since care home prescribing is identified through address matching, some non-care home prescriptions may be allocated to a care home and vice versa. Accuracy of identified care home matches is estimated to be 99.7%.
- Around 0.4% of prescription items for patients aged 65+ do not have a postcode, or have a non-English postcode, and are excluded from the analysis.
- Only partial fairness and bias testing is possible due to limited protected characteristics within the data. 
- Prescribing patterns are impacted by the COVID-19 pandemic and beyond. The pandemic started in March 2020 and included a series of national lockdowns during 2020/21.
- Geographical breakdowns will benefit from local insight and interpretation.
- There are numerous published sources of data on Adult Social Care home resident numbers that have differing coverage and scope. The data in this publication relate to residents receiving prescriptions and therefore will differ from other estimates. See the [Annex](http://127.0.0.1/Annex) for further sources of data available on the care home population.

See the [Datasets](http://127.0.0.1/Datasets) page for further information about the datasets used and any caveats.
