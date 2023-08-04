---
title: Something...
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE)
```

## Prescribing per patient-month (PPM)

The notion of ‘patient-months’ is used in all prescribing metrics. The _Patient-months example_ table displays the total number of prescription items and unique medicines prescribed per patient per month within a given financial year:

_Patient-months example_

```{r patient_months, results="asis"}

pm_table <- tibble::tribble(
  ~Patient,       ~Month,     ~`Prescription items`, ~`Unique medicines`,
  #---------------|-----------|--------------------|-----------------
  "A",            "April",      4,                  2,
  "A",            "May",        4,                  2,
  "A",            "June",       4,                  4,
  "A",            "July",       6,                  3,
  "A",            "September",  6,                  3,
  "A",            "November",   6,                  5,
  "B",            "April",      13,                 10,
  "B",            "May",        13,                 10,
  "B",            "August",     10,                 8,
  "C",            "October",    4,                  4
)

pm_table %>% kableExtra::kbl() %>% 
    kableExtra::kable_styling(
      bootstrap_options = c("striped", "hover", "condensed", "responsive")
    )
```

_Patient-months example_
| Patient       | Month     | Prescription items | Unique medicines |
| :------------ | :-------: | -----------------: | ---------------: |
| A             | April     | 4                  | 2                |
| A             | May       | 4                  | 2                |
| A             | June      | 4                  | 4                |
| A             | July      | 6                  | 3                |
| A             | September | 6                  | 3                |
| A             | November  | 6                  | 5                |
| B             | April     | 13                 | 10               |
| B             | May       | 13                 | 10               |
| B             | August    | 10                 | 8                |
| C             | October   | 4                  | 4                |

There are 10 patient-months (from 3 patients) in this sample. Over the 10 patient-months, 70 items were prescribed. The mean number of items prescribed per patient-month (PPM) was therefore 70 / 10 = 7 items. This metric uses the sum of all items as the numerator and the total number of patient-months as the denominator.

There are 10 patient-months in this sample. Over the 10 patient-months, 70 items were prescribed. The mean number of items prescribed per patient-month (PPM) was therefore 70 / 10 = 7 items. This metric uses the sum of all items as the numerator and the total number of patient-months as the denominator.

Out of the 10 patient-months, only two of them had 10 or more unique medicines prescribed. Therefore, the percentage of patient-months with 10 or more unique medicines prescribed is 2 / 10 = 20%. This metric uses a count of patient-months satisfying a condition as a numerator and the total number of patient-months as the denominator.

This analysis uses nine key prescribing metrics. Four of these calculate the mean value across patient-months, like the first example. Five of these see how many patient-months satisfy a condition out of all (or a selection of) patient-months, like the second example. 

All prescribing metric definitions are described in the ‘Analysis Metrics’ table.  Two metrics relate to cost and volume. Five polypharmacy metrics are based on metrics developed nationally and used in NHSBSA [ePACT2 polypharmacy dashboard](https://www.nhsbsa.nhs.uk/access-our-data-products/epact2/dashboards-and-specifications/medicines-optimisation-polypharmacy). Two of the metrics were generated from [research around medicines](https://www.nice.org.uk/guidance/cg161) associated with falls risk in elderly people.  The required additional information to fully understand these metrics is presented below.

The PPM metrics were calculated only using data from months where a patient received prescribing. If a patient was a care home resident for a given month yet received no prescribing, this month would not contribute towards PPM calculations. A care home resident with periodic repeat prescriptions could potentially have a drug allocation covering more than one month. Prescribing would only be allocated to the dispensing month.  

## Polypharmacy metrics

### Patients prescribed 6+ or 10+ unique medicines 

The percentage of patients prescribed 6+ or 10+ unique medicines are patient-month variations of metrics from the NHSBSA ePACT2 Polypharmacy dashboard. These metric calculations only consider prescription items from BNF chapters 1-4 and 6-10. The [rationale (PDF format)](https://www.nhsbsa.nhs.uk/sites/default/files/2018-02/PolyPharmacy%20Specification%20v1%200%20July%202017_0.pdf) for the selection of BNF chapters is that the comparators are intended to help practices to focus on mostly orally taken medicines, prescribed for long term conditions. This can then exclude, for example, incidental prescribing related to infections. In general, these are the medicines that have been found in studies to increase the risks associated with taking multiple medicines.

### Anticholinergic burden (ACB)

Anticholinergic medicines should be prescribed with caution as elderly patients are more likely to experience side effects such as constipation, urinary retention, dry mouth/eyes, sedation, delirium, falls and reduced cognition (which may be wrongly diagnosed as dementia).

This analysis uses a patient-month version of the NHSBSA [ePACT2 Polypharmacy dashboard ACB metric](https://view.officeapps.live.com/op/view.aspx?src=https%3A%2F%2Fwww.nhsbsa.nhs.uk%2Fsites%2Fdefault%2Ffiles%2F2021-02%2FACB1_specification_MH_v1.1_Feb2021.docx&wdOrigin=BROWSELINK). Rather than generating an ACB score, the percentage of patient-months with anticholinergic prescribing, in which at least 2 medicines of moderate to high anticholinergic burden were prescribed has been calculated.

### Medicines likely to cause kidney damage (DAMN)

Patients in the community with chronic kidney disease and patients with normal renal function who are treated with an angiotensin converting enzyme inhibitor (ACEi) or angiotensin receptor blocker (ARB) are at increased risk of acute kidney injury (AKI) if they develop an illness associated with hypovolaemia and hypotension. The temporary cessation of certain medications may induce, exacerbate, and complicate AKI. These drugs can be remembered by the mnemonic DAMN (diuretics, ACEi/ ARBs, metformin, NSAIDs).

This analysis uses a patient-month version of the NHSBSA [ePACT2 Polypharmacy dashboard DAMN metric](https://www.nhsbsa.nhs.uk/sites/default/files/2018-02/PolyPharmacy%20Specification%20v1%200%20July%202017_0.pdf), which is the percentage of patients prescribed two or more unique medicines during a single month that are likely to induce, exacerbate or complicate AKI (DAMN medicines), out of the months where patients was prescribed at least one such drug.

## Fall metrics

### Medicines associated with falls risk in elderly patients

Falls and falls-related injuries are a common and serious problem for elderly people. Whilst there are many contributing factors, [certain medications (PDF format)](https://pubmed.ncbi.nlm.nih.gov/29396189/) are recognised as a major and modifiable risk factor for falls. The human cost of falling includes distress, pain, injury, loss of confidence, loss of independence and mortality. Financially, falls are estimated to cost the NHS more than £2.3 billion per year. Falling in elderly people, such as the care home population, [impact both quality of life and healthcare costs](https://www.nice.org.uk/guidance/cg161). 

To support clinicians in the management of falls and to facilitate the deprescribing process, a group of clinicians developed the [Screening Tool of Older Persons Prescriptions in older adults with high fall risk](https://academic.oup.com/ageing/article/50/4/1189/6043386) (STOPPFall).

This analysis uses the broad medicine groups outlined by STOPPFall to define a list of medicines associated with falls risk. The medicine groups were converted into a list of chemical substances, collating the chemical substances within several BNF sections, paragraphs and sub-paragraphs most closely aligned with the STOPPFall medicine groups. 

The list of chemical substances within the broad STOPPFall drug groups was validated for associated falls risk by Heather Smith from West Yorkshire ICS and David Alldred from Leeds University. A handful of these were considered exceptions and removed. For example, antihistamines are a STOPPFall drug group yet not every single antihistamine chemical substance may have a falls risk.

The end-result is a list of 304 combinations of BNF paragraphs and chemical substances (as a single chemical substance can appear in multiple paragraphs and some of which are not required). The formation of this list is summarised by the _Falls risk drug groups_ table, taking all the BNF paragraph-chemical substance combinations resulting from multiple BNF level filters.

_Falls risk drug groups_
| BNF Level           | Category  | Name |
| :----------------: | :-------: | ---: |
| Section            | Inclusion | Antidepressant drugs<br>Antiepileptic drugs<br>Diuretics<br>Hypertension and heart failure<br>Hypnotics and anxiolytics | | Paragraph          | Inclusion | Antipsychotic depot injections<br>Antipsychotic drugs<br>Opioid analgesics<br>Opioid dependence<br>Alpha-adrenoceptor blocking drugs<br>Antihistamines<br>Vasodilator antihypertensive drugs<br>Drugs for urinary frequency enuresis and incontinence<br>Nitrates |
| Sub-paragraph      | Inclusion | Benzodiazepines |
| Chemical substance | Exclusion | Paraldehyde<br>Mirabegron<br>Mannitol<br>Loratadine<br>Desloratadine<br>Bilastine<br>Minoxidil |

## Prescribing metrics

The _Prescribing metric definitions_ table describes the name of each metric as it appears in the analysis, together with the numerator and denominator when calculating each metric, and a longer description of each metric.

_Prescribing metric definitions_
| Metric name | Numerator | Denominator | Description |
| ----------- | --------- | ----------- | ----------- |
| Mean prescription items PPM  | Sum of prescription items | Total patient-months | Mean number of items prescribed per patient-month |
| Mean drug cost PPM | Sum of drug cost | Total patient-months | Mean cost of items prescribed per patient-month |
| Mean unique medicines PPM | Sum of patient-monthly unique medicines from BNF chapters 1-4, 6-10 | Total patient-months | Mean number of unique medicines from BNF Chapters 1-4, 6-10 prescribed per patient-month |
| % of patient-months with 6+ unique medicines | Count of patient-months with 6+ unique medicines from BNF chapters 1-4, 6-10 | Total patient-months | Percentage of patient-months in which at least 6 unique medicines from BNF Chapters 1-4, 6-10 were prescribed |
| % of patient-months with 10+ unique medicines | Count of patient-months with 10+ unique medicines from BNF chapters 1-4, 6-10 | Total patient-months | Percentage of patient-months in which at least 10 unique medicines from BNF Chapters 1-4, 6-10 were prescribed |
| % of patient-months with 2+ ACB medicines | Count of patient-months with 2+ unique moderate to high ACB medicines | Count of patient-months with 1+ Moderate to High ACB medicine | Percentage of patient-months with anticholinergic prescribing, in which at least 2 medicines of moderate to high anticholinergic burden were prescribed |
| % of patient-months with 2+ DAMN medicines | Count of patient-months with 2+ unique DAMN medicines | Count of patient-months with 1+ DAMN medicine | Percentage of patient-months with DAMN prescribing, in which at least 2 medicines likely to cause kidney injury were prescribed |
| Mean unique falls risk medicines PPM | Sum of patient-monthly unique falls risk medicines | Total patient-months | Mean number of unique medicines associated with falls risk prescribed per patient-month |
| % of patient-months with 3+ falls risk medicines | Count of patient-months with 3+ unique falls risk medicines | Total patient-months | Percentage of patient-months in which at least 3 unique medicines associated with falls risk were prescribed |
