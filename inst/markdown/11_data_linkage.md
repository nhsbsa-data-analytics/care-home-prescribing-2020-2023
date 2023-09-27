## Data Linkage

Prescribing estimates are based on a sophisticated methodology which includes linking primary care prescription address data to care home addresses in [AddressBase Plus](https://www.ordnancesurvey.co.uk/business-government/products/addressbase) and [CQC data](https://anypoint.mulesoft.com/exchange/portals/care-quality-commission-5/4d36bd23-127d-4acf-8903-ba292ea615d4/cqc-syndication-1/). The address matching method has been made available as an R package named [<code>{<u>addressMatchR</u>}</code>](https://github.com/nhsbsa-data-analytics/addressMatchR). In addition, the R package [<code>{<u>nhsbsaR</u>}</code>](https://github.com/nhsbsa-data-analytics/nhsbsaR) is used. Many of the functions within the <code>{nhsbsaR}</code> and <code>{addressMatchR}</code> packages are scripted to work specifically with an Oracle database. The source code behind these functions may have to be edited for different database architectures, for them to work correctly.

A Single Line Address (SLA) data field was required for the first two matching stages. The <code>oracle_merge_strings</code> function from the <code>{nhsbsaR}</code> package aimed to generate a Single Line Address (SLA), by combining address components. This generated a SLA similar but not identical to that found in AB Core.

When SLA could not be exact matched, they were matched using the individual words (tokens) in the patient and lookup addresses. It was possible the same lookup token could be matched multiple times against different patient address tokens. In some instances, this may have exaggerated the similarity between two address strings. This resulted in 16.3 million forms being labelled through address matching.

Some address strings could not be matched between datasets. Additional processes were applied to match the remaining records to care homes. One method was to assign care home status based on the criteria:

*	address shares a postcode with an identified care home
*	five or more patients receive prescribing in a single month
*	keyword exclusions, such as hospitals.

This approach led to an additional 0.3 million prescription forms being labelled as belonging to a care home in 2022/23. The threshold of five patients was chosen to maximise the accuracy. It could change in future analyses.

A final stage looked at key words appearing in patient addresses, such as ‘care home’ and ‘residential home’. This resulted in an extra 0.5 million prescription forms being labelled as belonging to a care home. These keywords are described in the [full methodology](https://rpubs.com/nhsbsa-data-analytics/methodology) published on the RPubs website. The keywords could change in future analyses.

A comprehensive validation exercise estimated the overall accuracy of correctly classifying the patient address from a prescription for to a care home or otherwise at 99.6% for the 2020/21 data. With the methodology being consistent a similar level of accuracy can be expected for the 2021/22 and 2022/23 data. The validation only focused on false-positive matches. This was where a patient address record was incorrectly labelled as being a care home. No work was done to gauge the extent of false-negatives, namely patient address records that were care homes although not labelled as such. More information around validation accuracy estimates can be found within part 6 of the [full methodology](https://rpubs.com/nhsbsa-data-analytics/methodology).

Throughout the project the NHSBSA ensured data was protected and secure and adhered to the Caldicott Guardian principles.

The methodology is described in full on a [blog post on RPubs](https://rpubs.com/nhsbsa-data-analytics/methodology).
