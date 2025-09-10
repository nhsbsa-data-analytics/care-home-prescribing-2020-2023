## Datasets

### Prescriptions data

Analysis is based on primary care prescription data collected by the NHS Business Services Authority. The data are collected for the operational purpose of reimbursing and remunerating dispensing contractors for the costs of supplying drugs and devices, along with essential and advanced services, to NHS patients. This excludes:

- prescriptions that were issued but not presented for dispensing
- prescriptions that were not submitted to the NHSBSA for processing and reimbursement
- prescriptions issued and dispensed in prisons, hospitals, and private prescriptions
- prescription batches submitted late

Prescription data relates to prescription batches submitted to the NHSBSA for payment between April 2020 and March £>paste0(substring(latest_figures$latest_fy, 1, 2), substring(latest_figures$latest_fy, 6, 7))<£. The part month in NHSBSA data relates to the dispensing month for which the prescription batch was submitted. This is generally but not always the month in which the prescription was dispensed. This means there may be dispensing for given patients that has not been submitted to the NHSBSA for payment and is therefore not included. There may also be prescriptions included for a patient that were dispensed prior to the dispensing month.

Patients may receive prescription items that have not been prescribed to them personally and will not be accounted for. This may occur in the case of high-volume vaccines such as flu vaccines, and in the case of bulk prescribing of products, which can be bought in a pharmacy or supermarket such as Lactulose syrup and small volumes of Paracetamol. There is no means of quantifying the extent of this in NHSBSA data.

The NHSBSA do not capture the clinical indication of a prescription and therefore do not know the reason why a prescription was issued, or the condition it is intended to treat. Many drugs have multiple uses, and although classified in the BNF by their primary therapeutic use may be issued to treat a condition outside of this.

Due to manual processes involved in the processing of prescriptions there may be inaccuracies in capturing prescription information which are then reflected in the data. NHS Prescription Services have a variety of validation streams throughout prescription processing to support accurate capture of the data. In addition, a retrospective sample is completed in the month following reimbursement to identify the accuracy of prescription processing information. The check includes the accuracy of prescriber, practice, and drug information, but does not include the personal details of the patient. The reported Prescription Processing Information Accuracy for the 12-month rolling period ending March 2023 was 99.9%. The sample may not be representative at a more granular level; as such the level of accuracy is undetermined for specific groups such as drugs, geographies, and time periods. It should also be noted that the identification of errors in the accuracy checking sample does not result in amendments to data held in NHSBSA systems. Further details of Prescription Processing Information Accuracy can be found on our [website](https://www.nhsbsa.nhs.uk/pharmacies-gp-practices-and-appliance-contractors/payments-and-pricing/how-we-process-prescriptions).

### Patient data

A single age was attributed to each patient for each financial year, to enable aggregations for multiple charts within the analysis. The maximum age recorded across all prescription forms for a patient, within each financial year, was taken as their age.

The analysis focuses on prescriptions for older patients aged 65 years and over at the time of prescribing. Patient age was determined using a mixture of patient information from prescription forms and the [Personal Demographics Service](https://digital.nhs.uk/services/demographics) (PDS). Further details on the process of [patient age determination (PDF format)](https://www.nhsbsa.nhs.uk/sites/default/files/2018-02/180115%20Age%20Logic%20Summary%20Flow%20Chart%20-%20Revised%20Layout.pdf) can be found on our website.

A single gender was attributed to each patient. The most recent gender recorded against a patient was taken as their gender. Patient gender was sourced from the PDS, which includes four categories: (1) Male, (2) Female, (3) Not known, (4) Not specified. Category (3) consists of patients where gender has not been recorded; category (4) consists of patients who could not be classified as either male or female.

Patient prescription forms were labelled as being from a care home or not based on address matching described in the [Methodology](https://rpubs.com/nhsbsa-data-analytics/methodology). Of 248 million prescription forms issued to patients aged 65 years and over in 2022/23, 17 million could be categorised as being from a care home.

[Ordnance Survey AddressBase](https://www.ordnancesurvey.co.uk/business-government/products/addressbase) (AB) is a product that is in a continual state of refinement. While the AB epochs used in the analysis were the closest to the end of each financial year, there would still be instances where the information in AB did not mirror actual patient address details at the time of prescribing.

A selection of AB building classification types was removed from the lookup data, such as street records, objects of interest, car parks, garages and others. Some incorrect matches may have occurred through not excluding other building classification types.

The analysis required that every prescription form had a patient address recorded. Addresses were available for all electronic prescriptions. Address information was not captured directly from paper prescriptions and therefore a process was derived to generate these addresses using a mix of information from the Personal Demographic Service (PDS) and electronic prescriptions across a range of months This is described in [Section 2.3 of the Methodology](https://rpubs.com/nhsbsa-data-analytics/methodology). Although accurate, this is not as robust as directly sourced patient address information from electronic prescriptions.

For the 2022/23 financial year, patient addresses could be allocated for 99.9% of paper prescription forms where the patient’s NHS number could be identified, and the patient was aged 65 years and over.

Prescription forms with known non-English patient address information were removed from the analysis. Records with an unknown or missing postcode were included.

The analysis only includes patients with an NHS number and date of birth verified by PDS.

NHS numbers are captured for 100% of electronic prescription messages. We estimate that NHS numbers are captured for 94.7% of paper prescriptions. Overall, the capture rate is over 99% of all prescriptions.

The NHSBSA periodically investigate the accuracy of NHS numbers captured from paper forms. The personal details captured (NHS number, date of birth and age) are compared against those on the prescription form for a random sample of 50,000 prescription forms. The NHS number captured typically matches that on the prescription form for over 99.9% of forms. The results represent the accuracy for all prescription items processed; as such the level of accuracy is undetermined for specific medicines, geographies, time periods and other factors. By contrast, the accuracy of captured NHS numbers in electronic prescribing is estimated to be 100%.

### Lookup address

Patient geography information was determined using the latest available [National Statistics Postcode Lookup](https://www.ons.gov.uk/methodology/geography/geographicalproducts/postcodeproducts) (NSPL) from the [ONS Open Geography portal](https://geoportal.statistics.gov.uk/). An NHS region, Integrated Care System (ICS) or Local Authority could not be attributed to a patient address record if they had an unknown postcode, or if their postcode was not contained within the NSPL. We use 2023 boundaries for all administrative geographies.

[Ordnance Survey AddressBase](https://www.ordnancesurvey.co.uk/business-government/products/addressbase) (AB) was the foundation of the lookup address data, which was matched against patient address information. AB is available in three formats, Core, Plus and Premium. This analysis used AB Plus.

AB Plus is a product that is in a continual state of refinement, with epochs released on a six-weekly schedule. For each of the three financial years, the extract with date closest to the end of the financial year was used.

[AB building classifications (PDF format)](https://www.ordnancesurvey.co.uk/documents/product-support/tech-spec/addressbase-technical-specification.pdf) were critical to matching a patient record to an address classified as being a care home. These classifications are maintained by Ordnance Survey based on information supplied by external agencies (e.g. Care Quality Commission) and rely on accurate information being supplied to Ordnance Survey. To remove potential mismatches a selection of AB building classification types was removed from the lookup data, such as street records, objects of interest, car parks, garages, and others. However, although this reduces the scope for mismatching there may be other building classification types that could also be excluded.

[Care Quality Commission](https://www.cqc.org.uk/) (CQC) data was used to supplement the AB address information. This increased the pool of care home addresses and resulted in more accurate identification of prescriptions for care home residents.

CQC information was sourced from the [CQC API](https://anypoint.mulesoft.com/exchange/portals/care-quality-commission-5/4d36bd23-127d-4acf-8903-ba292ea615d4/cqc-syndication-1/).

As with AB, CQC data is also in a state of [continual refinement](https://www.cqc.org.uk/what-we-do/how-we-use-information/how-we-use-information), due to the changing nature of property details, and as with AB, a snapshot of the CQC data taken at a point in time was used for the matching process.

CQC data also holds additional care home definitions, including showing a distinction between nursing and residential homes. The definitions could be extended to AB records where the CQC data included a Unique Property Reference Number, although this was not available for all CQC records.
