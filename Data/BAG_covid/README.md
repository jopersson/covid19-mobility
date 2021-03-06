### COVID-19 outcomes and testing

The data files "tests.csv" and "BAG_covid.csv" are used to obtain the merged modelling data.
They are generated from raw data by running the scripts "load_test.R" and "load_BAG.R" in the folder "Data_preprocessing".

The raw data are available [here](https://www.covid19.admin.ch/en/overview) and [here](https://www.bag.admin.ch/bag/en/home/krankheiten/ausbrueche-epidemien-pandemien/aktuelle-ausbrueche-epidemien/novel-cov/situation-schweiz-und-international.html#-640157857).

- The following data files can be downloaded from the first link and are used in script "load_BAG.R":
    - COVID19Cases_geoRegion.csv
    - COVID19Death_geoRegion.csv
    - COVID19Hosp_geoRegion.csv
    - COVID19Test_geoRegion_all.csv

    Note that the current date is contained in the name of the downloaded data folder. Hence, to run "load_BAG.R", one needs to update the name of the input folder.


- For the second link, click "Data on tests conducted (XLS, 22 kB, 09.04.2021)" to download the file "Dashboard_3_COVID19_labtests_positivity.xlsx". The script "load_test.R" uses this data to generate the preprocessed data file "test.csv".
