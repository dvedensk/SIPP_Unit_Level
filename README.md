# SIPP_Unit_Level

To run the example code, run the following steps in a terminal (or download the files directly via the URLs)

mkdir data/

cd data

wget https://www2.census.gov/programs-surveys/sipp/data/datasets/2022/pu2022.csv.gz #get 2022 data

wget https://www2.census.gov/programs-surveys/sipp/data/datasets/2022/rw2022.csv.gz #get replicate weights

gunzip *gz

wget https://www2.census.gov/programs-surveys/demo/tables/wealth/2021/wealth-asset-ownership/State_Wealth_tables_dy2021.xlsx #Excel table to compare to

cd ..

Rscript household_networth_repex.R
