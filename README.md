# SIPP_Unit_Level

To run the example code, run the following steps

mkdir data/
wget https://www2.census.gov/programs-surveys/sipp/data/datasets/2022/pu2022.csv.gz data/ #get 2022 data
wget https://www2.census.gov/programs-surveys/sipp/data/datasets/2022/rw2022.csv.gz data/ #get replicate weights
gunzip data/*gz
wget https://www2.census.gov/programs-surveys/demo/tables/wealth/2021/wealth-asset-ownership/State_Wealth_tables_dy2021.xlsx data/ #Excel table to compare to

Rscript household_networth_repex.R
