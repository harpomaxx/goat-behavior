#!/bin/bash
echo -e "\n\n############\n## 5f-1s  ##\n############"
dvc exp run -n "5f-1s-no-log-1d-loocvtune" --set-param=select_features_boruta.maxnfeat=5 --set-param=select_features_boruta.numsamp=1 --force 
echo -e "\n\n############\n## 10f-1s ##\n############"
dvc exp run -n "10f-1s-no-log-1d-loocvtune" --set-param=select_features_boruta.maxnfeat=10 --set-param=select_features_boruta.numsamp=1 --force 
echo -e "\n\n############\n## 15f-1s ##\n############"
dvc exp run -n "15f-1s-no-log-1d-loocvtune" --set-param=select_features_boruta.maxnfeat=15 --set-param=select_features_boruta.numsamp=1 --force 
echo -e "\n\n############\n## 20f-1s ##\n############"
dvc exp run -n "20f-1s-no-log-1d-loocvtune" --set-param=select_features_boruta.maxnfeat=20 --set-param=select_features_boruta.numsamp=1 --force 
echo -e "\n\n############\n## 25f-1s ##\n############"
dvc exp run -n "25f-1s-no-log-1d-loocvtune" --set-param=select_features_boruta.maxnfeat=25 --set-param=select_features_boruta.numsamp=1 --force 
echo -e "\n\n############\n## 30f-1s ##\n############"
dvc exp run -n "30f-1s-no-log-1d-loocvtune" --set-param=select_features_boruta.maxnfeat=30 --set-param=select_features_boruta.numsamp=1 --force 
echo -e "\n\n############\n## 25f-1s ##\n############"
dvc exp run -n "35f-1s-no-log-1d-loocvtune" --set-param=select_features_boruta.maxnfeat=35 --set-param=select_features_boruta.numsamp=1 --force 

