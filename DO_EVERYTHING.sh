#!/bin/bash
tput setaf 1
echo "Beginning Analysis of All Data"
tput setaf 2
python ./overall_analysis.py
tput setaf 3
python ./all_inds.py
tput setaf 4
Rscript ./plots.R
tput setaf 5
Rscript q10_stuff.R
tput setaf 1
echo "analysis complete!"
tput setaf 2
