#!/usr/bin/python

import os
import Tkinter, Tkconstants, tkFileDialog
import tkMessageBox
from os import listdir
import numpy as np
import csv
import datetime

# gets a timestamp to identify data files
timestamp = datetime.datetime.now().strftime("%Y-%m-%d_%H:%M:%S")

# function to gather a list of the names of all csv files in the folder.
def find_csv_filenames(path_to_dir, suffix=".csv"):
	filenames = listdir(path_to_dir)
	return [ filename for filename in filenames if filename.endswith(suffix)]

# Asks user for data folder.  Should be the same as "annotation_folder" for overall_analysis.py
data_folder = tkFileDialog.askdirectory(initialdir= "/home/eebrandt/projects/temp_trials/test/data", title = "Choose the folder that contains data files.")

# Defines the header for the .csv file we're going to make
durations_output_header = ["tape_video", "complete", "individual", "treatment" , "rank", "date", "temperature", "weight", "ct_width", "scrape_q1", "scrape_q2", "scrape_q3", "scrape_q4", "scrape_avg", "thump_q1", "thump_q2", "thump_q3", "thump_q4", "thump_avg", "buzz_q1", "buzz_q2", "buzz_q3", "buzz_q4","buzz_avg", "srates_q1", "srates_q2", "srates_q3", "srates_q4", "srates_avg"]

# Defines and opens a .csv file that we'll write our file to
fl = open(data_folder + "/" + "duration_summary" + "_" + timestamp + '.csv', 'w')
writer = csv.writer(fl)
# writes header to csv file
writer.writerow(durations_output_header)

# looks in each individual folder for trial folders
individuals =  os.listdir(data_folder)
for individual in individuals:
	# make sure each "individual" is a folder
	if os.path.isdir(data_folder + "/" + individual):
		trials = os.listdir(data_folder + "/" + individual)
		# loop to go through each individual
		for trial in trials:
			# defines the folders that we'll be looking in for csvs.
			trial_folder = data_folder + "/" + individual + "/" + trial
			# makes sure each trial is a folder
			if os.path.isdir(trial_folder):
				# looks for all the csvs. in a trial folder
				csvs = find_csv_filenames(trial_folder, suffix = ".csv")
				# checks to make sure there is at least one csv file in the folder.  If not, moves to next one and alerts the user to this
				# throws error if there are no csv files
				if not csvs:
					tkMessageBox.showinfo(
					"Missing .csv file",
					"Duration file for " + trial + " was not found.  Moving to next one")
					break
				creation_time = []
				# gets creation time for each file, so we only use the most recent one				
				for csv in csvs:
					creation_time.append(os.path.getmtime(trial_folder + "/" +  csv))
				recent_time = max(creation_time)
				max_index = creation_time.index(recent_time)
				# file that we'll actually load
				use_file = csvs[max_index]
				# arguments that we'll use to open the folder.  Specifically: we're setting the data type for each column and naming each column.
				kwargs = dict(delimiter=",",
               				dtype= "S10, S10, S10, S10, S10, S10, f20, f20, f20, f20, f20, f20, f20, f20, f20, f20, f20, f20, S200",
              				names= "video, complete, individual, treatment, rank, date, temperature, weight, ct_width, scrape_pos, scrape_dur, thump_pos, thump_dur, buzz_pos, buzz_dur, srate_pos, srate_dur, srate_num, comments",
					skip_header=1)
				# load the data file once we know which is the proper one 
				data_file = np.genfromtxt(trial_folder + "/" + use_file, **kwargs)
				# get means for scrapes
				overall_dur = 0
				overall_count = 0
				feature_type = ["scrape", "thump", "buzz"]
				feature_counter = 0
				allmeanfeatures = []
				for feature in feature_type:
					quartile_durs = []
					mean_quartile = []
					quartile_count = 1
					average_count = []
					while quartile_count < 5:
						readvar = 0
						for item in data_file[feature + "_pos"]:
							if not np.isnan(item) and float(item) < (quartile_count * .25):
								average_count.append(data_file[feature + "_dur"][readvar])
								quartile_durs.append(data_file[feature +"_dur"][readvar])
							readvar = readvar + 1
						# makes sure there's something in the quartile_durs array.  It will throw an error (but not abort the program) if this isn't included
						if quartile_durs:
							mean_quartile.append(np.mean(quartile_durs))
						else:
							mean_quartile.append(" ")	
						quartile_count = quartile_count + 1
					# also makes sure there's something in average_count, so we don't get weird errors
					if average_count:
						mean_quartile.append(np.mean(average_count))
					else:
						mean_quartile.append(" ")
					allmeanfeatures.append(mean_quartile)
					feature_counter = feature_counter + 1	
				ratesquart = []
				rates = [None] * 5
				quartile_count = 0
				while quartile_count < 5:
					readvar = 0
					sdur = 0
					scount = 0
					for item in data_file["srate_pos"]:
						if float(item) < (quartile_count * .25) and not np.isnan(item):
							sdur = sdur + data_file["srate_dur"][readvar]
							scount = scount + data_file["srate_num"][readvar]
						readvar = readvar + 1
					if float(scount) != 0 and float(sdur) != 0:
						savg = float(scount)/float(sdur)
						rates[quartile_count - 1] = savg
						overall_dur = overall_dur + sdur
						overall_count = overall_count + scount
					quartile_count = quartile_count + 1
				overall_avg = overall_count/overall_dur
				rates[4] = overall_avg
				
				# these two conditionals put empty string instead of nan if weight or ct is missing.  Easier for later import				
				if data_file["weight"][0] == False:
					weight = " "
				else:
					weight = data_file["weight"][0]

				if data_file["ct_width"][0] == False:
					ct = " "
				else:
					ct = data_file["ct_width"][0]

				# defines the row that we're going to write to the csv.  This part is messy, but not an easier way to do this, really
				row = [data_file["video"][0], data_file["complete"][0], data_file["individual"][0], data_file["treatment"][0] , data_file["rank"][0], data_file["date"][0], data_file["temperature"][0], weight, ct, allmeanfeatures[0][0], allmeanfeatures[0][1], allmeanfeatures[0][2], allmeanfeatures[0][3], allmeanfeatures[0][4], allmeanfeatures[1][0], allmeanfeatures[1][1], allmeanfeatures[1][2], allmeanfeatures[1][3], allmeanfeatures[1][4], allmeanfeatures[2][0], allmeanfeatures[2][1], allmeanfeatures[2][2], allmeanfeatures[2][3], allmeanfeatures[2][4], rates[0], rates[1], rates[2], rates[3], rates[4]]

				writer.writerow(row) 
fl.close() 
