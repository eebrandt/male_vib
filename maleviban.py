#!/usr/bin/python
import numpy as np


def importanns(wavpath):

	# imports the label file with the specified path/file name
	loadarray = np.array(np.loadtxt(open(wavpath),dtype = "string", delimiter = "\t,", skiprows = 0))

	#variable to count through loop
	readvar = 0
	# defines three arrays, start time and end time for a given vibratory feature
	startarray = []
	endarray = []
	labelarray = []

	# loops through loadarray, converting starts and ends to floats. We end up with startarray (starting times), endarray(ending times), and labelarray (labels (s, t, b, and r)
	while readvar < loadarray.shape[0]:
		# splits a given line of loadarray into a separate string
		readstring = loadarray[readvar].split()	
		# converts all numbers to floats, appends to starting and ending arrays
		startarray.append(float(readstring[0]))
		endarray.append(float(readstring[1]))
		labelarray.append(readstring[2])
		readvar = readvar + 1

	# now we need to make three separate arrays, one each for scrapes, thumps, buzzes, and rate regions

	# this array holds the names of all the strings we're interested in iterating (in this case, scrape, thump, buzz, rate)
	strings = ["s", "t", "b", "r"]
	# variable for looping trhough each string we want to test
	stringloop = 0
	# variable to loop through each item in the array
	readvar = 0
	# lists to hold the start time, length, end time, and percent of total song values for each song element
	divstart = []
	divlength = []
	divend = []
	divpercent = []
	
	
	# sets the output variable as global so it can be returned
	global lengths_output
	# defines the output variable
	lengths_output = [0, 0, 0, 0]

	# loops through each string we want to test
	while stringloop < len(strings):
		readvar = 0
		# loops through entire array
		while readvar < len(labelarray):
			# asks whether a given element matches our string of interest.  If it does, adds information to the relevant arrays
			if strings[stringloop] in labelarray[readvar]:
				divstart.append(startarray[readvar])	
				divend.append(endarray[readvar])
				divlength.append(endarray[readvar]-startarray[readvar])		
				divpercent.append((startarray[readvar] - min(startarray))/(max(startarray) - min(startarray)))
			readvar = readvar +1
		# combines all of our separate arrays into one variable
		divarray = [divstart, divend, divlength, divpercent]
		# turns divarray into a np array
		divnparray = np.array(divarray)	
		# sets our current divnparray to the relevant portion of the output variable	
		lengths_output[stringloop] = divnparray
		# resets all our arrays so we don't just keep appending them
		divstart = []
		divlength = []
		divend = []
		divpercent = []
		stringloop = stringloop + 1
	# returns our output when we're all done	
	return lengths_output



def plotlengths (scrapedur, thumpdur, buzzdur):
# make plots for an individual that compare where the signal occurs in time to its length.  This is a good quick visual check to make sure everything's working well.  It also gives a good indication of how song elements are distributed temporally.
	import matplotlib.pyplot as plt
	import pylab

	fig = plt.figure(figsize=(5, 4))
	ax = fig.add_subplot(1,1,1)
	# we're just looking at scrapes, thumps, and buzzes here.  Keep in mind that the x, y axes are backwards.
	p1 = ax.scatter(scrapedur[3], scrapedur[2], color = "red", label = "scrapes")
	p2 = ax.scatter(thumpdur[3], thumpdur[2], color = "green", label = "thumps")
	p3 = ax.scatter(buzzdur[3], buzzdur[2], color = "blue", label = "buzzes")
	ax.set_xlabel('% of song at which feature begins')
	ax.set_ylabel('Length of feature (s)')
	ax.legend([p2, p1, p3], ["scrapes", "thumps", "buzzes"], 2)
	plt.xlim(0, 1)
	plt.ylim(0, 10)
	plt.show()



importanns("/home/eebrandt/projects/temp_trials/test/5.41.labels.txt")
plotlengths(lengths_output[0], lengths_output[1], lengths_output[2])
