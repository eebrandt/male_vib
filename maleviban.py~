#!/usr/bin/python
import numpy as np
import ctypes


def importanns(wavpath):
	"""
    	This function opens a file containing duration information for vibratory song components.  It takes a path name (string), and outputs a numpy array that contains the following columns: component labels, component start times, component end times, component midpoints(useful for plotting components against total song length), length of component, percent of the total song at which the midpoint of a component occurs.
	"""
	# imports the label file with the specified path/file name
	loadarray = np.array(np.loadtxt(open(wavpath),dtype = "string", delimiter = "\t,", skiprows = 0))

	#variable to count through loop
	readvar = 0
	# defines three arrays, start time and end time for a given vibratory feature
	startarray = []
	endarray = []
	labelarray = []

	# loops through loadarray, converting starts and ents to floats. We end up with startarray (starting times), endarray(ending times), and labelarray (labels (s, t, b, and r)
	while readvar < loadarray.shape[0]:
		# splits a given line of loadarray into a separate string
		readstring = loadarray[readvar].split()	
		# converts all numbers to floats, appends to starting and ending arrays
		startarray.append(float(readstring[0]))
		endarray.append(float(readstring[1]))
		labelarray.append(readstring[2])
		readvar = readvar + 1
	songlength = (max(startarray) - min(startarray))
	# now we need to make three separate arrays, one each for scrapes, thumps, buzzes, and rate regions

	# this array holds the names of all the strings we're interested in iterating (in this case, scrape, thump, buzz, rate)
	strings = ["s", "t", "b", "r"]
	# variable for looping trhough each string we want to test
	stringloop = 0
	
	# sets the output variable as global so it can be returned
	global lengths_output
	# defines the output variable
	lengths_output = [0, 0, 0, 0]

	# loops through each string we want to test
	while stringloop < len(strings):
		readvar = 0
		divlabels = []
		divstart = []
		divlength = []
		divend = []
		divmid = []
		divpercent = []
		songstart = min(startarray)
		songend = max(endarray)
		songlength = songend - songstart
		# loops through entire array
		while readvar < len(labelarray):
			# asks whether a given element matches our string of interest.  If it does, adds information to the relevant arrays
			if strings[stringloop] in labelarray[readvar]:	
				divlabels.append(labelarray[readvar])
				start = (startarray[readvar])
				divstart.append(start)
				end = (endarray[readvar])
				divend.append(end)
				length = (end - start)
				divlength.append(length)
				mid = (length / 2) + songstart + (start - songstart)
				divmid.append(mid)
				mid_to_start = mid - start
				mid_to_end = songend - mid
				percent = (mid - songstart)/songlength			
				divpercent.append(percent)
			readvar = readvar +1
		# combines all of our separate arrays into one variable
		divarray = np.array([divlabels, divstart, divend, divmid, divlength, divpercent])
		# sets our current divnparray to the relevant portion of the output variable	
		lengths_output[stringloop] = divarray
		# resets all our arrays so we don't just keep appending them
		stringloop = stringloop + 1
	# returns our output when we're all done	
	return lengths_output



def plotlengths (scrapedur, thumpdur, buzzdur):
	""" 
	This function takes three arrays that contain duration info for scrapes, thumps, and buzzes.  It generates a scatterplot that plots the duration of a component over the total length of the song (length given in percent length).
	"""
# make plots for an individual that compare where the signal occurs in time to its length.  This is a good quick visual check to make sure everything's working well.  It also gives a good indication of how song elements are distributed temporally.
	import matplotlib.pyplot as plt
	import pylab

	fig = plt.figure(figsize=(5, 4))
	ax = fig.add_subplot(1,1,1)
	# we're just looking at scrapes, thumps, and buzzes here.  Keep in mind that the x, y axes are backwards.
	p1 = ax.scatter(scrapedur[5], scrapedur[4], color = "red", label = "scrapes")
	p2 = ax.scatter(thumpdur[5], thumpdur[4], color = "green", label = "thumps")
	p3 = ax.scatter(buzzdur[5], buzzdur[4], color = "blue", label = "buzzes")
	# sets labels on plot axes
	ax.set_xlabel('% of song at which feature begins')
	ax.set_ylabel('Length of feature (s)')
	# sets up legend
	ax.legend([p2, p1, p3], ["scrapes", "thumps", "buzzes"], 2)
	# sets axis limits
	plt.xlim(0, 1)
	plt.ylim(0, 10)
	# show plot
	plt.show()

def ratecalcs (durationinfo, srfile):
	"""
	This function takes in information about the duration of a given feature (usually scrape-rate regions), and the path of a file that lists each scrape-rate region and the number of scrapes found in that region.  It plots the rates/length of the song (normalized to 1), and returns the array containing rate information.
	"""
	import matplotlib.pyplot as plt
	# variable to loop through each scrape-rate region
	readvar = 0
	# array to hold number of scrapes in a given scrape-rate region
	srnumarray = []
	# array to hold labels for eachregion
	srlabelarray = []
	# opens the file containing numbers of scrapes per scrape-region, note that we're opening it as a string
	readsrarray = np.array(np.loadtxt(open(srfile),dtype = "string", delimiter = "\t,", skiprows = 0))
	# loop to split the rows of the array we read in, into columns
	while readvar < readsrarray.shape[0]:
		# splits a given line of loadarray into a separate string
		srarray = readsrarray[readvar].split()	
		# converts counts to floats, adds them to the srnumarray
		srnumarray.append(float(srarray[0]))
		# writes all labels to srlabelarray
		srlabelarray.append(srarray[1])
		readvar = readvar + 1
	# resets the looping counter
	readvar = 0
	# array to hold scrape-rate counts
	countarray = []
	# array to hold scrape rates
	ratearray = []
	# array to hold component time mid-points
	midarray = []
	# array to hold percent of total song at which midpoint occurs
	percentarray = []
	# array to hold lengths of components
	lengtharray = []
	# loop to calculate duration info, goes through durationinfo. note: durationinfo and the input rate file must be in the same order, or you'll get strange numbers			
	while readvar < len(srlabelarray):
		#print durationinfo[0, readvar]
		#print srlabelarray[readvar]
		# checks to make sure the labels in the two files are in the same order.  If not, it doesn't stop anything, but prints a warning
		if durationinfo[0, readvar] not in srlabelarray[readvar]:
			print 'The scrape-rate regions do not seem to be in the same order in both files.  You probably want to fix this and re-run these data.'
		# scrape counts
		counts = float(srnumarray[readvar])
		countarray.append(counts)
		# length of comonent
		lengtharray.append(float(durationinfo[4, readvar]))
		# counts/second
		rate = counts/float(durationinfo[4, readvar])
		ratearray.append(float(rate))
		# midpoint of component
		mid = durationinfo[3, readvar]
		midarray.append(float(mid))
		# percent of song at which midpoint of component happens
		percent = durationinfo[5, readvar]
		percentarray.append(float(percent))					
		readvar = readvar + 1		
	global srtot
	# makes the final array that gets returned to whoever called the function
	srtot = [srlabelarray, countarray, lengtharray, ratearray, midarray, percentarray]
	ndsrtot = np.array(srtot)

	# makes a figure of the rate data
	fig = plt.figure(figsize=(5, 4))
	ax = fig.add_subplot(1,1,1)
	ax.set_xlabel('% of song at which feature begins')
	ax.set_ylabel("Scrape Rate (scrapes/second)")
	x = percentarray
	y = ratearray
	ratefit = np.polyfit(x, y, 1)
	yfit = np.polyval(ratefit, x)
	p1 = plt.plot(x, y, color = "red", marker='o', linestyle = 'None', label = 'scrape rates')
	p2 = plt.plot(x, yfit, label = 'linear fit line')
	plt.xlim(0, 1)
	#print (min(y))
	#print (min(y)) - .10 * min(y)
	plt.ylim(min(y) - (.05 * min(y)), max(y) + (.10 * max(y)))
	plt.legend(loc='upper center', shadow=True, numpoints = 1)
	#plt.legend([p1, p2], ["scrape rate", "linear fit line"], 2, scatterpoints = 1)
	plt.show()
	return ndsrtot					
	
importanns("/home/eebrandt/projects/temp_trials/test/5.41.labels.txt")
ratecalcs(lengths_output[3], "/home/eebrandt/projects/temp_trials/test/5.41.sr.txt")
