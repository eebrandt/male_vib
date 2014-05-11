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
	plt.ylim(min(y) - (.05 * min(y)), max(y) + (.10 * max(y)))
	plt.legend(loc='upper center', shadow=True, numpoints = 1)
	#plt.legend([p1, p2], ["scrape rate", "linear fit line"], 2, scatterpoints = 1)
	plt.show()
	return ndsrtot	
