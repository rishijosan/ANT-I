	fname <- file.choose()
	dat = read.csv(fname, header = TRUE)

	range = dat

	reg = lm(range[["corrRT"]] ~ (range[["group"]] + range[["trial"]] + range[["block"]] )^3 )
	scaledRes = scale(resid(reg))
	for (i in 1:length(scaledRes)) {scaledRes[i] = 10*scaledRes[i] + 50}

	
	startArr = array()
	startArr[1] = 1
	sub = range[1,1]
	
	for (k in 1:length(scaledRes))
	{
		if (sub != range[k,1])
		{
			startArr[length(startArr) + 1] = k
		}
		sub = range[k,1]
	}
	
	numSubjects = length(startArr)
	startArr[length(startArr) + 1] = length(scaledRes)+1
	
	for (j in (1:numSubjects))
	{
	start = startArr[j]
	end = startArr[j+1]-1
	iiv = sd(scaledRes[c(start:end)])
	
	print(range[start,1])
	print(iiv)
	writeLines(" ")


	}
