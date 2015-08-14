

	filePath <- 'C:/dev/Economic/USA'
	setwd(filePath)
	
	#download real GDP data MANUALLY from Bureau of Economic Analysis (BEA) in '.xls'
	#quarterly data starting at Q1 1947 (chained 2009 dollars) and save as '.csv'
	#'http://www.bea.gov/national/index.htm#gdp'
	#download.file(url1, destfile='gdplev.csv', method='libcurl')

	#download unemployment data from Bureau of Labor Statistics in '.xlsx'
	#data starting at Jan 1948, select column format and save as '.csv'
	#'http://data.bls.gov/timeseries/LNS14000000'
	#download.file(url2, destfile='employ.csv', method='libcurl')

	#download HPI and sales from housepriceindex.ca MANUALLY in xls and save as .csv
	#data starting at Jul 1990
	#url3 <- 'http://www.housepriceindex.ca/Excel2.aspx?langue=EN&mail=test@test.com'
	#download.file(url3, destfile='hpi.xls', method='libcurl')
	
	#read GDP
	gdp <- read.csv('gdplev.csv')
	gdp <- gdp[-(1:5),-(1:4)]
	gdp <- gdp[-(1:2),-(4:5)]
	names(gdp) <- c('Date', 'gdpCurrentDollar', 'gdp2009Dollar')
	gdp[,1] <- as.character(gdp[,1])
	gdp[,2] <- as.numeric(gsub(',','',as.character(gdp[,2])))
	gdp[,3] <- as.numeric(gsub(',','',as.character(gdp[,3])))

	#fix Ref_Date column, set day to 15th for every month
	for (i in 1:(nrow(gdp)))
	{
		if (substr(gdp[i,1],5,6) == 'q1')
		{
			gdp[i,1] <- paste(substr(gdp[i,1],1,4),'/03/15',sep='')
		}
		else if (substr(gdp[i,1],5,6) == 'q2')
		{
			gdp[i,1] <- paste(substr(gdp[i,1],1,4),'/06/15',sep='')
		}
		else if (substr(gdp[i,1],5,6) == 'q3')
		{
			gdp[i,1] <- paste(substr(gdp[i,1],1,4),'/09/15',sep='')
		}
		else if (substr(gdp[i,1],5,6) == 'q4')
		{
			gdp[i,1] <- paste(substr(gdp[i,1],1,4),'/12/15',sep='')
		}
	}

	gdp[,1] <- as.Date(gdp[,1],'%Y/%m/%d')
		
	#calculate quarterly % change in real GDP
	gdpDelta <- NA
	for (j in 1:(nrow(gdp))) 
	{
		gdpDelta <- c(gdpDelta, gdp[j,3] / gdp[j-1,3] - 1)
	}	
	gdp2 <- cbind(gdp, gdpDelta)
	gdpMean <- mean(gdpDelta, na.rm=TRUE)
	gdpStdev <- sd(gdpDelta, na.rm=TRUE)	
	input <- NULL
	input <- c(input, gdpMean, gdpStdev, gdp2[nrow(gdp2),1], gdp2[nrow(gdp2),3])

	#read unemployment rate and participation rate
	employ <- read.csv('employ.csv')
	employ <- employ[-(1:11),c(-1,-5,-6)]
	
	#fix Ref_Date column, set day to 15th for every month
	employ[,1] <- as.character(employ[,1])
	for (k in 1:(nrow(employ)))
	{
		if (employ[k,2] == 'M01')
		{
			employ[k,1] <- paste(employ[k,1],'/01/15',sep='')
		}
		else if (employ[k,2] == 'M02')
		{
			employ[k,1] <- paste(employ[k,1],'/02/15',sep='')
		}
		else if (employ[k,2] == 'M03')
		{
			employ[k,1] <- paste(employ[k,1],'/03/15',sep='')
		}
		else if (employ[k,2] == 'M04')
		{
			employ[k,1] <- paste(employ[k,1],'/04/15',sep='')
		}
		else if (employ[k,2] == 'M05')
		{
			employ[k,1] <- paste(employ[k,1],'/05/15',sep='')
		}
		else if (employ[k,2] == 'M06')
		{
			employ[k,1] <- paste(employ[k,1],'/06/15',sep='')
		}
		else if (employ[k,2] == 'M07')
		{
			employ[k,1] <- paste(employ[k,1],'/07/15',sep='')
		}
		else if (employ[k,2] == 'M08')
		{
			employ[k,1] <- paste(employ[k,1],'/08/15',sep='')
		}
		else if (employ[k,2] == 'M09')
		{
			employ[k,1] <- paste(employ[k,1],'/09/15',sep='')
		}
		else if (employ[k,2] == 'M10')
		{
			employ[k,1] <- paste(employ[k,1],'/10/15',sep='')
		}
		else if (employ[k,2] == 'M11')
		{
			employ[k,1] <- paste(employ[k,1],'/11/15',sep='')
		}
		else if (employ[k,2] == 'M12')
		{
			employ[k,1] <- paste(employ[k,1],'/12/15',sep='')
		}
	}

	employ[,1] <- as.Date(employ[,1],'%Y/%m/%d')
	employ <- employ[,-2]
	employ[,2] <- as.numeric(gsub(',','',as.character(employ[,2])))
	names(employ) <- c('Date', 'unemployRate')

	#calculate avg and stdev of unemployment rate
	employMean <- mean(employ$unemployRate, na.rm=TRUE)
	employStdev <- sd(employ$unemployRate, na.rm=TRUE)
	input <- c(input, employMean, employStdev)
	names(input) <- c('gdpMean', 'gdpStdev', 'lastGdpDate', 'lastGdp', 'employMean', 'employStdev')

	#merge tables
	mergedData <- merge(gdp2, employ, by.x= 'Date', by.y = 'Date', all=TRUE)

	write.csv(input,file='input.csv')	
	write.csv(mergedData, file='output.csv')
----------------------------------------------------------------------------------------------
	#read HPI
	hpi <- read.csv('hpi.csv')
	hpi <- hpi [-(1:3), -2]
	names(hpi)[12] <- 'AB_Edmonton'
	names(hpi)[24] <- 'QC_Quebec'
	hpi[,1] <- as.character(hpi[,1])

	for (m in 2:(ncol(hpi)))
	{
		hpi[, m] <- as.character(hpi[, m])
	}

	for (n in 2:(ncol(hpi)))
	{
		hpi[, n] <- as.numeric(hpi[, n])
	}

	hpi[hpi==0]<-NA

	#fix Ref_Date column, set day to 15th for every month
	for (l in 1:(nrow(hpi)))
	{
		hpi[l, 1] <- paste('15-', hpi[l,1], sep='')
	}

	hpi[,1] <- as.Date(hpi[,1],'%d-%b-%Y')
	
	#calculate seasonal adjustment for home sales volumn, HPI and home sales mean and stdev
	finalTable <- as.vector(NULL)
	
	sAdjSaleMean <- NULL
	sAdjSaleStdev <- NULL
	hpiMean <- NULL
	hpiStdev <- NULL
	saleMean <- NULL
	saleStdev <- NULL
	for (p in seq(3, ncol(hpi), 2))
	{
		if (length(hpi[,p]) == length(which(!is.na(hpi[,p]))))
		{
			index <- 0
		}
		else
		{
			index <- max(which(is.na(hpi[,p])))
		}

		seasonAdj <- NULL
		ratio <- NULL
		dates <- NULL
		z <- NA

		for (o in 1:(length(hpi[,p])-index-12))
		{
			x <- mean(hpi[,p][(index+o):(index+o+11)])
			y <- mean(hpi[,p][(index+o+1):(index+o+12)])
			seasonAdj <- c(seasonAdj, (x+y)/2)
			ratio <- c(ratio, hpi[,p][index+6+o]/((x+y)/2))
			dates <- c(dates, hpi$Transaction.Date[index+6+o])
			z <- c(z, seasonAdj[o]/seasonAdj[o-1]-1)
		}

		seasonTable <- data.frame(cbind(dates, seasonAdj, ratio))
		seasonTable$dates <- as.Date(seasonTable$dates, origin = "1970-01-01")

		seasonTable2 <- split(seasonTable, as.numeric(as.numeric(format(seasonTable$dates, '%m'))))
		seasonTable3 <- sapply(seasonTable2, function(elt) mean(elt[,3]))
		sums <- sum(seasonTable3)
		seasonTable4 <- seasonTable3 / sums * 12
		finalTable <- cbind(finalTable, seasonTable4)

		#calculate avg and stdev of seasonally adj. home sales
		sAdjSaleMean <- c(sAdjSaleMean, mean(z, na.rm=TRUE))
		sAdjSaleStdev <- c(sAdjSaleStdev, sd(z, na.rm=TRUE))

		colnames(finalTable)[(p-1)/2] <- names(hpi[p-1])
		
		#calculate avg and stdev of hpi and sales
		a <- NA
		b <- NA
		d <- NULL
		e <- NA
		for (q in 1:(length(hpi[,p-1])))	
		{
			a <- c(a, hpi[q,p-1]/hpi[q-1,p-1]-1)
			b <- c(b, hpi[q,p]/hpi[q-1,p]-1)
		
			#calculate seasonally adj. home sales
			if (format(hpi$Transaction.Date[q],'%m')=='01')
			{
				d <- c(d, hpi[q,p] / finalTable[1, (p-3)/2+1])
			}
			else if (format(hpi$Transaction.Date[q],'%m')=='02')
			{
				d <- c(d, hpi[q,p] / finalTable[2, (p-3)/2+1])
			}
			else if (format(hpi$Transaction.Date[q],'%m')=='03')
			{
				d <- c(d, hpi[q,p] / finalTable[3, (p-3)/2+1])
			}
			else if (format(hpi$Transaction.Date[q],'%m')=='04')
			{
				d <- c(d, hpi[q,p] / finalTable[4, (p-3)/2+1])
			}
			else if (format(hpi$Transaction.Date[q],'%m')=='05')
			{
				d <- c(d, hpi[q,p] / finalTable[5, (p-3)/2+1])
			}
			else if (format(hpi$Transaction.Date[q],'%m')=='06')
			{
				d <- c(d, hpi[q,p] / finalTable[6, (p-3)/2+1])
			}
			else if (format(hpi$Transaction.Date[q],'%m')=='07')
			{
				d <- c(d, hpi[q,p] / finalTable[7, (p-3)/2+1])
			}
			else if (format(hpi$Transaction.Date[q],'%m')=='08')
			{
				d <- c(d, hpi[q,p] / finalTable[8, (p-3)/2+1])
			}
			else if (format(hpi$Transaction.Date[q],'%m')=='09')
			{
				d <- c(d, hpi[q,p] / finalTable[9, (p-3)/2+1])
			}
			else if (format(hpi$Transaction.Date[q],'%m')=='10')
			{
				d <- c(d, hpi[q,p] / finalTable[10, (p-3)/2+1])
			}
			else if (format(hpi$Transaction.Date[q],'%m')=='11')
			{
				d <- c(d, hpi[q,p] / finalTable[11, (p-3)/2+1])
			}
			else if (format(hpi$Transaction.Date[q],'%m')=='12')
			{
				d <- c(d, hpi[q,p] / finalTable[12, (p-3)/2+1])
			}

			#calculate seasonally adj. home sales % change
			e <- c(e, d[q]/d[q-1]-1)

		}
		hpi <- cbind(hpi, a, b, d, e)
		names(hpi)[p] <- paste(names(hpi)[p-1],'Sale')
		names(hpi)[ncol(hpi)-3] <- paste(names(hpi)[p-1],'HPIDelta')
		names(hpi)[ncol(hpi)-2] <- paste(names(hpi)[p],'Delta')
		names(hpi)[ncol(hpi)-1] <- paste(names(hpi)[p],'sAdj')
		names(hpi)[ncol(hpi)] <- paste(names(hpi)[p],'sAdjDelta')
		
		hpiMean <- c(hpiMean, mean(a, na.rm=TRUE))
		hpiStdev <- c(hpiStdev, sd(a, na.rm=TRUE))
		saleMean <- c(saleMean, mean(b, na.rm=TRUE))	
		saleStdev <- c(saleStdev, sd(b, na.rm=TRUE))		
	}

	input <- c(input, hpiMean, hpiStdev, saleMean, saleStdev, sAdjSaleMean, sAdjSaleStdev)
	names(input) <- c('gdpMean', 'gdpStdev', 'lastGdpDate', 'lastGdp', 'employMean', 'employStdev',
				'Composite6HPIMean', 'Composite11HPIMean', 'VictoriaHPIMean', 'VancouverHPIMean',
				'CalgaryHPIMean', 'EdmontonHPIMean', 'WinnipegHPIMean', 'HamiltonHPIMean', 
				'TorontoHPIMean', 'OttawaHPIMean', 'MontrealHPIMean', 'QuebecHPIMean', 'HalifaxHPIMean',
				'Composite6HPIStdev', 'Composite11HPIStdev', 'VictoriaHPIStdev', 'VancouverHPIStdev',
				'CalgaryHPIStdev', 'EdmontonHPIStdev', 'WinnipegHPIStdev', 'HamiltonHPIStdev', 
				'TorontoHPIStdev', 'OttawaHPIStdev', 'MontrealHPIStdev', 'QuebecHPIStdev', 'HalifaxHPIStdev',
				'Composite6SaleMean', 'Composite11SaleMean', 'VictoriaSaleMean', 'VancouverSaleMean',
				'CalgarySaleMean', 'EdmontonSaleMean', 'WinnipegSaleMean', 'HamiltonSaleMean', 
				'TorontoSaleMean', 'OttawaSaleMean', 'MontrealSaleMean', 'QuebecSaleMean', 'HalifaxSaleMean',
				'Composite6SaleStdev', 'Composite11SaleStdev', 'VictoriaSaleStdev', 'VancouverSaleStdev',
				'CalgarySaleStdev', 'EdmontonSaleStdev', 'WinnipegSaleStdev', 'HamiltonSaleStdev', 
				'TorontoSaleStdev', 'OttawaSaleStdev', 'MontrealSaleStdev', 'QuebecSaleStdev', 'HalifaxSaleStdev',
				'Composite6sAdjSaleMean', 'Composite11sAdjSaleMean', 'VictoriasAdjSaleMean', 'VancouversAdjSaleMean',
				'CalgarysAdjSaleMean', 'EdmontonsAdjSaleMean', 'WinnipegsAdjSaleMean', 'HamiltonsAdjSaleMean', 
				'TorontosAdjSaleMean', 'OttawasAdjSaleMean', 'MontrealsAdjSaleMean', 'QuebecsAdjSaleMean', 'HalifaxsAdjSaleMean',
				'Composite6sAdjSaleStdev', 'Composite11sAdjSaleStdev', 'VictoriasAdjSaleStdev', 'VancouversAdjSaleStdev',
				'CalgarysAdjSaleStdev', 'EdmontonsAdjSaleStdev', 'WinnipegsAdjSaleStdev', 'HamiltonsAdjSaleStdev', 
				'TorontosAdjSaleStdev', 'OttawasAdjSaleStdev', 'MontrealsAdjSaleStdev', 'QuebecsAdjSaleStdev', 'HalifaxsAdjSaleStdev')

	#merge tables
	mergedData <- merge(gdp2, employ, by.x= 'Ref_Date', by.y = 'Ref_Date', all=TRUE)
	mergedData <- merge(mergedData, hpi, by.x= 'Ref_Date', by.y='Transaction.Date', all=TRUE)

	write.csv(input,file='input.csv')
	write.csv(finalTable,file='homeSales.csv')	
	write.csv(mergedData, file='output.csv')