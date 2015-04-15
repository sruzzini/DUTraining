##############################################################################
##    CLIENT: DUAID (DELOITTE UNIVERSITY AGENCY FOR INTERNAL DEVELOPMENT)   ##
##                                                                          ##
##    PROGRAM NAME: DRAFT_SUPERCLEAN                                        ##
##                                                                          ##
##    CREATED BY: STEVE RUZZINI, KAT KOSER,                                 ##
##                                                                          ##
##    DATE: 15 APR 2015                                                     ##
##                                                                          ##
##    PURPOSE: THE PURPOSE OF THIS CODE IS TO HELP DUAID IMPORT             ##
##    CENSUS DATA FROM TEXAS IN ORDER TO ANALYZE THE CHANGE IN POPULATION   ##
##    BY COUNTY EVERY 10 YEARS, COMPARED TO THE STATE AND NATIONAL TRENDS.  ##
##    THIS PROGRAM REPLACES THE MANUAL PROCESS TO MATCH COUNTY DATA ACROSS  ##
##    TIME, WHILE ALLOWING FOR CHANGES TO THE LIST OF COUNTIES AS NEW       ##
##    COUNTIES ARE OCCASIONALLY CREATING OR THE FORMAT CHANGES. FOR         ##
##    EXAMPLE, WILLACY COUNTY WAS ONLY CREATED IN 1911 IN TIME FOR THE      ##
##    1920 CENSUS  ROM PARTS OF OTHER CAMERON AND HIDALGO COUNTIES.         ##
##                                                                          ##
##############################################################################

# TRIM 
# PARAMETERS:
#	"x" - STRING TO REMOVE LEADING AND TRAILING WHITE SPACE FROM
# OUTPUT:
#	INPUT STRING WITHOUT LEADING OR TRAILING WHITE SPACE
trim = function(x) {gsub("^\\s+|\\s+$", "", x);}

# IMPORT DATA, POSTED ONLINE ON A RECURRING BASIS.
raw1 = read.fwf('http://www.census.gov/population/cencounts/tx190090.txt',
                      widths = c(6,9,10,10,10,21), header=FALSE, skip=11);

# INITIALIZE VARIABLES AND STRIP AWAY ROWS OF NULL DATA FROM RAW TEXT READ IN
raw2 = raw1[!(is.na(raw1$V1)),];
counter=0;
innerCounter=1;
numrows = nrow(raw2);
superClean = list();

# CREATE VECTOR FOR EACH COLUMN OF THE DIRTY TEXT DATA
v1 = lapply(raw2$V1, as.character);
v2 = lapply(raw2$V2, as.character);
v3 = lapply(raw2$V3, as.character);
v4 = lapply(raw2$V4, as.character);
v5 = lapply(raw2$V5, as.character);
v6 = lapply(raw2$V6, as.character);

# LOOP THROUGH EACH ROW OF THE TEXT DATA TO 
for (i in 1:numrows) {
	# IF "FIPS" IS DETECTED, CREATE A NEW DATAFRAME AND APPEND TO THE LIST
	if (identical(v1[[i]], " FIPS ")) {
		counter = counter + 1;
		innerCounter = 1;
		superClean[[counter]] = c(v1[i], v2[i], v3[i], v4[i], v5[i], v6[i]);
	}
	#ELSE, ADD DATA TO THE LATEST DATAFRAME APPENDED
	else {
		x = superClean[[counter]];
		superClean[[counter]] = rbind(x,c(trim(v1[i]), as.numeric(trim(v2[i])), as.numeric(trim(v3[i])), as.numeric(trim(v4[i])), as.numeric(trim(v5[i])), trim(v6[i])));
	}
	innerCounter = innerCounter + 1;
}

# NUMBER OF DATAFRAMES APPENDED TO THE LIST
numFrames = length(superClean);

# LOOP THROUGH EACH DATAFRAME IN THE LIST AND EDIT HEADERS
for (i in 1:numFrames) {
	s = superClean[[i]];
	superClean[[i]] = data.frame(s);
	x = superClean[[i]];
	names(x) = c(trim(x[1,1]), trim(x[1,2]), trim(x[1,3]), trim(x[1,4]), trim(x[1,5]), trim(x[1,6]));
	y = x[, !duplicated(colnames(x))];
	superClean[[i]] = y;
}

# MERGE DATAFRAMES HORIZONTALLY INTO 1 ENCAPSULATING DATAFRAME
final = superClean[[1]]
final2 = cbind(final, superClean[[2]]);
final3 = cbind(final2, superClean[[3]]);
final4 = final3[, !duplicated(colnames(final3))];
final5 = final4[2:nrow(final4), ];
colnames(final5)[6] = "Location";
colind = grep("Location", names(final5));

# FINAL CLEANED DATA
RESULT = final5[, c(colind, (1:ncol(final5))[-colind])];
row.names(RESULT) = NULL;

# UNCOMMENT BELOW LINE TO AUTOMATICALLY VIEW CLEANED DATA IN RSTUDIO
#View(RESULT);

# EXPORT TO EXCEL SHAREDRIVE WHERE DUAID CLIENT ACCESSES IT
library(xlsx);
write.xlsx2(RESULT, file="PopulationData.xlsx", sheetName="Census_Deliverable", col.names=TRUE, row.names=FALSE);
print("Done!");