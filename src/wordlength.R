library('XML')
default.texts.dir <- 'texts'
default.texts.url <- 'http://www.tesserae-dev.org/elegiacs/meter_texts.tgz'
default.output <- 'wl-data.txt'

#
# functions
#

# check whether texts directory exists
fail.text.dir <- function(dirname) {
	test = file.info(dirname)$isdir
	
	return(is.na(test) | test == FALSE)
}

# look for default text directory
init.text.dir <- function(dirname=default.texts.dir, url=default.texts.url) {

	if (fail.text.dir(dirname)) {
		cat("Can't find texts; trying to download...\n")

		staging = tempfile()
		if (! download.file(url, staging)) {
			untar(staging)
		}
	}
	
	if (fail.text.dir(dirname)) {
		cat("Failure!\n")
		return(NA)
	}
	
	return(dirname)
}

# determine verse category 
getVerseType <- function(class, n) {
	label <- NA
	
	# if it's a hexametric poem, label 'sh' 
	#   for 'stichic hexameter'
	if (class == 'hex') {
		label <- 'sh'
	}
	
	# if it's an elegiac poem, label odd verses
	#   'eh' for 'elegiac hexameter' and even verses
	#   'ep' for 'elegiac pentameter'
	if (class == 'eleg') {
		
		n <- sub('\\D', '', n)
		n <- as.numeric(n)
		
		label <- ifelse(n %% 2 > 0, 'eh', 'ep')
	}
	
	return(label)
}


# process a line
parseXMLLine <- function(l, author, file, class) {

	content <- xmlValue(l, encoding='utf8')
	n <- xmlGetAttr(l, 'n')
	label <- getVerseType(class, n)
	
	return(data.frame(label=label, author=author, file=file, wchars=feat.wchar.count(content), words=feat.word.count(content)))
}

parseTextFile <- function(file, author, class) {

	xml.doc <- xmlParse(file)
	
	lset <- getNodeSet(xml.doc, '//l')
	
	rows <- do.call(rbind, lapply(lset, parseXMLLine, author=author, file=file, class=class))
	
	return(rows)
}

# average wchars and words over a sample of n lines
chunkAuthorLabel <- function(author, label, chunksize) {

	rows <- which(verse.data$author==author & verse.data$label==label)
 	nchunks <- floor(length(rows)/chunksize)
	rows <- sample(rows, nchunks*chunksize)
	mask <- floor(seq(rows) %% nchunks)+1
	
	do.call(rbind, lapply(1:nchunks, function(x) {
		wchars <- mean(verse.data[rows[mask==x], 'wchars'])
		words <- mean(verse.data[rows[mask==x], 'words'])
		return(data.frame(author=author, label=label, wchars=wchars, words=words))
	}))
}

chunkAuthor <- function(author, chunksize) {

	rows <- which(verse.data$author==author)
	labels <- levels(verse.data[rows, 'label'])
	
	do.call(rbind, lapply(labels, function(label) {
		chunkAuthorLabel(author=author, label=label, chunksize=chunksize)
	}))
}

# word characters per verse
feat.wchar.count <- function(text) {

	words.only <- gsub('\\W', '', text)
	return(nchar(words.only))
}


# words per verse
feat.word.count <- function(text) {

	words <- unlist(strsplit(text, '\\W+'))
	words <- words[words != '']
	return(length(words))
}


#
# main
#

# load index
text.dir <- init.text.dir()

text.index <- read.table(file.path(text.dir, 'index.txt'), header=TRUE)
text.index$file <- as.character(text.index$file)

# data table
verse.data <- data.frame()

# process texts
for (i in seq(nrow(text.index))) {
	
	cat(paste('Reading ', text.index[i, 'file'], ' [', i, '/', nrow(text.index), ']\n', sep=''))
	
	file <- file.path(text.dir, paste(text.index[i, 'file'], 'xml', sep='.'))
	author <- text.index[i, 'author']
	class <- text.index[i, 'class']
	
	verse.data <- rbind(verse.data, parseTextFile(file=file, author=author, class=class))
}

# group in 20-line samples
chunked.means <- do.call(rbind, lapply(levels(verse.data$author), chunkAuthor, chunksize=20))

# export data
cat(paste('Writing ', default.output, '\n', sep=''))
write.table(chunked.means, file=default.output, quote=FALSE)