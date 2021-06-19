.mread.R_code <- function ( files ) lapply (
	files , read.R_code )

mread.R_code <- function ( file ) {

	DF <- read.csv (
		file = file ,
		colClasses = "character" ,
		stringsAsFactors = FALSE )

	cc <- function ( string ) as.integer ( eval (
		expr = parse ( text = paste0 ( "c(" , string , ")" ) ) ,
		envir = baseenv () ) )

	DF1 <- data.frame (
		stringsAsFactors = FALSE ,
		FILE = DF [[ "FILE" ]] ,
		LINES = matrix ( lapply ( DF [[ "LINES" ]] , cc ) ) )

	FILES.UNIQ <- unique ( DF1 [[ "FILE" ]] )

	TEXT.UNIQ <- .mread.R_code ( FILES.UNIQ )

	fetchLines <- function ( filename , lines , na.replacement = "" ) {
		i <- match ( filename , FILES.UNIQ )
		TEXT <- TEXT.UNIQ [[ i ]]
		if ( ! length ( lines ) )
				lines <- seq_along ( TEXT )
		EXCERPT <- TEXT [ lines , ]
		rownames ( EXCERPT ) <- NULL
		names ( EXCERPT ) [[ 3 ]] <- "TEXT"
		EXCERPT [ is.na ( EXCERPT $ TEXT ) , "TEXT" ] <- na.replacement
		data.frame ( FILE = i , LINE = lines , EXCERPT ) }

	DF2 <- do.call ( rbind , lapply (
		X = seq_len ( nrow ( DF1 ) ) , FUN = function ( i ) {
			fetchLines (
				DF1 [ i , "FILE" ] ,
				unlist ( DF1 [ i , "LINES" ] ) ) } ) )

	DF2 }

