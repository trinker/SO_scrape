FUN <- function(n) {
    URL <- paste0("http://stackoverflow.com/questions/tagged/r?page=", 
        n, "&sort=newest&pagesize=50")
    doc <- htmlTreeParse(URL, useInternalNodes=TRUE)
    x <- getNodeSet(doc, "//h3/a") 
    x <- lapply(x, xmlToList)
    y <- sapply(sapply(x, "[", ".attrs"), "[", "href")
    names(y) <- NULL
    z <- unlist(sapply(x, "[", "text"))
    names(z) <- NULL
    data.frame(url = y, title = z)
}

FUN2 <- function(n = 1, dat = URLS, root = "http://stackoverflow.com") {
    URL <- paste0(root, URLS[n, 1])
    doc <- htmlTreeParse(URL, useInternalNodes=TRUE)
    x <- getNodeSet(doc, "//h1/a") 
    title <- sapply(x, xmlValue)
    post <- getNodeSet(doc, "//td[@class='postcell']//div[@class='post-text']//p/text()")
    post <- sapply(post, xmlValue)    
    code <- getNodeSet(doc, "//td[@class='postcell']//div[@class='post-text']//code/text()")
    exp.code <- paste(sapply(code, xmlValue), collapse="|||||")
    n.code.chunks <- length(code)
    code.len <- sum(nchar(sapply(code, xmlValue)))
    n.ans <- getNodeSet(doc, "//h2")
    n.ans <- sapply(n.ans, xmlValue)[[1]]
    n.ans <- ifelse(grepl("closed", n.ans), NA, ifelse(!grepl("Answer", n.ans), 
    	0, as.numeric(gsub("[^\\d]+", "", n.ans, perl = TRUE))
    ))
    tags <- getNodeSet(doc, "//div[@class='post-taglist']")
    tags <- unlist(strsplit(Trim(clean(sapply(tags, xmlValue))), "\\s+"))
    q.time <- getNodeSet(doc, "//td[@class='post-signature owner']//div[@class='user-action-time']//span[@class='relativetime']")
	if (is.null(q.time)) {
	    q.time <- getNodeSet(doc, "//span[@class='relativetime']")
	    q.time <- unlist(lapply(q.time, xmlToList))
	    q.time <- as.POSIXct(q.time[names(q.time) == ".attrs.title"])
	    q.time <- min(q.time)
	} else {
        q.time <- as.POSIXct(unlist(lapply(q.time, xmlToList))[2])
	}
    q.time.break <- unlist(strsplit(as.character(q.time), " "))
    dow <- weekdays(as.Date(q.time.break[1]))
    t1 <- as.numeric(unlist(strsplit(q.time.break[2], "\\:")))
    n.votes <- getNodeSet(doc, "//span[@class='vote-count-post ']")
    hour <- t1[1] + t1[2]/60 + t1[2]/3600 #EST
    n.votes <- as.numeric(sapply(n.votes, xmlValue))[[1]]
    post.wc <- sum(wc(post), na.rm = TRUE)
    title.wc <- wc(title)
    n.time <- getNodeSet(doc, "//div[@class='answer']//span[@class='relativetime']")
	if (!is.null(n.time)) {
	    n.time <- unlist(lapply(n.time, xmlToList))
        n.time <- as.POSIXct(n.time[names(n.time) == ".attrs.title"])
        n.time <- min(n.time)					 
	    ellapse <- as.numeric(difftime(n.time, q.time, units = "min"))
	} else {
	    n.time <- ellapse <- NA	
	}
    accept <- getNodeSet(doc, "//div[@class='answer accepted-answer']")
    accept <- lapply(accept, xmlToList)
    accept <- ifelse(identical(accept, list()), FALSE, TRUE)  
    data.frame(title.wc = title.wc, post.wc = post.wc, 
        n.code.chunks = n.code.chunks, code.len = code.len, n.ans = n.ans, 
        q.time = q.time, hour = hour, day.o.wk = dow, n.votes = n.votes, 
    	n.time = n.time, min.ellapse = ellapse,tags = paste(tags, collapse="|"),
        accept = accept, code = exp.code)  

}