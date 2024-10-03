#' conditionalProbNspades
#' @export
conditionalProbNspades<-
	function(N) {
		# returns an array of probabilities of number of spades in an opponent's (unknown) hand
		# conditioned on the number of spades in my (known) hand
		# arr position 1 corresponds to n = 0
		arr<-array(dim=14)
		for(n in 0:13) {
			arr[n+1]<-choose(13-N,n)*choose(26+N,13-n)/choose(39,13)
			}
		return(arr)
		}

#' conditionalProbNspadesDriver
#' @export	
conditionalProbNspadesDriver<-
	function() {
		# driver to invoke conditionalProbNspades
		m<-matrix(data=1:196,nrow=14,ncol=14,byrow=TRUE)
		for(N in 0:13) { # N is the number of spades in my hand
			arr<-conditionalProbNspades(N)
			for(n in 1:14)
				m[N+1,n]<-arr[n]
			}
			
	return(m)
	}

#' cumulativeConditionalProbNspades
#' @export	
cumulativeConditionalProbNspades<-
	function() {
		# compute cumulative probabilities from the matrix of conditional probabilities
		CUM=matrix(nrow=14,ncol=14)
		rownames(CUM)<-0:13
		colnames(CUM)<-0:13
		m<-conditionalProbNspadesDriver()
		rownames(m)<-0:13
		colnames(m)<-0:13
		print.table(round(m,4))
		for(N in 1:14) # N is the number of spades in my hand
			for(cum in 1:14)
				CUM[N,cum]<-sum(m[N,1:cum])
		print.table(round(CUM,4))
		print.table(round(1-CUM,4))
	}