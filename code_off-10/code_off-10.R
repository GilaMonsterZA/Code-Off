# install.packages("plyr") # to install if not present
require(plyr) 

MorseCode <- read.delim("~/RScripts/CodeOff/code_off-10/MorseCode.txt", stringsAsFactors=FALSE)
MaxCharacterLength <- 6 # longest morse code character

MessageLetters <- strsplit(gsub(" ", "/", scan("~/RScripts/CodeOff/code_off-10/code_off-10.in", what="", sep="\n")), "")
MessageEncoded <- list(1:length(MessageLetters))

EncodingDot <- as.data.frame(cbind(lapply(lapply(c(MaxCharacterLength:1), function(x) rep.int(".", times = x)), function(x) paste(x, collapse='')),0))
EncodingDash <- as.data.frame(cbind(lapply(lapply(c(MaxCharacterLength:1), function(x) rep.int("-", times = x)), function(x) paste(x, collapse='')),0))
EncodingDot[,2] <- as.character(nchar(EncodingDot[,1]))
EncodingDash[,2] <- sapply(nchar(EncodingDash[,1])+64,function(x) rawToChar(as.raw(x)))

for (i in 1:MaxCharacterLength) { #Encode the original Morsecode, not the message. Should be faster than looping over a, potentially, long message
  MorseCode$Encoding <- gsub(EncodingDash[i,1],EncodingDash[i,2],gsub(EncodingDot[i,1],EncodingDot[i,2],MorseCode$Encoding,fixed=TRUE),fixed=TRUE)
}

for (i in seq_along(MessageLetters)) { #Then convert the message into the, already encoded, morse
  MessageEncoded[[i]] <- mapvalues(MessageLetters[[i]],MorseCode$Letter, MorseCode$Encoding, warn_missing = FALSE)
}

EncodedMessage = sapply(lapply(MessageEncoded, function(x) paste(x, collapse="|")), function(x) gsub("|/|","/",gsub("||","|",x, fixed=TRUE),fixed=TRUE)) # concatenate and clean up the excess seperation characters that paste added
writeLines(EncodedMessage, "~/RScripts/CodeOff/code_off-10/code_off-10.out")
