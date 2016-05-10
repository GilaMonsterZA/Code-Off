# install.packages("plyr") # to install if not present
# install.packages("stringr)
require(plyr) 
require(stringr) 

MorseCode <- read.delim("~/RScripts/CodeOff/code_off-10/MorseCode.txt", stringsAsFactors=FALSE)
MaxCharacterLength <- 6 # longest morse code character

Message <- scan("~/RScripts/CodeOff/code_off-10/code_off-10.in", what="", sep="\n") 

# We need, as part of the encoding, to seperate words with the /, 
# so do it now while the message is still in strings rather than after it's split into characters
Message <- str_replace_all(Message, fixed(" "), "/") 
MessageLetters <- str_split(Message, "") # split the message into individual characters. Results in a list of lists

MessageEncoded <- list(1:length(MessageLetters)) # Placeholder for the encoded letters

#Get lists of dashes and dots, starting at 6 characters long and going down to 1
ListOfDots <- lapply(lapply(c(MaxCharacterLength:1), function(x) rep.int(".", times = x)), function(x) str_join(x, collapse=''))
ListOfDashes <- lapply(lapply(c(MaxCharacterLength:1), function(x) rep.int("-", times = x)), function(x) str_join(x, collapse=''))
# and calculate the correct encoding for each
EncodingDot <- as.character(nchar(ListOfDots))
EncodingDash <- sapply(nchar(ListOfDashes)+64,function(x) rawToChar(as.raw(x)))

for (i in 1:MaxCharacterLength) { #Encode the original Morsecode, not the message. Should be faster than looping over a, potentially, long message
  MorseCode$Encoding <- gsub(ListOfDots,EncodingDot,MorseCode$Encoding,fixed=TRUE) # replace the dots with their encoding
  MorseCode$Encoding <- gsub(ListOfDashes,EncodingDash,MorseCode$Encoding, fixed=TRUE) # replace dashes with their encoding
}

for (i in seq_along(MessageLetters)) { #Then convert the message into the, already encoded, morse
  MessageEncoded[[i]] <- mapvalues(MessageLetters[[i]],MorseCode$Letter, MorseCode$Encoding, warn_missing = FALSE)
}

# concatenate and clean up the excess seperation characters that paste added
EncodedMessage = sapply(lapply(MessageEncoded, function(x) str_join(x, collapse="|")), function(x) gsub("|/|","/",gsub("||","|",x, fixed=TRUE),fixed=TRUE)) 

writeLines(EncodedMessage, "~/RScripts/CodeOff/code_off-10/code_off-10.out")
