#######################################
# Produce summary statistics for a single e-mail 
spamCounts = function(subject, body){ 
  # assume subject is a character string
  # assume body is a character vector, where each element is a line of body text
  
  # find the number of exclamation marks in the subject of the e-mail
  num_exclam = length(grep("[\\!]",unlist(strsplit(subject,""))))
 
  # the proportion of uppercase characters in the body or subject

  frac_upper_body=length(grep("[[:upper:]]", unlist(strsplit(body,""))))/length(grep("[[:alpha:]]",unlist(strsplit(body,""))))
  frac_upper_sub=length(grep("[[:upper:]]", unlist(strsplit(subject,""))))/length(grep("[[:alpha:]]",unlist(strsplit(subject,""))))
  
  # return a vector with num_exclam, frac_upper_body, frac_upper_sub
  # (in that order)
  return(list(num_exclam,frac_upper_body,frac_upper_sub))
}



# Check if the Reply-To e-mail contains an underscore
spamReplyTo = function(reply_to){
  # assume reply_to is a character string
  
  # logical, TRUE if reply_to contains an underscore
  hasUnderscore = if(length(grep("[\\_]",reply_to))!=0){
    return(TRUE)}else{
      return(FALSE)
    }
  
  
  # return hasUnderscore
return(hasUnderscore)}


# Check if the subject contains mixed characters, like V1agra or Cia1is
spamSubject1 = function(subject){
  # assume subject is a character string
  
  # hasMixedChar is TRUE if subject contains punctuation or digits
  # surrounded by characters
 subject=gsub("'","i",subject)
  hasMixedChar = if(length(grep("[[:alpha:]]+[[:digit:][:punct:]][[:alpha:]]+",subject))==0){
    return(FALSE)} else{
   
  return(TRUE)}
  return(hasMixedChar)
}



# Check if the subject is on a different topic than the body
spamSubject2 = function(subject, body){
  # assume subject is a character string
  # assume body is a character vector, where each element is a line of body text
  
  # isOffTopic is TRUE if the subject has the form "Re: blah" and 
  # "blah" is NOT in the body text
  isOffTopic = if(length(grep("^[[:blank:]]?R[Ee][\\:][[:blank:]]",subject))==0){
    return(FALSE)} else{
      match=substring(subject,5,nchar(subject))
      if(length(grep(match,body,fixed=TRUE))==0){
        return(TRUE)}
        else{ 
          return(FALSE)}
    }
  
  # return isOffTopic
  return(isOffTopic)
}



#######################################
# Put all of these functions together to build a basic spam filter

spamFilter = function(email, max_exclam, max_up_body, max_up_sub){
  # email is a list with at least two components, header and body (as
  # described in the assignment PDF, or Emails[[1]] for an example)  
  # max_exclam is the upper threshold for the number of exclamation marks
  # in the subject
  # max_up_body, max_up_sub are upper thresholds for the fraction of
  # uppercase characters in the body and subject, respectively.
  
  
  # isSpam is TRUE if at least one of these is true 
  # 1. num exclamation marks in subject > max_exclam
  # 2. fraction of uppercase in body > max_up_body
  # 3. fraction of uppercase in subject > max_up_sub
  # 4. spamReplyTo() returns TRUE
  # 5. spamSubject1() returns TRUE
  # 6. spamSubject2() returns TRUE 
  
  isSpam = if (is.na(email$header["Subject"])) return(TRUE) else {
  if (is.na(email$body$text))return(TRUE) else {
    if(length(grep("^[[:blank:]]?R[Ee][\\:][[:blank:]]&)",email$header["Subject"]))>=1){
      return(TRUE)} else{
        v=unlist(spamCounts(email$header["Subject"],email$body$text)) > c(max_exclam,max_up_body,max_up_sub)
      if(sum(v,na.rm=TRUE)>=1)
    return(TRUE) else {
     if(is.na(email$header["Reply-To"])) 
       return(spamSubject1(email$header["Subject"])|(spamSubject2(email$header["Subject"],email$body$text))) else {
        
           return(spamReplyTo(email$header["Reply-To"]) | spamSubject1(email$header["Subject"]) | spamSubject2(email$header["Subject"],email$body$text))}
    }}}}
  # return isSpam
  return(isSpam)
}
