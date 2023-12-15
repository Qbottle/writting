data<-read.csv("C:/Users/GB Park/Documents/DB.csv")
#Q1) Misikga
ipmat<-rep(0,89)
ipmat2<-rep(0,89)
j=1;
temp=data$Label[1]
for(i in (1:length(data$Label))){
  if(substr(data$Label[i],1,1)=='A'){data$Label[i]<-as.numeric(substr(data$Label[i],2,3))}
  else{data$Label[i]<-(as.numeric(substr(data$Label[i],2,3))+43)}
}

for(i in (1:length(data$Label))){
  if(data$Label[i]==temp){ipmat[j]=ipmat[j]+data$Price[i]
  ipmat2[j]=ipmat2[j]+1}
  else{
    ipmat[j]=(ipmat[j]/ipmat2[j])
    j=j+1
    ipmat[j]=ipmat[j]+data$Price[i]
    ipmat2[j]=ipmat2[j]+1
  }
  temp=data$Label[i]
}
ipmat[89]=(ipmat[89]/ipmat2[89])

misik<-which(ipmat>summary(ipmat)[5][[1]])
misik.Nfreq<-rep(0,22)
misik.Nfreq2<-rep(0,22)
nonmisik.Nfreq<-rep(0,67)
nonmisik.Nfreq2<-rep(0,67)
temp="1";temp2="2";j=1;j2=1;
for(i in (1:length(data$Label))){
  if(data$Label[i] %in% misik){
  if(data$Label[i]==temp){
    if(data$Location[i]=='North'){
      misik.Nfreq[j]=misik.Nfreq[j]+1}
    misik.Nfreq2[j]=misik.Nfreq2[j]+1}
  else{
    if(misik.Nfreq[j]!=0){misik.Nfreq[j]=(misik.Nfreq[j]/misik.Nfreq2[j])}
    j=j+1
    if(data$Location[i]=='North'){
      misik.Nfreq[j]=misik.Nfreq[j]+1}
    misik.Nfreq2[j]=misik.Nfreq2[j]+1
  }
  temp=data$Label[i]
  }
  else{
    if(data$Label[i]==temp2){
      if(data$Location[i]=='North'){
        nonmisik.Nfreq[j2]=nonmisik.Nfreq[j2]+1}
      nonmisik.Nfreq2[j2]=nonmisik.Nfreq2[j2]+1}
    else{
      if(nonmisik.Nfreq[j2]!=0){nonmisik.Nfreq[j2]=(nonmisik.Nfreq[j2]/nonmisik.Nfreq2[j2])}
      
      j2=j2+1
      if(data$Location[i]=='North'){
        nonmisik.Nfreq[j2]=nonmisik.Nfreq[j2]+1}
      nonmisik.Nfreq2[j2]=nonmisik.Nfreq2[j2]+1
    }
    temp2=data$Label[i]
    
  }
}
misik.Nfreq[22]<-misik.Nfreq[22]/misik.Nfreq2[22];nonmisik.Nfreq[67]<-nonmisik.Nfreq[67]/nonmisik.Nfreq2[67]

#preference
misik.Npref<-rep(0,22)
misik.Npref2<-rep(0,22)
nonmisik.Npref<-rep(0,67)
nonmisik.Npref2<-rep(0,67)
temp="1";temp2="2";j=1;j2=1;
for(i in (1:length(data$Label))){
  if(data$Label[i] %in% misik){
    if(data$Label[i]==temp){
      if(data$Location[i]=='North'){
        misik.Npref[j]=misik.Npref[j]+data$Rate[i]
      misik.Npref2[j]=misik.Npref2[j]+1}}
    else{
      if(misik.Npref[j]!=0){misik.Npref[j]=(misik.Npref[j]/misik.Npref2[j])}
      j=j+1
      if(data$Location[i]=='North'){
        misik.Npref[j]=misik.Npref[j]+data$Rate[i]
      misik.Npref2[j]=misik.Npref2[j]+1}
    }
    temp=data$Label[i]
  }
  else{
    if(data$Label[i]==temp2){
      if(data$Location[i]=='North'){
        nonmisik.Npref[j2]=nonmisik.Npref[j2]+data$Rate[i]
      nonmisik.Npref2[j2]=nonmisik.Npref2[j2]+1}}
    else{
      if(nonmisik.Npref[j2]!=0){nonmisik.Npref[j2]=(nonmisik.Npref[j2]/nonmisik.Npref2[j2])}
      
      j2=j2+1
      if(data$Location[i]=='North'){
        nonmisik.Npref[j2]=nonmisik.Npref[j2]+data$Rate[i]
      nonmisik.Npref2[j2]=nonmisik.Npref2[j2]+1}
    }
    temp2=data$Label[i]
    
  }
}
misik.Npref[22]<-misik.Npref[22]/misik.Npref2[22];nonmisik.Npref[67]<-nonmisik.Npref[67]/nonmisik.Npref2[67]


#Q2) Bindo-Manjokdo
ind.freq<-rep(0,89)
ind.freq2<-rep(0,89)
ind.rate<-rep(0,89)

temp="1";j=1;
for(i in (1:length(data$Label))){

    if(data$Label[i]==temp){
      if(data$Location[i]=='North'){
        ind.freq[j]=ind.freq[j]+1
        ind.rate[j]=ind.rate[j]+data$Rate[i]
        ind.freq2[j]=ind.freq2[j]+1
        }
    
      }

    else{
      if(ind.freq[j]!=0){ind.freq[j]=(ind.freq[j]/ind.freq2[j])}
      if(ind.rate[j]!=0){cat(j,ind.rate[j],ind.freq2[j],"\n")
        ind.rate[j]=(ind.rate[j]/ind.freq2[j])}
      j=j+1
      if(data$Location[i]=='North'){
        ind.freq[j]=ind.freq[j]+1
        ind.rate[j]=ind.rate[j]+data$Rate[i]
      }
      ind.freq2[j]=ind.freq2[j]+1
    temp=data$Label[i]
    }
  
}

ind.freq[89]<-ind.freq[89]/ind.freq2[89];ind.rate[89]<-ind.rate[89]/ind.freq2[89]

#Q3) Stress-Satisfaction
stress<-rep(0,89)
stress2<-rep(0,89)
j=1;
temp=data$Label[1]

for(i in (1:length(data$Label))){
  if(data$Label[i]==temp){stress[j]=stress[j]+data$stress[i]
  stress2[j]=stress2[j]+1}
  else{
    stress[j]=(stress[j]/stress2[j])
    j=j+1
    stress[j]=stress[j]+data$stress[i]
    stress2[j]=stress2[j]+1
  }
  temp=data$Label[i]
}
stress[89]=(stress[89]/stress2[89])

highstress<-which(stress>summary(stress)[5][[1]])


highstress.Nfreq<-rep(0,22)
highstress.Nfreq2<-rep(0,22)
nonhighstress.Nfreq<-rep(0,67)
nonhighstress.Nfreq2<-rep(0,67)
temp="10";temp2="1";j=1;j2=1;
for(i in (1:length(data$Label))){
  if(data$Label[i] %in% highstress){
    if(data$Label[i]==temp){
      if(data$Location[i]=='North'){
        highstress.Nfreq[j]=highstress.Nfreq[j]+1}
      highstress.Nfreq2[j]=highstress.Nfreq2[j]+1}
    else{
      if(highstress.Nfreq[j]!=0){highstress.Nfreq[j]=(highstress.Nfreq[j]/highstress.Nfreq2[j])}
      j=j+1
      if(data$Location[i]=='North'){
        highstress.Nfreq[j]=highstress.Nfreq[j]+1}
      highstress.Nfreq2[j]=highstress.Nfreq2[j]+1
    }
    temp=data$Label[i]
  }
  else{
    if(data$Label[i]==temp2){
      if(data$Location[i]=='North'){
        nonhighstress.Nfreq[j2]=nonhighstress.Nfreq[j2]+1}
      nonhighstress.Nfreq2[j2]=nonhighstress.Nfreq2[j2]+1}
    else{
      if(nonhighstress.Nfreq[j2]!=0){nonhighstress.Nfreq[j2]=(nonhighstress.Nfreq[j2]/nonhighstress.Nfreq2[j2])}
      
      j2=j2+1
      if(data$Location[i]=='North'){
        nonhighstress.Nfreq[j2]=nonhighstress.Nfreq[j2]+1}
      nonhighstress.Nfreq2[j2]=nonhighstress.Nfreq2[j2]+1
    }
    temp2=data$Label[i]
    
  }
}
highstress.Nfreq[22]<-highstress.Nfreq[22]/highstress.Nfreq2[22];nonhighstress.Nfreq[67]<-nonhighstress.Nfreq[67]/nonhighstress.Nfreq2[67]

#preference
highstress.Npref<-rep(0,22)
highstress.Npref2<-rep(0,22)
nonhighstress.Npref<-rep(0,67)
nonhighstress.Npref2<-rep(0,67)
temp="10";temp2="1";j=1;j2=1;
for(i in (1:length(data$Label))){
  if(data$Label[i] %in% highstress){
    if(data$Label[i]==temp){
      if(data$Location[i]=='North'){
        highstress.Npref[j]=highstress.Npref[j]+data$Rate[i]
        highstress.Npref2[j]=highstress.Npref2[j]+1}}
    else{
      if(highstress.Npref[j]!=0){highstress.Npref[j]=(highstress.Npref[j]/highstress.Npref2[j])}
      j=j+1
      if(data$Location[i]=='North'){
        highstress.Npref[j]=highstress.Npref[j]+data$Rate[i]
        highstress.Npref2[j]=highstress.Npref2[j]+1}
    }
    temp=data$Label[i]
  }
  else{
    if(data$Label[i]==temp2){
      if(data$Location[i]=='North'){
        nonhighstress.Npref[j2]=nonhighstress.Npref[j2]+data$Rate[i]
        nonhighstress.Npref2[j2]=nonhighstress.Npref2[j2]+1}}
    else{
      if(nonhighstress.Npref[j2]!=0){nonhighstress.Npref[j2]=(nonhighstress.Npref[j2]/nonhighstress.Npref2[j2])}
      
      j2=j2+1
      if(data$Location[i]=='North'){
        nonhighstress.Npref[j2]=nonhighstress.Npref[j2]+data$Rate[i]
        nonhighstress.Npref2[j2]=nonhighstress.Npref2[j2]+1}
    }
    temp2=data$Label[i]
    
  }
}
highstress.Npref[22]<-highstress.Npref[22]/highstress.Npref2[22];nonhighstress.Npref[67]<-nonhighstress.Npref[67]/nonhighstress.Npref2[67]

# Create a data frame with different lengths
combined_data <- data.frame(misik.Nfreq = c(misik.Nfreq, rep(NA, 89- length(misik.Nfreq))),
                            misik.Npref = c(misik.Npref, rep(NA, 89- length(misik.Npref))),
                            nonmisik.Nfreq = c(nonmisik.Nfreq, rep(NA, 89- length(nonmisik.Nfreq))),
                            nonmisik.Npref = c(nonmisik.Npref, rep(NA, 89- length(nonmisik.Npref))),
                            ind.freq = c(ind.freq, rep(NA, 89- length(ind.freq))),
                            ind.rate = c(ind.rate, rep(NA, 89- length(ind.rate))),
                            highstress.Nfreq = c(highstress.Nfreq, rep(NA, 89- length(highstress.Nfreq))),
                            highstress.Npref = c(highstress.Npref, rep(NA, 89- length(highstress.Npref))),
                            nonhighstress.Nfreq = c(nonhighstress.Nfreq, rep(NA, 89- length(nonhighstress.Nfreq))),
                            nonhighstress.Npref = c(nonhighstress.Npref, rep(NA, 89- length(nonhighstress.Npref)))
                            )

# Write the data frame to a CSV file
write.csv(combined_data, file = "C:/Users/GB Park/Documents/combined_data_2.csv", row.names = FALSE)
