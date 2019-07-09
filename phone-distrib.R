p1=0.25
p2=0.35

p2tel=0.15
p2teldif<-p1*p2
p2tel1=(p2tel-p2teldif)/2
p2tel2=(p2tel-p2teldif)/2
p1tel1<-p1-p2tel1-p2teldif
p1tel2<-p2-p2tel2-p2teldif
npers=10000

phones1<-rbinom(npers,1,p1tel1)
phones2<-rbinom(npers,1,p1tel2)
for(i in 1:npers) {
  if(phones1[i] == 1) {
    phones1[i] = phones1[i] + rbinom(1,1,p2tel1)
  }
  if(phones2[i] == 1) {
    phones2[i] = phones2[i] + rbinom(1,1,p2tel2)
  }
    
}

