### RANDOM LIGHTFLECK GENERATOR ###

# Created by: Elias Kaiser in June 2019

# This program first generates a continously changing baseline light intensity  
 # (which is actually the maximum light intensity in a given moment during 
 # the photoperiod) in the form of a sine wave, and subsequently replaces this 
 # by lower light intensities, at random picking timing, duration and 
 # relative intensity of any specific decrease in intensity.
 # Next, this sequence is saved in a number of .xml files that each contain max.
  # 90 segments. For example, in a 12h photoperiod with a 1 min time steps (720
  # steps), 8 programs are generated

 # Constraints/Warnings:
 # Make sure total program doesnt run for >24h/day. This is possible computationally
  # (no errors are shown), but will generate nonsensical .xml file names that cannot be
  # interpreted by the LED chamber program

library(stringr)

### PARAMETERS (to be defined by user)
  day <- 20190621 # date the program is written for (Format: YYYYMMDD)
  start.day <- 7 # start of photoperiod (hours)
  daylength <- 4  # length of photoperiod (hours)
  time.step <- 1 # length of time step (min) (only use numbers that 'cleanly' divide 60)
  baseline.max.int <- 960   # peak light intensity during photoperiod (µmol m-2 s-1)
  avg.intensity <- 250  # average light intensity per day (µmol m-2 s-1)
  lightfl.max.ratio <- .95  # maximum intensity during lightfleck (multiple of baseline)
  lightfl.min.ratio <- .05  # minimum intensity during lightfleck (multiple of baseline)
  lightfl.max.dur <- 1  # maximum duration of lightfleck (min)
  # (duration >5 min not recommended, this tends to reduce mean(baseline.int2))
  lightfl.min.dur <- 1  # minimum duration of lightfleck (min)

### GENERATION OF LIGHTFLECKS
 # Generate a vector of length=daylength (min) and fill it with sinusoidal 
   # baseline intensities
  day.min<-1:(daylength*(60/time.step))
  t.start<-start.day/24
  t.end<-(start.day+daylength)/24
  t.seq<-(start.day/24)+(day.min/(24*(60/time.step)))
  baseline.int=baseline.max.int*sin((pi/(daylength/24))*(t.seq-t.start))

# While loop that keeps adding lightflecks to the diurnal light intensity pattern 
  # until avg.intensity has been reached
light.loop<-while(mean(baseline.int)>avg.intensity){
  r.time<-sample(day.min)[1]
  r.dur<-sample(seq(lightfl.min.dur,lightfl.max.dur))[1]
  r.int<-sample(seq(lightfl.min.ratio,lightfl.max.ratio,by=0.0005))[1]
  lightfl<-rep(baseline.int[r.time]*r.int,times=r.dur)
  baseline.int2<-replace(baseline.int,list=c(r.time:(r.time+length(lightfl)-1)),
                         values=lightfl)
  if (length(baseline.int2)>length(baseline.int)) {
    baseline.int<-baseline.int
  } else {
    baseline.int<-baseline.int2}}

# Translation of baseline.int into lamp settings (lamp.set)
# Parameters that describe the polynomial relationship between light intensity (x) and
  # lamp percentage (y) (y=a+b*x+c*x2)

# Parameters for white light ('Weiss') in phytotron K10, MPI Potsdam-Golm
  a=-0.3738532650
  b=0.0701677651
  c=0.0000350849

  lamp.set<-as.integer(a+b*baseline.int+c*(baseline.int^2))
  plot(lamp.set,type="l")

# SAVE RESULTS TO .XML FILES THAT CAN BE READ BY CLIMATE CHAMBER PROGRAM      
# Put lamp.set values into .xml segments
  # Blueprints .xml segments
  segment1<-as.character('<Ramp Name="Weiss" Target="0.0" Type="Step"/>')
  segment2<-as.character('
                          <Segment Msec="60000" Balance="true">
                          <Ramp Name="Temp" Target="16.0" Type="Step"/>
                          <Ramp Name="Feuchte" Target="75.0" Type="Step"/>
                         <Ramp Name="Licht" Target="0.0" Type="RampFromSp"/>
                         <Ramp Name="Weiss" Target="0.0" Type="Step"/>
                         <Ramp Name="Rot630" Target="0.0" Type="RampFromSp"/>
                         <Ramp Name="Rot660" Target="0.0" Type="Step"/>
                         <Ramp Name="Amber" Target="0.0" Type="RampFromSp"/>
                         <Ramp Name="Blau" Target="0.0" Type="Step"/>
                         <Ramp Name="UV370" Target="0.0" Type="RampFromSp"/>
                         <Ramp Name="UV400" Target="0.0" Type="RampFromSp"/>
                         <Ramp Name="Rot740" Target="0.0" Type="RampFromSp"/>
                         <Setting Channel="DA01" Value="0"/>
                         <Setting Channel="DA02" Value="0"/>
                         <Setting Channel="DA03" Value="0"/>
                         </Segment>')

  # Make one long vector of .xml segments with target settings for 'Weiss'
  # Adjust length of each segment ('Msec') to length of time.step
x1<-c(1:length(lamp.set)) 
x2<-replace(x1,list=c(1:length(lamp.set)),segment1)
x3<-replace(x1,list=c(1:length(lamp.set)),segment2)
x4<-replace(x1,list=c(1:length(lamp.set)),as.character(time.step*600))
x5<-as.character(lamp.set)
x6<-str_replace(x2,'0',x5)
x7<-str_replace(x3,'<Ramp Name="Weiss" Target="0.0" Type="Step"/>',x6)
x8<-str_replace(x7,'600',x4)

# Write segments to several files, each 90 segments long
  # Initial and final parts of each file: block1 and block2
block1<-as.character('<?xml version="1.0" encoding="UTF-8"?>
                     
                     <Program Name="TProgLED10_201902240700" Machine="LED1JCIPhyto(K10)" LastChange="2018-12-19 14:13:58">
                     <Comment><![CDATA[]]></Comment>
                     <Statistics>
                     <Parameter ProgDurationMsec="86400000"/>
                     <Parameter NumJobs="1"/>
                     <Parameter NumBlocks="1"/>
                     <Parameter NumSegments="xxx"/>
                     <Parameter NumLoops="0"/>
                     <Parameter NumWaitUntils="0"/>
                     <Parameter NumWaitClocks="0"/>
                     </Statistics>
                     <Job Type="Block" Id="17b6179e-b666-4db4-971e-6941a73576c4" Name="JobName" Cycles="1">
                     <Comment><![CDATA[]]></Comment>
                     <ShowInGraph/> ')
block2<-as.character('
</Job>
  </Program>')

# Alternate ending (for last program of a given day) that makes sure there's darkness until the start
 # the photoperiod the next day
end.segment<-as.character('
                          <Segment Msec="43140000" Balance="true">
                          <Ramp Name="Temp" Target="16.0" Type="Step"/>
                          <Ramp Name="Feuchte" Target="75.0" Type="Step"/>
                          <Ramp Name="Licht" Target="0.0" Type="RampFromSp"/>
                          <Ramp Name="Weiss" Target="0.0" Type="Step"/>
                          <Ramp Name="Rot630" Target="0.0" Type="RampFromSp"/>
                          <Ramp Name="Rot660" Target="0.0" Type="Step"/>
                          <Ramp Name="Amber" Target="0.0" Type="RampFromSp"/>
                          <Ramp Name="Blau" Target="0.0" Type="Step"/>
                          <Ramp Name="UV370" Target="0.0" Type="RampFromSp"/>
                          <Ramp Name="UV400" Target="0.0" Type="RampFromSp"/>
                          <Ramp Name="Rot740" Target="0.0" Type="RampFromSp"/>
                          <Setting Channel="DA01" Value="0"/>
                          <Setting Channel="DA02" Value="0"/>
                          <Setting Channel="DA03" Value="0"/>
                          </Segment>
                          </Job>
                          </Program>')

# Generate files with names readable to climate chamber program, save in working directory of R
y1<-0
file.vector<-c(1:ceiling((length(lamp.set))/90))

for (i in 1:length(file.vector)){
  num.segments<-(if(length(file.vector)!=file.vector[i]){90}else{(length(lamp.set)-
    (file.vector[i]-1)*90)})
  hours<- start.day+((y1*time.step)/60)
  hours1<-if(hours<10){paste('0',trunc(hours),sep="")}else{paste(trunc(hours),sep="")}
  minutes<-if(hours-trunc(hours)==0){paste('00',sep="")}else{paste((hours-trunc(hours))*60,sep="")}
  empty.file<-file(description=paste('TProgLED10_',day,hours1,minutes,'.prg',sep=""))
  block3<-str_replace(block1,'TProgLED10_201902240700',paste('TProgLED10_',day,hours1,minutes,sep=""))
  prog.length<-(if(length(file.vector)!=file.vector[i]){90*time.step*60}else{(length(lamp.set)-(file.vector[i]-1)*90)*time.step*60})
  block4<-str_replace(block3,'86400',as.character(prog.length))
  block5<-str_replace(block4,'xxx',as.character(num.segments+1))
  y2<-y1+num.segments
  length.end.segment<-(((1-t.end)+t.start)*24*3600)
  end.segment1<-str_replace(end.segment,'43140',as.character(length.end.segment))
  z1<-if(length(file.vector)==file.vector[i]){end.segment}else{block2}
  x9<-c(block5,x8[y1:y2],z1)
  cat(x9,file=empty.file)
  y1<-y2
  print(file.vector[i])}