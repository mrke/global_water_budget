#clear all
rm(list=ls(all=T))

#First calculate your Pwet term based on actual water loss measurements (either whole body, or head/body compartments)
#############################################################
###Enter your experimental data: if unavailable, enter NA
Et.reported<-0.032  #ml/hr, Total water loss rate
Ec.reported<-NA  #ml/hr, Cutaneous water loss rate
Er.reported<-NA  #ml/hr, Respiratory water loss rate
Tb<-25                 #C, core body temperature
Ts<-Tb+0.001            #C, skin surface temperature, if not known add 0.001 to Tb
Ta <- Tb+0.002          #C, ambient air temperature, if not known add 0.002 to Tb
RHex <- 100             #%, RH inside animal, assume 100%RH
RHin <- 5              #%, RH of ambient air 
mass<-229.24             #g, body mass
SA<-374.57           #cm2, surface area of animal, in not known use below equation from Benedict
#SA=10*mass^(2/3)
VO2<-6.738            #mlO2/h, Metabolic rate, if unknown use regression equation below (Anders Pough 85) 
#Ms<-1                #Ms is 0 for standard MR and 1 for resting MR
#VO2 <- 0.13*mass^0.8*10^(0.038 * Tb)*10^(0.14*Ms)
oee <- 16             #%, Oxygen extraction coefficient, if not known assume 16%
Vt <- NA              #ml/breath, Tidal volume
Br <- NA              #breaths/hr, Breathing rate
AP<-1013250           #air pressure in pa
Cp <- 1.00484         #J/g/K, specific heat of air
wind<-NA              #cm/s, not equal to flow rate, if only flow rate is known, can roughly estimate wind speed 
cross.animal<-3.14*(3.103/2)^2 #cm2, ave cross sectional area of animal, I am using ave of head width/height as diameter
cross.chamber<-3.14*(9/2)^2 #cm2, cross sectional area of chamber
flow<-150/60            #cm3/sec, flow rate in chamber
wind<-flow/(cross.chamber-cross.animal)
SAeye<-2*3.14*(0/2)^2     #cm2, surface area of eye while measurement was made, i am assuming cunninghami's closed eyes in chamber
eyecorrect<-0           #If compartment method used, was the eye excluded from the head compartment? Yes=1, no=0
headcorrect<-1          #If compartment method used, was the skin on the head included in the head compartment corrected? Yes=1, no=0

pwet=list(Et.reported=Et.reported,Ec.reported=Ec.reported,Er.reported=Er.reported,Tb=Tb,Ts=Ts,Ta=Ta,RHex=RHex,RHin=RHin,mass=mass,SA=SA,VO2=VO2,oee=oee,Vt=Vt,Br=Br,AP=AP,Cp=Cp,wind=wind,SAeye=SAeye)
source('find_pwet_function.R')
pwet_results<-find_pwet(pwet)
skinwet<-pwet_results[[10]] # %, percentage of total surface area acting like a free water surface for evaporation 
############################################################



#Alternatively you can experiment with different values of skin resistance, surface area of eye, and proportion of time spent with open eyes
############################################################
#Rs<-pwet_results[[5]]         #s/cm, skin resistance 
Rs<-700         #s/cm, skin resistance 
SAeye<-2*3.14*(0.44/2)^2      #cm2, surface area of eye, I used ratio of eye to SVL from photo (0.024) with ave SVL of our adult lizards (183.8mm) to get eye diameter
Popen<-1                    #proportion of active time spent with eyes open
#oee<- 12

pwet=list(Et.reported=Et.reported,Ec.reported=Ec.reported,Er.reported=Er.reported,Tb=Tb,Ts=Ts,Ta=Ta,RHex=RHex,RHin=RHin,mass=mass,SA=SA,VO2=VO2,oee=oee,Vt=Vt,Br=Br,AP=AP,Cp=Cp,wind=wind,SAeye=SAeye)
source('new_pwet_function.R')
new_pwet_results<-new_pwet(pwet)
skinwet<-new_pwet_results[[6]]
############################################################



#Next start water budget model
spatial<-"c:/global climate/" # place where climate input files are kept

library(ggplot2)
library(sp)
library(maps)
library(mapproj)
library(rasterVis) # Note: requires ggplot2

r = raster(ncol = 13, nrow = 10, xmn =115-1.5, xmx = 155-1.5, ymn = -40-1.5, ymx = -11-1.5)
#r = raster(ncol = 41, nrow = 30, xmn = 115, xmx = 155, ymn = -40, ymx = -11)
values(r) = 0

JanEtsum_cunn<-r
JulyEtsum_cunn<-r
JanEtact_cunn<-r
JulyEtact_cunn<-r
JanEtsumperact_cunn<-r
JulyEtsumperact_cunn<-r
#crs(r) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" FIND CORRECT CRS

Long <- rep(seq(from=115,to=151,by=3),by=10)
Lat <- rep(seq(from=-12,to=-39,by=-3),each=13)
#Long <- rep(seq(from=115,to=155,by=1),by=30)
#Lat <- rep(seq(from=-11,to=-40,by=-1),each=41)
coords <- as.data.frame(cbind(Long, Lat))

points <- coords
# turn this data frame into a Spatial Point object
coordinates(points) <- ~Long + Lat
#c(Mike's default point of Nyrippi,grid of AUS points)
sites<-cbind(coords)

#Graph of points:
spatial<-"c:/global climate/"
global_climate<-brick(paste(spatial,"global_climate.nc", sep= ""))

#FOR JAN
plot(global_climate[[37]],ylim=c(-44,-8),xlim=c(110,155))
points(sites)
#plot(mean(global_climate[[73]],global_climate[[85]]),ylim=c(-44,-8),xlim=c(110,155))

# RAINFALL <- CLIMATE[,1:12]
#    WNMAXX <- CLIMATE[,13:24]
#   WNMINN<-WNMAXX*0.1 # impose diurnal cycle
#  TMINN <- CLIMATE[,25:36]
#  TMAXX <- CLIMATE[,37:48]
# ALLMINTEMPS<-TMINN
#ALLMAXTEMPS<-TMAXX
# ALLTEMPS <- cbind(ALLMAXTEMPS,ALLMINTEMPS)
# CCMAXX <- 100-CLIMATE[,49:60]
# CCMINN<-CCMAXX
# RAINYDAYS <- CLIMATE[,61:72]
#  RHMINN <- CLIMATE[,73:84]
# RHMAXX <- CLIMATE[,85:96]

####################location loop starts###################################################
for (w in 1:length(sites[,1])){
  
  if (is.na(extract(global_climate[[4]],coords[w,]))==FALSE){
    points(x=sites[w,1],y=sites[w,2],pch=19,col="red")
    
######################### model modes ###########################################################
mac<-0 # choose mac (1) or pc (0) 
writecsv<-0 # make Fortran code write output as csv files
write_input<-0 # write csv files of final input to working directory? 1=yes, 0=no.
runshade<-1 # run the model twice, once for each shade level (1) or just for the first shade level (0)?
runmoist<-0 # run soil moisture model (0=no, 1=yes)?
snowmodel<-0 # run the snow model (0=no, 1=yes)? - note that this runs slower
basic<-0 # for use with a simplified demo script 
shore<-0 # include tide effects (if 0, an empty matrix of tide effects is created)
rungads<-1 # use the Global Aerosol Database?
#########################################################################################################

############## location and climatic data  ###################################
sitemethod <- 0 # 0=specified single site long/lat, 1=place name search using geodis (needs internet)
longlat<-c(sites[w,1],sites[w,2]) # type a long/lat here in decimal degrees, used if option 0 is chosen above
loc <- "Nyrripi Northern Territory, Australia" # type in a location here, used if option 1 is chosen above
timezone<-0 # if timezone=1 (needs internet), uses GNtimezone function in package geonames to correct to local time zone (excluding daylight saving correction)
timeinterval<-12 # number of time intervals to generate predictions for over a year (must be 12 <= x <=365)
nyears<-1 # number of years to run
#########################################################################################################

############# microclimate model parameters ################################
EC <- 0.0167238 # Eccenricity of the earth's orbit (current value 0.0167238, ranges between 0.0034 to 0.058)
RUF <- 0.004 # Roughness height (m), , e.g. sand is 0.05, grass may be 2.0, current allowed range: 0.001 (snow) - 2.0 cm.
SLE <- 0.96 # Substrate longwave IR emissivity (decimal %), typically close to 1
ERR <-2.5 # Integrator error for soil temperature calculations
DEP <- c(0., 2.5,  5.,  10.,  15.,  20.,  30.,  50.,  100.,  200.) # Soil nodes (cm) - keep spacing close near the surface, last value is where it is assumed that the soil temperature is at the annual mean air temperature
####Thcond <- rocks[1,w] # soil minerals thermal conductivity (W/mC) 
####Density <- rocks[2,w] # soil minerals density (kg/m3)
####SpecHeat <- rocks[3,w] # soil minerals specific heat (J/kg-K)
####BulkDensity <- rocks[4,w] # soil bulk density (kg/m3)
Thcond <- 2.5 # soil minerals thermal conductivity (W/mC)
Density <- 2560. # soil minerals density (kg/m3)
SpecHeat <- 870. # soil minerals specific heat (J/kg-K)
BulkDensity <- 2560. # soil bulk density (kg/m3) 
cap<-0 # organic cap present on soil surface? (cap has lower conductivity - 0.2 W/mC - and higher specific heat 1920 J/kg-K)
Clay <- 20 # clay content for matric potential calculations (%)
SoilMoist<-0.2
SoilMoist_Init <- rep(0.2,10) # fractional soil moisture (decimal %)
#  soil moisture parameters found in Table 9.1 in Campbell and Norman, 1995 (Environmental Biophysics)
####soiltype<-soils[w]
soiltype<-7 #clay loam
CampNormTbl9_1<-read.csv('../micro_global/CampNormTbl9_1.csv')
fieldcap<-CampNormTbl9_1[soiltype,7] # field capacity, mm
wilting<-CampNormTbl9_1[soiltype,8]  # use value from digital atlas of Australian soils # wilting point, mm
PE<-rep(CampNormTbl9_1[soiltype,4],19) #air entry potential J/kg 
KS<-rep(CampNormTbl9_1[soiltype,6],19) #saturated conductivity, kg s/m3
BB<-rep(CampNormTbl9_1[soiltype,5],19) #soil 'b' parameter
BD<-rep(1.3,19) # soil bulk density, Mg/m3
L<-c(0,0,8.18990859,7.991299442,7.796891252,7.420411664,7.059944542,6.385001059,5.768074989,4.816673431,4.0121088,1.833554792,0.946862989,0.635260544,0.804575,0.43525621,0.366052856,0,0)*10000
LAI<-0.1 # leaf area index, used to partition traspiration/evaporation from PET
rainmult<-1 # rainfall multiplier to impose catchment
maxpool<-10000 # max depth for water pooling on the surface, mm (to account for runoff)
evenrain<-0 # spread daily rainfall evenly across 24hrs (1) or one event at midnight (2)
REFL<-0.10 # soil reflectance (decimal %)
maxpool<-200000#6 # max depth for water pooling on the surface, mm (to account for runoff)
slope<-0. # slope (degrees, range 0-90)
aspect<-180. # aspect (degrees, 0 = North, range 0-360)
hori<-rep(0,24) # enter the horizon angles (degrees) so that they go from 0 degrees azimuth (north) clockwise in 15 degree intervals
PCTWET<-0. # percentage of surface area acting as a free water surface (%)
CMH2O <- 1. # precipitable cm H2O in air column, 0.1 = VERY DRY; 1.0 = MOIST AIR CONDITIONS; 2.0 = HUMID, TROPICAL CONDITIONS (note this is for the whole atmospheric profile, not just near the ground)  
TIMAXS <- c(1.0, 1.0, 0.0, 0.0)   # Time of Maximums for Air Wind RelHum Cloud (h), air & Wind max's relative to solar noon, humidity and cloud cover max's relative to sunrise      														
TIMINS <- c(0.0, 0.0, 1.0, 1.0)   # Time of Minimums for Air Wind RelHum Cloud (h), air & Wind min's relative to sunrise, humidity and cloud cover min's relative to solar noon
minshade<-0. # minimum available shade (%)
maxshade<-90. # maximum available shade (%)
runshade<-1. # run the model twice, once for each shade level (1) or just for the first shade level (0)?
grasshade<-0 # this drives min shade value by the relative soil moisture multiplied by the maxshade parameter, above
Usrhyt <- 1# local height (cm) at which air temperature, relative humidity and wind speed calculatinos will be made 
snowtemp<-1.5 # temperature at which precipitation falls as snow (used for snow model)
snowdens<-0.3 # snow density (mg/m3)
snowmelt<-0.5 # proportion of calculated snowmelt that doesn't refreeze
undercatch<-1. # undercatch multipier for converting rainfall to snow
rainmelt<-0.0125 # paramter in equation that melts snow with rainfall as a function of air temp
####I added from micro_global's microclimate test file
densfun<-c(0,0) # slope and intercept of linear model of snow density as a function of day of year - if it is c(0,0) then fixed density used

# run the model
curdir<-getwd()
setwd('../micro_global/')
niche<-list(SoilMoist=SoilMoist,writecsv=writecsv,basic=basic,shore=shore,L,LAI=LAI,evenrain=evenrain,runmoist=runmoist,maxpool=maxpool,PE=PE,KS=KS,BB=BB,BD=BD,loc=loc,timeinterval=timeinterval,nyears=nyears,RUF=RUF,SLE=SLE,ERR=ERR,DEP=DEP,Thcond=Thcond,Density=Density,SpecHeat=SpecHeat,BulkDensity=BulkDensity,Clay=Clay,CMH2O=CMH2O,TIMAXS=TIMAXS,TIMINS=TIMINS,minshade=minshade,maxshade=maxshade,Usrhyt=Usrhyt,REFL=REFL,slope=slope,aspect=aspect,hori=hori,rungads=rungads,cap=cap,write_input=write_input,spatial=spatial,snowmodel=snowmodel,snowtemp=snowtemp,snowdens=snowdens,snowmelt=snowmelt,undercatch=undercatch,rainmult=rainmult,rainmelt=rainmelt,runshade=runshade,mac=mac)
source('NicheMapR_Setup_micro.R')
nicheout<-NicheMapR(niche)
setwd(curdir)

# get output
dim<-nicheout$dim
metout<-as.matrix(nicheout$metout[1:(dim*24),]) # above ground microclimatic conditions, min shade
shadmet<-as.matrix(nicheout$shadmet[1:(dim*24),]) # above ground microclimatic conditions, max shade
soil<-as.matrix(nicheout$soil[1:(dim*24),]) # soil temperatures, minimum shade
shadsoil<-as.matrix(nicheout$shadsoil[1:(dim*24),]) # soil temperatures, maximum shade
soilmoist<-as.matrix(nicheout$soilmoist[1:(dim*24),]) # soil water content, minimum shade
shadmoist<-as.matrix(nicheout$shadmoist[1:(dim*24),]) # soil water content, maximum shade
humid<-as.matrix(nicheout$humid[1:(dim*24),]) # soil humidity, minimum shade
shadhumid<-as.matrix(nicheout$shadhumid[1:(dim*24),]) # soil humidity, maximum shade
soilpot<-as.matrix(nicheout$soilpot[1:(dim*24),]) # soil water potential, minimum shade
shadpot<-as.matrix(nicheout$shadpot[1:(dim*24),]) # soil water potential, maximum shade
rainfall<-as.matrix(nicheout$RAINFALL)
MAXSHADES<-as.matrix(nicheout$MAXSHADES)
elev<-as.numeric(nicheout$ALTT)
REFL<-as.numeric(nicheout$REFL)
longlat<-as.matrix(nicheout$longlat)
ectoin<-as.matrix(rbind(elev,REFL,longlat,0,0,1990,1990+nyears-1))
wetlandTemps=matrix(data = 0., nrow = 24*dim, ncol = 1)
wetlandDepths=matrix(data = 0., nrow = 24*dim, ncol = 1)
RAINFALL<-rainfall


library(lattice)

# 
# # write output to csv files for possibly reading in by ectotherm model
# write.csv(metout,'metout.csv')
# write.csv(shadmet,'shadmet.csv')
# write.csv(soil,'soil.csv')
# write.csv(shadsoil,'shadsoil.csv')
# write.csv(soilmoist,'soilmoist.csv')
# write.csv(shadmoist,'shadmoist.csv')
# write.csv(humid,'humid.csv')
# write.csv(shadhumid,'shadhumid.csv')
# write.csv(soilpot,'soilpot.csv')
# write.csv(shadpot,'shadpot.csv')
# write.csv(rainfall,'rainfall.csv')
# write.csv(ectoin,'ectoin.csv')
# write.csv(DEP,'DEP.csv')
# write.csv(MAXSHADES,'MAXSHADES.csv')

microin<-"none" # directory where the microclimate model outputs are (empty if in present directory)

# simulation settings
mac<-0 # choose mac (1) or pc (0) 
live<-1 # live (metabolism/behavoiur) or dead animal?

# habitat settings
minshade<-0. # minimum available shade (percent)
maxshade<-90. # maximum available shade (percent)

# morphological traits
# 'lometry' determines whether standard or custom shapes/surface area/volume relationships are used.
# 0=plate,1=cyl,2=ellips,3=lizard (desert iguana),4=frog (leopard frog),
lometry<-3 # organism shape (see above)
ABSMAX<-0.866 # decimal %, maximum solar absorptivity
ABSMIN<-0.866 # decimal %, maximum solar absorptivity
ptcond<-0.15 # decimal % of surface contacting the substrate


# physiological traits ####I have added my E. Striata grad and wr data, and mr/et to get pwet
####TMAXPR<-therms[1,w] # degrees C, voluntary thermal maximum (upper body temperature for foraging and also burrow depth selection) # Licht 1966 thermal gradient
####TMINPR<-therms[2,w] # degrees C, voluntary thermal minimum (lower body temperature for foraging) # Kearney Obs (PhD field trip)
####TBASK<-therms[4,w] #5 # degrees C, minimum basking temperature (14. deg C, Fraser 1985 thesis, min of A in Fig. 7.3)
####TEMERGE<-therms[5,w] # degrees C, temperature at which animal will move to a basking site
####ctmax<-therms[6,w]  # degrees C, critical thermal maximum (animal will die if ctkill = 1 and this threshold is exceeded)
####ctmin<-therms[7,w] # degrees C, critical thermal minimum (used by program to determine depth selected when inactive and burrowing)
####TPREF<-therms[3,w] # preferred body temperature (animal will attempt to regulate as close to this value as possible) (mean 31.9, range 29.4-34.3, Bennett, A.F. & John-Alder, H. (1986) Thermal Relations of Some Australian Skinks (Sauria: Scincidae). Copeia, 1986, 57-64.), mode in Pamula Fig. 3.14 around 33.5
TMAXPR<-41.1 # degrees C, voluntary thermal maximum (upper body temperature for foraging and also burrow depth selection) # Licht 1966 thermal gradient
TMINPR<-32.5 # degrees C, voluntary thermal minimum (lower body temperature for foraging) # Kearney Obs (PhD field trip)
TBASK<-22#5 # degrees C, minimum basking temperature (14. deg C, Fraser 1985 thesis, min of A in Fig. 7.3)
TEMERGE<-15 # degrees C, temperature at which animal will move to a basking site
ctmax<-42.0  # degrees C, critical thermal maximum (animal will die if ctkill = 1 and this threshold is exceeded)
ctmin<-5.9 # degrees C, critical thermal minimum (used by program to determine depth selected when inactive and burrowing)
TPREF<-33.8 # preferred body temperature (animal will attempt to regulate as close to this value as possible) (mean 31.9, range 29.4-34.3, Bennett, A.F. & John-Alder, H. (1986) Thermal Relations of Some Australian Skinks (Sauria: Scincidae). Copeia, 1986, 57-64.), mode in Pamula Fig. 3.14 around 33.5
#Pwet from actual Ec ranges from 0.1 to 1.2 in my database

# behavioural traits
dayact<-1 # diurnal activity allowed (1) or not (0)?
nocturn<-0 # nocturnal activity allowed (1) or not (0)?
crepus<-0 # crepuscular activity allowed (1) or not (0)?
burrow<-1 # shelter in burrow allowed (1) or not (0)?
shdburrow<-1 # choose if the animal's retreat is in the shade (1) or in the open (0)
mindepth<-2 # minimum depth (soil node) to which animal can retreat if burrowing
maxdepth<-8 # maximum depth (soil node) to which animal can retreat if burrowing
CkGrShad<-1 # shade seeking allowed (1) or not (0)?
climb<-0 # climbing to seek cooler habitats allowed (1) or not (0)?

# parameters for allometric model of respiration, for use in heat budget when DEB model is not
# run so that metabolic heat generation and respiratory water loss can be calculated.
# Metabolic rate, MR (ml O2/h, STP) at a given body mass (g) and body temperature, Tb (deg C)
# MR=MR1*M^MR2*10^(MR3*Tb) based on Eq. 2 from Andrews & Pough 1985. Physiol. Zool. 58:214-231
####amass<-masses[w] # g, mass of animal (used if the 'monthly' option is checked and DEB model is thus off)
amass<-243.1 # g, mass of animal (used if the 'monthly' option is checked and DEB model is thus off)
MR_1<-exp(-5.242)
MR_2<-1.29
MR_3<-0.044


#set up call to NicheMapR function
setwd('../ectotherm/')
niche<-list(mac=mac,microin=microin,minshade=minshade,maxshade=maxshade,lometry=lometry,ABSMAX=ABSMAX,ABSMIN=ABSMIN,ptcond=ptcond,ctmax=ctmax,ctmin=ctmin,TMAXPR=TMAXPR,TMINPR=TMINPR,TPREF=TPREF,skinwet=skinwet,dayact=dayact,nocturn=nocturn,crepus=crepus,burrow=burrow,CkGrShad=CkGrShad,climb=climb,amass=amass,MR_1=MR_1,MR_2=MR_2,MR_3=MR_3,maxdepth=maxdepth,mindepth=mindepth,TBASK=TBASK,TEMERGE=TEMERGE)
source('NicheMapR_Setup_ecto_basic.R')
nicheout<-tryCatch(NicheMapR_ecto(niche), error = function() next) 
setwd(curdir)


# retrieve output
metout<-as.data.frame(metout)
shadmet<-as.data.frame(shadmet)
soil<-as.data.frame(soil)
shadsoil<-as.data.frame(shadsoil)
rainfall<-as.data.frame(rainfall)
nyears<-nicheout$nyears
environ<-as.data.frame(nicheout$environ)
enbal<-as.data.frame(nicheout$enbal)
masbal<-as.data.frame(nicheout$masbal)
# append dates
if(timeinterval==365){
tzone<-paste("Etc/GMT-",10,sep="") # doing it this way ignores daylight savings!
dates<-seq(ISOdate(2014,1,1,tz=tzone)-3600*12, ISOdate((2014+nyears),1,1,tz=tzone)-3600*13, by="hours")
dates<-subset(dates, format(dates, "%m/%d")!= "02/29") # remove leap years
dates2<-seq(ISOdate(2014,1,1,tz=tzone)-3600*12, ISOdate((2014+nyears),1,1,tz=tzone)-3600*13, by="days") 
dates2<-subset(dates2, format(dates2, "%m/%d")!= "02/29") # remove leap years
}else{
dates<-environ$DAY+environ$TIME/24-1
dates2<-seq(1,timeinterval,1)
}
environ<-cbind(dates,environ)
masbal<-cbind(dates,masbal)
enbal<-cbind(dates,enbal)
soil<-cbind(dates,soil)
metout<-cbind(dates,metout)
shadsoil<-cbind(dates,shadsoil)
shadmet<-cbind(dates,shadmet)
rainfall<-as.data.frame(cbind(dates2,rainfall))
colnames(rainfall)<-c("dates","rainfall")

############### plot results ######################
library(lattice) # package used for 'xyplot'
juldays<-c(15.,46.,74.,105.,135.,166.,196.,227.,258.,288.,319.,349.) # middle day of each month

####with(environ, plot(TC~dates,ylim=c(-20,70),type = "l"))
####with(environ, points(ACT*5~dates,type = "l",col="orange"))
####with(environ, points(SHADE/10~dates,type = "l",col="green"))
####with(environ, points(DEP/10~dates,type = "l",col="brown"))
#with(metout, points(TAREF~dates,type = "l",col="light blue"))
####abline(TMAXPR,0,lty=2,col='red')
####abline(TMINPR,0,lty=2,col='blue')

####with(masbal,plot(H2OCut_g~dates,ylim=c(0,max(masbal$H2OCut_g)),type = "l",col="blue"))
####with(masbal,points(H2OResp_g~dates,type = "l",col="red"))

forage<-subset(environ,ACT==2)
bask<-subset(environ,ACT==1)
night<-subset(metout,ZEN==90)
day<-subset(metout,ZEN!=90)
####with(night,plot(TIME/60~JULDAY,pch=15,cex=2,col='dark blue'))
####with(forage,points((TIME-1)~JULDAY,pch=15,cex=2,col='orange'))
####with(bask,points((TIME-1)~JULDAY,pch=15,cex=2,col='light blue'))

####My raster output files
points(x=sites[w,1],y=sites[w,2],pch=19,col="green",cex=1)

JanEtsum_cunn[w]<-sum(masbal$H2OEvap_g[which(masbal$JULDAY==15)])/amass
JulyEtsum_cunn[w]<-sum(masbal$H2OEvap_g[which(masbal$JULDAY==196)])/amass

environ2<-environ
environ2$ACT[environ2$ACT==2]<-1
JanEtact_cunn[w]<-sum(environ2$ACT[which(environ2$JULDAY==15)])
JulyEtact_cunn[w]<-sum(environ2$ACT[which(environ2$JULDAY==196)])

JanEtsumperact_cunn[w]<-JanEtsum_cunn[w]/JanEtact_cunn[w]
JulyEtsumperact_cunn[w]<-JulyEtsum_cunn[w]/ JulyEtact_cunn[w]



  }#end if over land statement
}#end w loop


# cunningham_eopen<-brick(c(JanEtsum_cunn,JulyEtsum_cunn,JanEtact_cunn,JulyEtact_cunn,JanEtsumperact_cunn,JulyEtsumperact_cunn))
# writeRaster(cunningham_eopen, filename="cunningham_eopen.grd", bandorder='BIL', overwrite=TRUE)
cunningham_rs700<-brick(c(JanEtsum_cunn,JulyEtsum_cunn,JanEtact_cunn,JulyEtact_cunn,JanEtsumperact_cunn,JulyEtsumperact_cunn))
writeRaster(cunningham_rs700, filename="cunningham_rs700.grd", bandorder='BIL', overwrite=TRUE)
