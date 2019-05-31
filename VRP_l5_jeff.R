### Install 'VarroaPopWrapper' package (depends on 'devtools' package). Note that 'logs' and 'save_files' operators default to "FALSE" and must be set to "TRUE" to get log, in/output files
library("devtools", lib.loc="~/R/win-library/3.5")
library("VarroaPopWrapper", lib.loc="~/R/win-library/3.5")
library("abind", lib.loc="~/R/win-library/3.5")
library("truncnorm", lib.loc="~/R/win-library/3.5") #Required to use truncated normal function
library("EnvStats", lib.loc="~/R/win-library/3.5") #Required to use the truncated lognormal function

### Create folder and sub-folders for files created during MC analyses
dir.create("C:/Users/CDouglas/VarroaPop/jeff/", showWarnings = FALSE)
dir.create("C:/Users/CDouglas/VarroaPop/jeff/inputs_archive/", showWarnings = FALSE)
dir.create("C:/Users/CDouglas/VarroaPop/jeff/inputs_array/", showWarnings = FALSE)
dir.create("C:/Users/CDouglas/VarroaPop/jeff/outputs_archive/", showWarnings = FALSE)
dir.create("C:/Users/CDouglas/VarroaPop/jeff/outputs_array/", showWarnings = FALSE)

### Define weather input file
weather = "C:/Users/CDouglas/VarroaPop/inputs/15055_grid_35.875_lat.wea"

### Define location of VarroaPop exe file (more easily allows for use of updated versions of the model)
exe_file = "C:/Users/CDouglas/Documents/R/win-library/3.5/VarroaPopWrapper/varroapop_files/exe/3311/VarroaPop.exe"

### Define nectar/pollen residue levels (i.e. "contamination")
#npcontamination_level0 <- "C:/Users/CDouglas/VarroaPop/inputs/clo_feeding_0.csv"
#npcontamination_level1 <- "C:/Users/CDouglas/VarroaPop/inputs/clo_feeding_10.csv"
#npcontamination_level2 <- "C:/Users/CDouglas/VarroaPop/inputs/clo_feeding_20.csv"
#npcontamination_level3 <- "C:/Users/CDouglas/VarroaPop/inputs/clo_feeding_40.csv"
#npcontamination_level4 <- "C:/Users/CDouglas/VarroaPop/inputs/clo_feeding_80.csv"
npcontamination_level5 <- "C:/Users/CDouglas/VarroaPop/inputs/clo_feeding_160.csv"
#npcontamination_level6 <- "C:/Users/CDouglas/VarroaPop/inputs/clo_feeding_16000.csv"
necpolfile <- npcontamination_level5

### Define number of desired MC runs
nsims <- 1000
z <- nsims

### Setup 3d data storage array for model outputs (ie to store desired data points from VRP output files)
nrows <- 5
ncol <- 7
summ_outputs <- array(data = NA, c(nrows, ncol, nsims))
colnames(summ_outputs)<-c("adults","eggs","larvae","pupae","dadults","dfrgrs","necconc")
rownames(summ_outputs)<-c("CCA3","CCA4","CCA5","CCA6","CCA7")

### Setup 2d data storage array for model inputs (ie to store inputs for each VRP model run)
in_nrows <- 34
in_ncol <- nsims
summ_inputs <- array(data = NA, c(in_nrows, in_ncol))
rownames(summ_inputs)<-c("ait50", "adLD50","adslope","lLD50","lslope",
                         "ca13nectar","ca13pollen","ca1120nectar","ca1120pollen",
                         "ca410nectar","ca410pollen","cadnectar","cadpollen",
                         "cl4nectar","cl4pollen","cl5nectar","cl5pollen","cldnectar","cldpollen",
                         "frgrnectar","frgrpollen","nload","ntrips","pload","ptrips",
                         "initialnectar","initialpollen","frgrmaxprop","frgrlifespan","queenstrength",
                         "adults","pupae","eggs","larvae")
    
### Loop to do n Monte Carlo runs
for(i in 1:nsims){
  
  ### Define varying input parameters for VarroaPop model
  ait50 <- 25 #AIHalfLife
  adLD50 <- rtruncnorm(1, a=0.003, b=0.04069, mean=0.02008, sd=0.00808) #AIAdultLD50
  adslope <- rtruncnorm(1, a=1.25, b=8.63, mean=4.93, sd=2.19) #AIAdultSlope
  lLD50 <- rtruncnorm(1, a=0.0001, b=0.1, mean=0.04311, sd=0.02831) #AILarvaLD50
  lslope <- rtruncnorm(1, a=1.34, b=8.66, mean=5.07, sd=2.14) #AILarvaSlope
  ca13nectar <- 60 #runif(1, 20, 100) 
  ca13pollen <- 6.7 #runif(1, 2, 9.5) 
  ca1120nectar <- 60 #runif(1, 20, 100) 
  ca1120pollen <- 1.7 #runif(1, 0, 3) 
  ca410nectar <- 60 #runif(1, 20, 200) 
  ca410pollen <- 1.7 #runif(1, 2, 9.5) 
  cadnectar <- 225 #runif(1, 133, 337)
  cadpollen <- 0 #runif(1, 0, 1)
  cl4nectar <- 60 #runif(1, 0, 100) 
  cl4pollen <- 1.8 #runif(1, 0, 4)
  cl5nectar <- 120 #runif(1, 0, 150) 
  cl5pollen <- 3.6 #runif(1, 0, 6) 
  cldnectar <- 130 #runif(1, 0, 160)
  cldpollen <- 3.6 #runif(1, 0, 5)
  frgrnectar <- 292 #runif(1, 80.25, 321) # CForagerNectar
  frgrpollen <- 0 #runif(1, 8.75, 13) # CForagerPollen
  nload <- 30 #runif(1, 22, 51) # INectarLoad
  ntrips <- 17 #rlnormTrunc(1, meanlog=13, sdlog=3, min=1, max=17) # INectarTrips
  pload <- 15 #runif(1, 15, 35) #IPollenLoad
  ptrips <- 8 #rlnormTrunc(1, meanlog=10, sdlog=3, min=1, max=24) #IPollenTrips
  initialnectar <- 0 #rtruncnorm(1, a=11408, b=19571, mean=15490, sd=7101) #InitColNectar
  initialpollen <- 0 #rtruncnorm(1, a=1775, b=2921, mean=2348, sd=997) #InitColPollen
  frgrmaxprop <- 0.3 #runif(1, 0, 0.3) # ForagerMaxProp
  frgrlifespan <- 16 #rtruncnorm(1, a=11.97, b=15.9, mean=14.37, sd=1.07) # ICForagerLifespan
  queenstrength <- 4 #rtruncnorm(1, a=2.69, b=4.88, mean=3.87, sd=0.61) # RQQueenStrength
  adults <- rtruncnorm(1, a=16599, b=24113, mean=20356, sd=6530) #ICWorkerAdults Adjusted by 30% to account
            #for VRP's apportioning ~30% of this value to foragers
  pupae <- rtruncnorm(1, a=15507, b=19396, mean=17452, sd=3379) #ICWorkerBrood
  eggs <- rtruncnorm(1, a=4503, b=7904, mean=6203, sd=2959) #ICWorkerEggs
  larvae <- rtruncnorm(1, a=7549, b=11375, mean=9462, sd=3320) #ICWorkerLarvae
  pupaetoadults = "06/15/2014, 10/30/2014, 60" #BToAXiTion
  
  ### Run iteration of VRP Wrapper for negative control (i.e., level0)
  params = c(SimStart = "06/20/2014", SimEnd = "10/22/2014", AIHalfLife = ait50,
             AIAdultLD50 = adLD50, AIAdultSlope = adslope, 
             AILarvaLD50 = lLD50, AILarvaSlope = lslope,
             CA13Nectar = ca13nectar, CA13Pollen = ca13pollen,
             CA1120Nectar = ca1120nectar, CA1120Pollen = ca1120pollen,
             CA410Nectar = ca410nectar, CA410Pollen = ca410pollen,
             CADNectar = cadnectar, CADPollen = cadpollen,
             CL4Nectar = cl4nectar, CL4Pollen = cl4pollen,
             CL5Nectar = cl5nectar, CL5Pollen = cl5pollen,
             CLDNectar = cldnectar, CLDPollen = cldpollen,
             ForagerMaxProp = frgrmaxprop, ICForagerLifespan = frgrlifespan,
             RQQueenStrength = queenstrength,
             INectarLoad = nload, INectarTrips = ntrips,
             IPollenLoad = pload, IPollenTrips = ptrips,
             InitColNectar = initialnectar, InitColPollen = initialpollen,
             CForagerNectar = frgrnectar, CForagerPollen = frgrpollen,
             ICWorkerAdults = adults, ICWorkerBrood = pupae,
             ICWorkerEggs = eggs, ICWorkerLarvae = larvae,
             BToAXitionEnable = TRUE, BToAXition = pupaetoadults,
             NecPolFileEnable = TRUE, NecPolFileName = necpolfile)
  
  summ_inputs[,i] <- c(ait50, adLD50, adslope, lLD50, lslope, 
             ca13nectar, ca13pollen, ca1120nectar, ca1120pollen,
             ca410nectar, ca410pollen, cadnectar, cadpollen,
             cl4nectar, cl4pollen, cl5nectar, cl5pollen, cldnectar, cldpollen,
             frgrnectar, frgrpollen,
             nload, ntrips, pload, ptrips,
             initialnectar, initialpollen,
             frgrmaxprop, frgrlifespan, queenstrength,
             adults, pupae, eggs, larvae)
  
  # These input/output files will be used for population of the storage array
  in_path <- "C:/Users/CDouglas/VarroaPop/jeff/inputs_array/"
  in_file <- "inputs.txt"
  out_path <- "C:/Users/CDouglas/VarroaPop/jeff/outputs_array/"
  out_file <- "outputs.txt"
  output <- RunVarroaPopLocal(parameters = params, exe_file = exe_file, weather_file = weather, 
                              logs = TRUE, save_files = TRUE, out_file = out_file, 
                              out_path = out_path, in_file = in_file, in_path = in_path)
  
  # These input/output files will be for archiving and allow for posthoc inspection and analyses of each run (if necessary)
  sim_in_path <- "C:/Users/CDouglas/VarroaPop/jeff/inputs_archive/"
  sim_in_file <- paste("inputs_", i, ".txt", sep="")
  sim_out_path <- "C:/Users/CDouglas/VarroaPop/jeff/outputs_archive/"
  sim_out_file <- paste("outputs_", i, ".txt", sep="")
  sim_output <- RunVarroaPopLocal(parameters = params, exe_file = exe_file, weather_file = weather, logs = TRUE, save_files = TRUE, out_file = sim_out_file, 
                              out_path = sim_out_path, in_file = sim_in_file, in_path = sim_in_path)
  
    ### Pull back in the results to populate the data array
    outputs <- read.table("C:/Users/CDouglas/VarroaPop/jeff/outputs_array/outputs.txt", skip = 7)
    
      ### Populate the storage array ('summ_outputs') with simulation results
      summ_outputs[,,i][c(1,2,3,4,5),1]<-outputs[c(1,28,49,83,125),4]
      summ_outputs[,,i][c(1,2,3,4,5),2]<-outputs[c(1,28,49,83,125),11]
      summ_outputs[,,i][c(1,2,3,4,5),3]<-outputs[c(1,28,49,83,125),9]
      summ_outputs[,,i][c(1,2,3,4,5),4]<-outputs[c(1,28,49,83,125),7]
      summ_outputs[,,i][c(1,2,3,4,5),5]<-outputs[c(1,28,49,83,125),26]
      summ_outputs[,,i][c(1,2,3,4,5),6]<-outputs[c(1,28,49,83,125),27]
      summ_outputs[,,i][c(1,2,3,4,5),7]<-outputs[c(1,28,49,83,125),22]
      
} # End i loop

write.csv(summ_inputs, "C:/Users/CDouglas/VarroaPop/jeff/inputs_archive/summ_inputs.csv")
write.csv(summ_outputs, "C:/Users/CDouglas/VarroaPop/jeff/outputs_archive/summ_outputs.csv")

### Run descriptive statistics for VRP output files - means are stored in "summ_mean" output file; standard deviations are stored in "summ_sd"; means +/- 95% CIs are stored in "summ_stats"

summ_mean <- array(data = NA, c(nrows, ncol))
colnames(summ_mean)<-c("adults_mean","eggs_mean","larvae_mean","pupae_mean","dadults_mean","dfrgrs_mean","necconc_mean")
rownames(summ_mean)<-c("CCA3","CCA4","CCA5","CCA6","CCA7")
summ_sd <- array(data = NA, c(nrows, ncol))
colnames(summ_sd)<-c("adults_sd","eggs_sd","larvae_sd","pupae_sd","dadults_mean","dfrgrs_mean","necconc_mean")
rownames(summ_sd)<-c("CCA3","CCA4","CCA5","CCA6","CCA7")
ci <- array(data = NA,c(nrows,ncol))
low <- array(data = NA,c(nrows,ncol))
up <- array(data = NA,c(nrows,ncol))
colnames(low)<-c("adults_low","eggs_low","larvae_low","pupae_low","dadults_low","dfrgrs_low","necconc_low")
colnames(up)<-c("adults_up","eggs_up","larvae_up","pupae_up","dadults_up","dfrgrs_up","necconc_up")

for (i in 1:nrows) {
  for (j in 1:ncol) {
    
    summ_sd[i,j] <- sd(summ_outputs[i,j,1:nsims]) 
    summ_mean[i,j] <- mean(summ_outputs[i,j,1:nsims])
    summ_msd <- cbind(summ_mean, summ_sd)
    colnames(summ_msd)<-c("adults_mean","eggs_mean","larvae_mean","pupae_mean","dadults_mean","dfrgrs_mean","necconc_mean",
                          "adults_sd","eggs_sd","larvae_sd","pupae_sd","dadults_sd","dfrgrs_sd","necconc_sd")
    
    ci[i,j] <- qnorm(.975)*(sd(summ_outputs[i,j,1:nsims])/sqrt(nsims))
    up[i,j] <- (mean(summ_outputs[i,j,1:nsims]))+(qnorm(.975)*(sd(summ_outputs[i,j,1:nsims])/sqrt(nsims)))
    low[i,j] <- (mean(summ_outputs[i,j,1:nsims]))-(qnorm(.975)*(sd(summ_outputs[i,j,1:nsims])/sqrt(nsims)))
    5151
    summ_mci <- abind(summ_mean,low,up)
    colnames(summ_mci)<-c("adults_mean","eggs_mean","larvae_mean","pupae_mean","dadults_mean","dfrgrs_mean","necconc_mean",
                          "adults_low","eggs_low","larvae_low","pupae_low","dadults_low","dfrgrs_low","necconc_low",
                          "adults_up","eggs_up","larvae_up","pupae_up","dadults_up","dfrgrs_up","necconc_up")
    
  } # Close j loop
} # Close i loop

write.csv(summ_msd, "C:/Users/CDouglas/VarroaPop/jeff/outputs_archive/summ_msd.csv")
write.csv(summ_mci, "C:/Users/CDouglas/VarroaPop/jeff/outputs_archive/summ_mci.csv")
