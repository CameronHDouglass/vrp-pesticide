### Install 'VarroaPopWrapper' package (depends on 'devtools' package). Note that 'logs' and 'save_files' operators default to "FALSE" and must be set to "TRUE" to get log, in/output files
library("devtools", lib.loc="~/R/win-library/3.5")
library("VarroaPopWrapper", lib.loc="~/R/win-library/3.5")
library("abind", lib.loc="~/R/win-library/3.5")
library("truncnorm", lib.loc="~/R/win-library/3.5") #Required to use truncated normal function
library("EnvStats", lib.loc="~/R/win-library/3.5") #Required to use the truncated lognormal function

### Create folder and sub-folders for files created during MC analyses
dir.create("C:/Users/CDouglas/VarroaPop/l0/", showWarnings = FALSE)
dir.create("C:/Users/CDouglas/VarroaPop/l0/inputs_archive/", showWarnings = FALSE)
dir.create("C:/Users/CDouglas/VarroaPop/l0/inputs_array/", showWarnings = FALSE)
dir.create("C:/Users/CDouglas/VarroaPop/l0/outputs_archive/", showWarnings = FALSE)
dir.create("C:/Users/CDouglas/VarroaPop/l0/outputs_array/", showWarnings = FALSE)

### Define weather input file
weather = "C:/Users/CDouglas/VarroaPop/inputs/15055_grid_35.875_lat.wea"

### Define location of VarroaPop exe file (more easily allows for use of updated versions of the model)
exe_file = "C:/Users/CDouglas/Documents/R/win-library/3.5/VarroaPopWrapper/varroapop_files/exe/3311/VarroaPop.exe"
vrp_file = "C:/Users/CDouglas/Documents/R/win-library/3.5/VarroaPopWrapper/varroapop_files/exe/3311/test.vrp"

### Define nectar/pollen residue levels (i.e. "contamination")
npcontamination_level0 <- "C:/Users/CDouglas/VarroaPop/inputs/clo_feeding_0.csv"
#npcontamination_level1 <- "C:/Users/CDouglas/VarroaPop/inputs/clo_feeding_10.csv"
#npcontamination_level2 <- "C:/Users/CDouglas/VarroaPop/inputs/clo_feeding_20.csv"
#npcontamination_level3 <- "C:/Users/CDouglas/VarroaPop/inputs/clo_feeding_40.csv"
#npcontamination_level4 <- "C:/Users/CDouglas/VarroaPop/inputs/clo_feeding_80.csv"
#npcontamination_level5 <- "C:/Users/CDouglas/VarroaPop/inputs/clo_feeding_160.csv"
necpolfile <- npcontamination_level0

### Define number of desired MC runs
nsims <- 1
z <- nsims

### Setup 3d data storage array for model outputs (ie to store desired data points from VRP output files)
nrows <- 5
ncol <- 4
summ_outputs <- array(data = NA, c(nrows, ncol, nsims))
colnames(summ_outputs)<-c("adults","eggs","larvae","pupae")
rownames(summ_outputs)<-c("CCA3","CCA4","CCA5","CCA6","CCA7")

### Setup 2d data storage array for model inputs (ie to store inputs for each VRP model run)
in_nrows <- 27
in_ncol <- nsims
summ_inputs <- array(data = NA, c(in_nrows, in_ncol))
rownames(summ_inputs)<-c("adLD50","adslope","lLD50","lslope","ca13nectar","ca13pollen","ca1120nectar","ca1120pollen",
                         "ca410nectar","ca410pollen","cl4nectar","cl4pollen","cl5nectar","cl5pollen","frgrmaxprop",
                         "frgrlifespan","queenstrength","nload","ntrips","initialnectar","initialpollen",
                         "frgrnectar","frgrpollen","adults","pupae","eggs","larvae")

### Loop to do n Monte Carlo runs
for(i in 1:nsims){
  
  ### Define varying input parameters for VarroaPop model
  adLD50 <- rtruncnorm(1, a=0.00967, b=0.04069, mean=0.02008, sd=0.00808) # AIAdultLD50
  adslope <- rtruncnorm(1, a=1.25, b=8.63, mean=4.93, sd=2.19) # AIAdultSlope
  lLD50 <- rtruncnorm(1, a=0.00372, b=0.09548, mean=0.04311, sd=0.02831) # AILarvaLD50
  lslope <- rtruncnorm(1, a=1.34, b=8.66, mean=5.07, sd=2.14) # AILarvaSlope
  ca13nectar <- runif(1, 20, 100) 
  ca13pollen <- runif(1, 2, 9.5) 
  ca1120nectar <- runif(1, 20, 100) 
  ca1120pollen <- runif(1, 0, 3) 
  ca410nectar <- runif(1, 20, 200) 
  ca410pollen <- runif(1, 2, 9.5) 
  cl4nectar <- runif(1, 0, 100) 
  cl4pollen <- runif(1, 0, 4)
  cl5nectar <- runif(1, 0, 150) 
  cl5pollen <- runif(1, 0, 6) 
  frgrmaxprop <- runif(1, 0, 0.3) # ForagerMaxProp
  frgrlifespan <- rtruncnorm(1, a=11.97, b=15.9, mean=14.37, sd=1.07) # ICForagerLifespan
  queenstrength <- rtruncnorm(1, a=2.69, b=4.88, mean=3.87, sd=0.61) # RQQueenStrength
  nload <- runif(1, 22, 51) # INectarLoad
  ntrips <- rlnormTrunc(1, meanlog=10, sdlog=3, min=1, max=15) # INectarTrips
  initialnectar = rtruncnorm(1, a=11901, b=18066, mean=14983, sd=7534) #InitColNectar
  initialpollen = rtruncnorm(1, a=2012, b=2825, mean=2419, sd=998) #InitColPollen
  frgrnectar = runif(1, 80.25, 321) # CForagerNectar
  frgrpollen = runif(1, 8.75, 13) # CForagerPollen
  adults = rtruncnorm(1, a=12942, b=17087, mean=15014, sd=5092) #ICWorkerAdults
  pupae = rtruncnorm(1, a=15830, b=18760, mean=17295, sd=3592) #ICWorkerBrood
  eggs = rtruncnorm(1, a=5160, b=7627, mean=6394, sd=3038) #ICWorkerEggs
  larvae = rtruncnorm(1, a=8436, b=11167, mean=9801, sd=3358) #ICWorkerLarvae
  pupaetoadults = c(06/20/2014, 10/22/2014, 60) #BToAXiTion
  
  ### Run iteration of VRP Wrapper for negative control (i.e., level0)
  params = c(SimStart = "06/20/2014", SimEnd = "10/22/2014",
             AIAdultLD50 = adLD50, AIAdultSlope = adslope, 
             AILarvaLD50 = lLD50, AILarvaSlope = lslope,
             ca13nectar = ca13nectar, ca13pollen = ca13pollen,
             ca1120nectar = ca1120nectar, ca1120pollen = ca1120pollen,
             ca410nectar = ca410nectar, ca410pollen = ca410pollen,
             cl4nectar = cl4nectar, cl4pollen = cl4pollen,
             cl5nectar = cl5nectar, cl5pollen = cl5pollen,
             ForagerMaxProp = frgrmaxprop, ICForagerLifespan = frgrlifespan,
             RQQueenStrength = queenstrength,
             INectarLoad = nload, INectarTrips = ntrips,
             InitColNectar = initialnectar, InitColPollen = initialpollen,
             CForagerNectar = frgrnectar, CForagerPollen = frgrpollen,
             ICWorkerAdults = adults, ICWorkerBrood = pupae,
             ICWorkerEggs = eggs, ICWorkerLarvae = larvae, 
             BToAXitionEnable = TRUE, BToAXition = pupaetoadults,
             NecPolFileEnable = TRUE, NecPolFileName = necpolfile)
  
  summ_inputs[,i] <- c(adLD50, adslope, lLD50, lslope, 
             ca13nectar, ca13pollen, ca1120nectar, ca1120pollen,
             ca410nectar, ca410pollen, cl4nectar, cl4pollen,
             cl5nectar, cl5pollen,
             frgrmaxprop, frgrlifespan,
             queenstrength,
             nload, ntrips,
             initialnectar, initialpollen,
             frgrnectar, frgrpollen,
             adults, pupae, eggs, larvae)
  
  # These input/output files will be used for population of the storage array
  in_path <- "C:/Users/CDouglas/VarroaPop/l0/inputs_array/"
  in_file <- "inputs.txt"
  out_path <- "C:/Users/CDouglas/VarroaPop/l0/outputs_array/"
  out_file <- "outputs.txt"
  output <- RunVarroaPopLocal(parameters = params, exe_file = exe_file, vrp_file = vrp_file, weather_file = weather, logs = TRUE, save_files = TRUE, out_file = out_file, 
                              out_path = out_path, in_file = in_file, in_path = in_path)
  
  # These input/output files will be for archiving and allow for posthoc inspection and analyses of each run (if necessary)
  sim_in_path <- "C:/Users/CDouglas/VarroaPop/l0/inputs_archive/"
  sim_in_file <- paste("inputs_", i, ".csv", sep="")
  sim_out_path <- "C:/Users/CDouglas/VarroaPop/l0/outputs_archive/"
  sim_out_file <- paste("outputs_", i, ".csv", sep="")
  sim_output <- RunVarroaPopLocal(parameters = params, exe_file = exe_file, vrp_file = vrp_file, weather_file = weather, logs = TRUE, save_files = TRUE, out_file = sim_out_file, 
                              out_path = sim_out_path, in_file = sim_in_file, in_path = sim_in_path)
  
    ### Pull back in the results to populate the data array
    outputs <- read.table("C:/Users/CDouglas/VarroaPop/l0/outputs_array/outputs.txt", skip = 7)
    
      ### Populate the storage array ('summ_outputs') with simulation results
      summ_outputs[,,i][c(1,2,3,4,5),1]<-outputs[c(1,28,49,83,125),4]
      summ_outputs[,,i][c(1,2,3,4,5),2]<-outputs[c(1,28,49,83,125),11]
      summ_outputs[,,i][c(1,2,3,4,5),3]<-outputs[c(1,28,49,83,125),9]
      summ_outputs[,,i][c(1,2,3,4,5),4]<-outputs[c(1,28,49,83,125),7]
    
} # End i loop

write.csv(summ_inputs, "C:/Users/CDouglas/VarroaPop/l0/inputs_archive/summ_inputs.csv")
write.csv(summ_outputs, "C:/Users/CDouglas/VarroaPop/l0/outputs_archive/summ_outputs.csv")

### Run descriptive statistics for VRP output files - means are stored in "summ_mean" output file; standard deviations are stored in "summ_sd"; means +/- 95% CIs are stored in "summ_stats"

summ_mean <- array(data = NA, c(nrows, ncol))
colnames(summ_mean)<-c("adults_mean","eggs_mean","larvae_mean","pupae_mean")
rownames(summ_mean)<-c("CCA3","CCA4","CCA5","CCA6","CCA7")
summ_sd <- array(data = NA, c(nrows, ncol))
colnames(summ_sd)<-c("adults_sd","eggs_sd","larvae_sd","pupae_sd")
rownames(summ_sd)<-c("CCA3","CCA4","CCA5","CCA6","CCA7")
ci <- array(data = NA,c(nrows,ncol))
low <- array(data = NA,c(nrows,ncol))
up <- array(data = NA,c(nrows,ncol))
colnames(low)<-c("adults_low","eggs_low","larvae_low","pupae_low")
colnames(up)<-c("adults_up","eggs_up","larvae_up","pupae_up")

for (i in 1:nrows) {
  for (j in 1:ncol) {
    
      summ_sd[i,j] <- sd(summ_outputs[i,j,1:nsims]) 
      summ_mean[i,j] <- mean(summ_outputs[i,j,1:nsims])
      summ_msd <- cbind(summ_mean, summ_sd)
    
      ci[i,j] <- qnorm(.975)*(sd(summ_outputs[i,j,1:nsims])/sqrt(nsims))
      up[i,j] <- (mean(summ_outputs[i,j,1:nsims]))+(qnorm(.975)*(sd(summ_outputs[i,j,1:nsims])/sqrt(nsims)))
      low[i,j] <- (mean(summ_outputs[i,j,1:nsims]))-(qnorm(.975)*(sd(summ_outputs[i,j,1:nsims])/sqrt(nsims)))
      
      summ_mci <- abind(summ_mean,low,up)
      
    } # Close j loop
  } # Close i loop

write.csv(summ_msd, "C:/Users/CDouglas/VarroaPop/l0/outputs_archive/summ_msd.csv")
write.csv(summ_mci, "C:/Users/CDouglas/VarroaPop/l0/outputs_archive/summ_mci.csv")
