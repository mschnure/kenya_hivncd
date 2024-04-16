# Kenya HIV-NCD model 
A compartmental model used to provide accurate forecasts of the age distribution of people living with HIV in Kenya from 2025 to 2040. Calibrated to historical estimates of HIV epidemiology in Kenya using Bayesian methods.

Published in AIDS in March 2024: https://pubmed.ncbi.nlm.nih.gov/38537051/

---

![Model figure](/results/model_figure.png)

---

#### Foreward 
The following documentation describes the files available here for running the model.

---

#### Folders: 
* Main folders 
    + model 
    + calibration 
    + interventions 
    + for_ncd_model 
* Other folders 
    + cached – large objects 
    + data – input data files 
    + mcmc_cache – autosaves when running mcmc, clears itself 
    + mcmcruns – mcmc files 
    + results – figures and tables 

#### model 
1. diffeq (created 1/20/22)
    a. Core differential equation functions to model disease dynamics/changes in states
    b. See ‘More details’ below
2. parameters (created 1/20/22)
    a. Functions to set up the parameter object with constant and time-varying parameters 
    b. See ‘More details’ below
3. extract_data (created 1/20/22) 
    a. Functions to extract different information from the ODE result; called in plotting functions but can be used on their own 
    b. See ‘More details’ below
4. test_case (created 1/27/22) 
    a. Code to run a test case of the model
    b. See ‘More details’ below
5. plots (created 2/10/22) 
    a. Plotting functions that pull data from both the simulation and surveillance data and plots them
    b. See ‘More details’ below
6. age_mappings (created 3/24/22)
    a. Code to map finer age brackets to wider ones; either by aggregating model age groups to create surveillance age groups or vice versa
    b. See ‘More details’ below
7. run_systematic (created 5/25/22) 
    a. Code that runs from initializing parameters all the way through simulation; single function that is analogous to all the code in the         test_case file, but also allows for easier manipulation of sampled parameters 
    b. See ‘More details’ below
8. parameter_optim (created 5/25/22)
    a. Code for running and scoring simulations using different sampled parameter values; can set values either manually or using an optimization function 
    b. See ‘More details’ below

#### calibration 
1. Likelihood folder 
    a. likelihood.R (created 9/22/22) 
    b. test_likelihood.R (created 10/13/22) 
2. Parameter mappings folder 
    c. age_sex_mixing_proportions (created 6/9/22)
        i. See ‘More details’ below
    a. south_africa_age_mixing (created 7/5/22) 
        i. See ‘More details’ below
    b. age_sex_transmission_multipliers.R (created 11/7/22) 
    c. engagement_disengagement_data.R (created 11/17/22) 
    d. prior_distributions.R (created 10/24/22) 
    e. suppression_rebound_data.R (created 11/14/22) 
    f. testing_projection.R (created 11/7/22) 
3. Setting up/running/analyzing mcmc 
    a. setup_mcmc.R (created 1/27/23) 
    b. run_mcmc_chain.R (created 2/9/23) 
    c. setup_run_mcmc.R (created 1/31/23) 
    d. assemble_mcmc.R (created 2/24/23) 
    e. analyze_parallel_mcmc.R (created 1/17/23) 
4. Weighting/scaling
    a. data_point_weighting.R (created 2/23/23) 
    b. resampling.R (created 5/4/23) 
    c. scaling_prevalence (created 2/3/23) 
5. Other 
    a. file_settings.R (created 3/29/23) 
        i. For specifying directories and versions 
    b. Testing folder – files for debugging/profiling 
    c. Starting values folder – Rdata objects to start MCMC from 

#### interventions 
1. Creating interventions, pulling/checking results  
    a. create_interventions.R (created 1/31/23) 
        i. Main file for creating intervention units, intervention from units, running intervention on simset 
    b. extract_intervention_results (created 2/9/23) 
        i. File for generating results array, calculating estimates (age distributions, medians, incidence reductions, etc.) 
    c. checking_interventions.R (created 2/8/23) 
        i. Making sure the interventions are working as they should and viewing results 
2. Intervention sets 
    a. intervention_set_v1.R (created 2/24/23) 
    b. intervention_set_v2.R (created 4/12/23) 
    c. intervention_set_ncd_model.R (created 5/1/23) 
3. Running interventions 
    a. run_interventions_on_simset.R (created 5/4/23) 
    b. run_interventions_on_simset_ncd_model.R (created 5/1/23) 
    c. RUN_SAVE_ALL_RESULTS.R (created 5/4/23) 
    d. RUN_SAVE_ALL_RESULTS_manuscript.R (created 9/21/23) 

#### for_ncd_model 
1. extract_ncd_output.R (created 10/13/22) 
    a. Functions for pulling simset output to use as input for ncd model 
2. save_ncd_output.R (created 3/8/23) 
    a. File to run to actually pull and save output 
3. Data files 
    a. hivpop.csv
    b. simpop.csv 
4. Other 
    a. output scratch code.R (created 1/9/23) 
    b. testing_ncd_output.R (created 11/8/22) 
    c. testing_parameter_methods (created 11/17/22) 

#### Other top-level files 
1. data_manager (created 2/18/22)
    a. Functions used to read in and extract surveillance data for model calibration/inputs; also used in plotting functions (analog to extract_data functions for simulation data) 
    b. See ‘More details’ below
2. source_code (created 3/24/22)
    a. Sources necessary model files; creates data.manager object 
3. figures.R (created 2/24/23) 
    a. For creating prettier figures for presentation/papers (different from plot file) 
4. save_data_manager.R (created 10/13/22) 
    a. So I don’t have to create data manager each time 
5. sensitivity_analysis.R (created 5/10/23) 
    a. Used the sensitivity_analysis_NEW.R in model folder instead, created 8/11/23
6. baseline_values.R (created 8/29/23) 
    a. For estimating baseline values pre-intervention (for manuscript) 



#### More details on specific files/functions: 
1. diffeq (created 1/20/22)
    a. Core differential equation functions to model disease dynamics/changes in states
    b. Functions 
        i. compute.dx
            1. Called at every iteration; takes a vector form of the model state at this time, computes the changes in state we are interested in, returns a flattened vector of the changes in each element we are interested in
        ii. set.up.initial.diffeq.vector
            1. Builds initial vector
        iii. run.model 
            1. Uses odeintr package to integrate the ODE system 
        iv. process.ode.results
            1. Separates out and saves the ode results in a meaningful data structure (list of results with class “hiv_simulation,” indexed by time, including states/incidence/diagnoses, etc.) 
2. parameters (created 1/20/22)
    a. Functions to set up the parameter object with constant and time-varying parameters 
    b. Core functions 
        i. create.model.parameters 
            1. Sets up the basic parameters (sexes, ages, subgroups, HIV status); call this function with no arguments when first setting up a test case 
        ii. get.default.parameters 
            1. Sets default values for parameters we will sample; called in “run_systematic” code with the option to change values
        iii. map.model.parameters 
            1. Uses parameters object (set up via create.model.parameters) and sampled parameters (set up via get.default.parameters) to set up full set of parameters needed for diffeq (all dimensions of age/sex, etc., all years) 
            2. Types of parameters: fertility, aging, mortality (HIV/non-HIV), diagnoses, transmission rates, infectiousness, engagement/disengagement, suppression/unsuppression
            3. Everything technically added as a time-varying parameter even if it doesn’t vary 
        iv. add.time.varying.parameter.value
            1. Adds a time point and a value to the spline for a parameter; added to parameters$time.varying.parameters
        v. compute.time.varying.parameters
            1. Computes the parameter value at a specific time; called at beginning of compute.dx function in diffeq code; applied across all parameters$time.varying.parameters
    c. Other/helper functions 
        i. calculate.all.death.rates 
            1. Calculates death rates for correct model age stratifications, based on surveillance data
        ii. get.initial.population 
            1. Sets up initial population by mapping surveillance data age brackets to model age brackets (using map.population.ages function); adds all initial population to HIV negative status in 1970 except for specified seed cases 
        iii. make.transmission.array
            1. Multiplies global transmission rate by age and sex multipliers; called in map.model.parameters function when setting up transmission rates 
            2. Age and sex multipliers come from sampled.parameters 
    iv. map.birth.rates (no longer in use)
            1. Creates array of crude birth rates from total births; no longer in use – using age-specific fertility instead 
3. data_manager (created 2/18/22)
    a. Functions used to read in and extract surveillance data for model calibration/inputs; also used in plotting functions (analog to extract_data functions for simulation data) 
    b. Higher-level core functions 
        i. get.surveillance.data 
            1. Extracts data from data.manager object (created using call to read.surveillance.data function); called in plotting functions but can be used on its own; allows user to specify which dimensions to look at and/or stratify by (see simplot function) 
        ii. read.surveillance.data 
            1. Creates data.manager object; called in source code 
            2. Calls different lower-level functions for each data type (based on input file type) 
                a. Incidence/prevalence: read.surveillance.data.type
                b. AIDS mortality: read.surveillance.data.files
                c. Population: read.population.data.files.model.ages (aggregated into model age groups) or read.population.data.files.all.ages (full population) 
                d. Fertility: read.fertility.data.files
                e. Birth rate: read.birth.data.files (no longer using)
                f. Deaths: read.death.data.files 
    c. Lower-level helper functions 
        i. read.surveillance.data.type
            1. Called once for each data type within read.surveillance.data function; creates list with an array for each data stratification (i.e., total, by age, by sex, by subgroup, by age and subgroup, etc.) 
            2. Calls lower-level functions 
                a. read.surveillance.data.files (for total/subgroup) 
                b. read.surveillance.data.stratified (for age/sex) 
        ii. read.surveillance.data.stratified 
            1. Called once for each dimension (age/sex) within read.surveillance.data.type 
            2. Loops through lower-level function for each stratum of that dimension (i.e., calls function once for every age group) 
                a. Age: read.surveillance.data.files
                b. Sex: combine.pdf.years
        iii. read.surveillance.data.files 
            1. Called once for each stratum of specified dimension within read.surveillance.data.stratified 
            2. Reads in csv files; formats data; returns an array of data with correct dimensions based on strata; option to read in lower/upper bound files  
    d. Specialty functions
        i. combine.pdf.years 
            1. Called for sex-specific incidence/prevalence data in read.surveillance.data.stratified
            2. Combines multiple pdfs from different years; calls read.pdf.data.files for each year
        ii. read.pdf.data.files
            1. For each pdf year, read in csv file from tabula, return sex-specific incidence or prevalence 
        iii. read.population.data.files.model.ages 
            1. Reads in population data files and returns data in the age brackets needed for the model; returns list with an array for each stratification (total, age, sex, age*sex) 
        iv. read.population.data.files.all.ages 
            1. Reads in population data files and returns data in the age brackets they are reported in; returns list with an array for each stratification (total, age, sex, age*sex) 
        v. read.fertility.data.files
            1. Reads in age-specific fertility data 
        vi. read.death.data.files
            1. Reads in age/sex-specific death data 
        vii. read.birth.data.files
            1. No longer using; read in total births to create a crude birth rate 
4. extract_data (created 1/20/22)
    a. Functions to extract different information from the ODE result; called in plotting functions but can be used on their own 
    b. Functions 
        i. extract.data
            1. General function to extract data based on data type; calls lower-level functions (e.g., extract incidence) based on which data type is given 
        ii. do.extract.4D
            1. Used to pull four-dimensional results from the simulation (i.e., population) that are indexed age, sex, subgroup, HIV status (also includes year as the first dimension) 
        iii. do.extract.3D
            1. Used to pull three-dimensional results from the simulation (i.e., incidence) that is only within the HIV population and is indexed age, sex, subgroup (again includes year as first dimension) 
        iv. extract.population 
            1. Call to do.extract.4D; pulls population 
        v. extract.incidence 
            1. Call to do.extract.3D; pulls incidence
        vi. extract.prevalence
            1. Call to extract.population with HIV status set only to HIV states (need to keep fourth dimension of HIV status to differentiate between cascade states, but only pull those with HIV); pulls prevalence 
        vii. extract.new.diagnoses
            1. Call to do.extract.3D; pulls new diagnoses 
        viii. extract.hiv.mortality
            1. Call to do.extract.4D; pulls HIV mortality 
        ix. extract.suppression
            1. Call to do.extract.3D; pulls suppression
        x. extract.engagement
            1. Call to do.extract.3D; pulls engagement
        xi. extract.disengagement.suppressed
            1. Call to do.extract.3D; pulls disengaged from suppressed
        xii. extract.disengagement.unsuppressed
            1. Call to do.extract.3D; pulls disengaged from unsuppressed
5. test_case (created 1/27/22) 
    a. Code to run a test case of the model
        i. Importantly, this code does not easily facilitate changing values for our sampled parameters (automatically sets up with default values and not easy to change); in order to change sampled parameter values and run the model more systematically, use run_systematic code 
    b. No new functions defined 
    c. Steps: Create basic model parameters; map all parameters to structure needed for diffeq; set up the initial state; run the model; plot outputs 
6. plots (created 2/10/22) 
    a. Plotting functions that pull data from both the simulation and surveillance data and plots them
    b. Functions 
        i. simplot.basic
            1. Generate plot comparing simulation results to surveillance data; only allowed to facet by data type 
    ii. simplot
            1. Generate plot comparing simulation results to surveillance data; allows the user to specify which dimensions they’d like to look at (i.e., only look at data among women) and/or which dimensions they’d like to stratify by (i.e., separate panels or shapes by age group)
7. age_mappings (created 3/24/22) 
    a. Code to map finer age brackets to wider ones; either by aggregating model age groups to create surveillance age groups or vice versa
    b. Functions 
        i. parse.age.brackets 
            1. Using age cutoffs, creates age bracket labels 
        ii. get.age.brackets.in.range
            1. Given a lower and upper value, returns all age brackets within range; call to parse.age.brackets
        iii. map.ages 
            1. Creates list of necessary age brackets based on mapping list; called in extract_data functions 
            2. For now, uses hard-coded/specific mappings; created generic functions below but these are not really being used fully 
    c. Generic functions
        i. map.population.ages 
            1. Used when surveillance age brackets are finer than model age brackets (only for population data) 
            2. Calls maps.ages.by.cutoffs once for each model age bracket
            3. Currently only being used to create initial population and calculate death rates 
    ii. map.all.ages
            1. Used when model age brackets are finer than surveillance age brackets (everything except for population data) 
            2. Calls map.ages.by.cutoffs once for each surveillance age bracket 
            3. Currently not being used 
    iii. map.ages.by.cutoffs
            1. Called once for each wider age bracket in map.population.ages and map.all.ages
            2. Given each wider age bracket, returns the finer age brackets to include (e.g., given age bracket of 15-24, would return 15-19, 20-24) 
8. run_systematic (created 5/25/22)
    a. Code that runs from initializing parameters all the way through simulation; single function that is analogous to all the code in the test_case file, but also allows for easier manipulation of sampled parameters 
    b. Functions 
        i. run.model.for.parameters 
            1. Analogous to test_case code (create basic model parameters; map all parameters to structure needed for diffeq; set up the initial state; run the model)
            2. Can be passed variable.parameters as an input to override default sampled parameters
            3. Called in parameter_optim code 
        ii. extract.hiv.data.for.ncd 
            1. Extracts outputs needed for NCD model 
9. parameter_optim (created 5/25/22)
    a. Code for running and scoring simulations using different sampled parameter values; can set values either manually or using an optimization function 
    b. Generic optim function in this code returns optimized parameter values; can then compare simulation scores using these optimized parameter values or manually chosen parameter values 
    c. Functions 
        i. run.and.score.sim
            1. Runs a simulation given set of sampled parameters; returns a score by calling score.sim function
            2. This function is fed to a generic optimization function to be minimized 
        ii. score.sim
            1. Calculates the log sum of squared errors between the simulated population and surveillance data for population (to be minimized via optimization) 
            2. Called by run.and.score.sim 
9. age_sex_mixing_proportions (created 6/9/22) 
    a. Code that generates mixing proportions, i.e., the percent of an individual’s partners who are in each age/sex stratum
    b. These functions are called in succession in the parameters code when setting up transmission rates (by multiplying the age/sex transmission array by an array of mixing proportions) 
    c. Functions 
        i. get.sex.mixing.proportions 
            1. Returns a vector with the proportion of an individual’s partners who are in each sex (for now hard-coded to opposite sex only) 
            2. Called in map.model.parameters function in parameters code in order to create mixing proportions matrix to multiply by transmission rates
        ii. get.age.mixing.proportions 
            1. Returns a vector with the proportion of an individual’s partners who are in each age
            2. Called in map.model.parameters function in parameters code in order to create mixing proportions matrix to multiply by transmission rates
            3. Calls calculate.pairing.proportions.by.age for each sex
        iii. calculate.pairing.proportions.by.age
            1. Given regression coefficients for the mean age difference between partners (intercept and slope each for mean difference and standard deviation of the mean difference), returns full matrix of pairing proportions by age
            2. When called by get.age.mixing.proportions above, pulls out the single “age to” bracket 
10. south_africa_age_mixing (created 7/5/22)
    a. Code to return regression coefficients for mean partner age differences from South Africa paper 
    b. Functions
        i. get.male.to.female.age.model
            1. Using data on female ages and the mean difference in age of their partner, regresses on both the mean difference and the standard deviation of the mean differences, returns intercept and slope for each
            2. Uses the data exactly from the paper; HIV positive women in male-female clusters with HIV-positive male; presumed male to female transmission 
        ii. get.female.to.male.age.model
            1. Same process as above 
            2. Because the paper does not report the reverse partnerships (female to male transmission), used data on HIV-positive women from the community survey for the mean difference regression and used the male to female data for the standard deviation regression  
