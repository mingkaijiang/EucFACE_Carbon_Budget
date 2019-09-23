run_MCMC_main_program <- function(prefit.option,
                                  obsDF,
                                  eco2DF,
                                  dist.type) {
    if (prefit.option == "prefit") {
        #### B. Estimate prefit allocation parameter uncertainties for ambient CO2 treatment
        ### step B1:
        ## this initial parameters explore prefit parameter space
        init.parameters <- run_prefit_program_MCMC(dist.type=dist.type, 
                                                   obsDF=obsDF,
                                                   eco2DF=eco2DF,
                                                   range.option="sd")
        
        ########################################################################################
        #### C. Estimate remaining parameter uncertainties for ambient CO2 treatment
        ### step C1: set up 
        ## initialize parameters 
        ## based on prefit parameters
        source("definitions/initialize_aCO2_parameters.R")
        source("definitions/initialize_eCO2_parameters.R")
        
        ### Assign chain length for MCMC parameter fitting
        chainLength <- 5000
        
        ### step C2: fitting
        ## Ring 2
        step.size.aCO2 <- 0.05 
        pChain_aCO2_1 <- MCMC_model_fitting(params = params.aCO2.R2, 
                                            params.lower = params.aCO2.lower.R2,
                                            params.upper = params.aCO2.upper.R2,
                                            obs=obsDF[1,],
                                            chainLength=chainLength,
                                            dist.type=dist.type,
                                            step.size=step.size.aCO2)
        
        
        generate_most_likely_outcome(inDF=pChain_aCO2_1,
                                     obs=obsDF[1,])
        
        
        plot_parameter_trace_within_parameter_space(params= params.aCO2.R2, 
                                                    params.lower = params.aCO2.lower.R2,
                                                    params.upper = params.aCO2.upper.R2,
                                                    inDF = pChain_aCO2_1,
                                                    dist.type=dist.type,
                                                    step.size=step.size.aCO2,
                                                    chainLength=chainLength,
                                                    Trt = "aCO2_1")
        
        
        # Ring 3
        step.size.aCO2 <- 0.05 
        pChain_aCO2_2 <- MCMC_model_fitting(params = params.aCO2.R3, 
                                            params.lower = params.aCO2.lower.R3,
                                            params.upper = params.aCO2.upper.R3,
                                            obs=obsDF[2,],
                                            chainLength=chainLength,
                                            dist.type=dist.type,
                                            step.size=step.size.aCO2)
        
        generate_most_likely_outcome(inDF=pChain_aCO2_2,
                                     obs=obsDF[2,])
        
        
        plot_parameter_trace_within_parameter_space(params= params.aCO2.R3, 
                                                    params.lower = params.aCO2.lower.R3,
                                                    params.upper = params.aCO2.upper.R3,
                                                    inDF = pChain_aCO2_2,
                                                    dist.type=dist.type,
                                                    step.size=step.size.aCO2,
                                                    chainLength=chainLength,
                                                    Trt = "aCO2_2")
        
        # Ring 6
        step.size.aCO2 <- 0.0008 
        pChain_aCO2_3 <- MCMC_model_fitting(params = params.aCO2.R6, 
                                            params.lower = params.aCO2.lower.R6,
                                            params.upper = params.aCO2.upper.R6,
                                            obs=obsDF[3,],
                                            chainLength=chainLength,
                                            dist.type=dist.type,
                                            step.size=step.size.aCO2)
        
        
        generate_most_likely_outcome(inDF=pChain_aCO2_3,
                                     obs=obsDF[3,])
        
        
        plot_parameter_trace_within_parameter_space(params= params.aCO2.R6, 
                                                    params.lower = params.aCO2.lower.R6,
                                                    params.upper = params.aCO2.upper.R6,
                                                    inDF = pChain_aCO2_3,
                                                    dist.type=dist.type,
                                                    step.size=step.size.aCO2,
                                                    chainLength=chainLength,
                                                    Trt = "aCO2_3")
        
        
        ### step C3: 
        ### combine the results, and make some plots
        pChain.aCO2 <- rbind(pChain_aCO2_1, pChain_aCO2_2, pChain_aCO2_3)
        
        plot_posterior(inDF = pChain.aCO2, Trt = "aCO2", dist.type = dist.type,
                       chainLength = chainLength)
        
        ### step C4: 
        ### predict final output, at mean aCO2
        ### print out the final predicted results 
        ### check if the Rhet is OKish?
        predict_final_output(pChain = pChain.aCO2, 
                             obs = obsDF[4,],
                             return.option = "Check result")
        
        
        ########################################################################################
        #### D: Check what parameters are needed for the eCO2 response
        ### step D1: 
        predict_final_output(pChain = pChain.aCO2, 
                             obs = eco2DF[4,],
                             return.option = "Check result")
        
        ### step D2: 
        ### set up step size for aCO2 and eCO2 
        chainLength <- 5000
        
        ### step D3:
        ### fit the model with eCO2 parameter space to get parameter uncertainties
        # Ring 1
        step.size.eCO2 <- 0.006 
        pChain_eCO2_1 <- MCMC_model_fitting(params = params.eCO2.R1, 
                                            params.lower = params.eCO2.lower.R1,
                                            params.upper = params.eCO2.upper.R1,
                                            obs=eco2DF[1,],
                                            chainLength=chainLength,
                                            dist.type=dist.type,
                                            step.size=step.size.eCO2)
        
        generate_most_likely_outcome(inDF=pChain_eCO2_1,
                                     obs=eco2DF[1,])
        
        plot_parameter_trace_within_parameter_space(params= params.eCO2.R1, 
                                                    params.lower = params.eCO2.lower.R1,
                                                    params.upper = params.eCO2.upper.R1,
                                                    inDF = pChain_eCO2_1,
                                                    dist.type=dist.type,
                                                    step.size=step.size.eCO2,
                                                    chainLength=chainLength,
                                                    Trt = "eCO2_1")
        
        
        # ring 4
        step.size.eCO2 <- 0.002 
        pChain_eCO2_2 <- MCMC_model_fitting(params = params.eCO2.R4, 
                                            params.lower = params.eCO2.lower.R4,
                                            params.upper = params.eCO2.upper.R4,
                                            obs=eco2DF[2,],
                                            chainLength=chainLength,
                                            dist.type=dist.type,
                                            step.size=step.size.eCO2)
        
        generate_most_likely_outcome(inDF=pChain_eCO2_2,
                                     obs=eco2DF[2,])
        
        
        plot_parameter_trace_within_parameter_space(params= params.eCO2.R4, 
                                                    params.lower = params.eCO2.lower.R4,
                                                    params.upper = params.eCO2.upper.R4,
                                                    inDF = pChain_eCO2_1,
                                                    dist.type=dist.type,
                                                    step.size=step.size.eCO2,
                                                    chainLength=chainLength,
                                                    Trt = "eCO2_2")
        
        
        # ring 5
        step.size.eCO2 <- 0.007 
        pChain_eCO2_3 <- MCMC_model_fitting(params = params.eCO2.R5, 
                                            params.lower = params.eCO2.lower.R5,
                                            params.upper = params.eCO2.upper.R5,
                                            obs=eco2DF[3,],
                                            chainLength=chainLength,
                                            dist.type=dist.type,
                                            step.size=step.size.eCO2)
        
        generate_most_likely_outcome(inDF=pChain_eCO2_3,
                                     obs=eco2DF[3,])
        
        plot_parameter_trace_within_parameter_space(params= params.eCO2.R5, 
                                                    params.lower = params.eCO2.lower.R5,
                                                    params.upper = params.eCO2.upper.R5,
                                                    inDF = pChain_eCO2_1,
                                                    dist.type=dist.type,
                                                    step.size=step.size.eCO2,
                                                    chainLength=chainLength,
                                                    Trt = "eCO2_3")
        
        
        ### step D4: 
        ### combine the results, and make some plots
        pChain.eCO2 <- rbind(pChain_eCO2_1, pChain_eCO2_2, pChain_eCO2_3)
        
        plot_posterior(inDF = pChain.eCO2, Trt = "eCO2", dist.type = dist.type,
                       chainLength = chainLength)
        
        ### step D5: 
        ### predict final output, at mean eCO2 values
        ### print out the final predicted results 
        ### check if the Rhet is OK?
        predict_final_output(pChain = pChain.eCO2, 
                             obs = eco2DF[4,],
                             return.option = "Check result")
        
    } else if (prefit.option == "wide") {

        ########################################################################################
        #### C. Estimate remaining parameter uncertainties for ambient CO2 treatment
        ### step C1: set up 
        ## initialize parameters
        ## based on standardized initial parameter space
        source("definitions/initialize_aCO2_parameters_wide.R")
        source("definitions/initialize_eCO2_parameters_wide.R")
        
        ### Assign chain length for MCMC parameter fitting
        chainLength <- 5000
        
        ### step C2: fitting
        ## Ring 2
        step.size.aCO2 <- 0.005 
        pChain_aCO2_1 <- MCMC_model_fitting(params = params.aCO2.R2, 
                                            params.lower = params.aCO2.lower.R2,
                                            params.upper = params.aCO2.upper.R2,
                                            obs=obsDF[1,],
                                            chainLength=chainLength,
                                            dist.type=dist.type,
                                            step.size=step.size.aCO2)
        
        
        generate_most_likely_outcome(inDF=pChain_aCO2_1,
                                     obs=obsDF[1,])
        
        
        plot_parameter_trace_within_parameter_space(params= params.aCO2.R2, 
                                                    params.lower = params.aCO2.lower.R2,
                                                    params.upper = params.aCO2.upper.R2,
                                                    inDF = pChain_aCO2_1,
                                                    dist.type=dist.type,
                                                    step.size=step.size.aCO2,
                                                    chainLength=chainLength,
                                                    Trt = "aCO2_1")
        
        
        # Ring 3
        step.size.aCO2 <- 0.005 
        chainLength <- 50000
        pChain_aCO2_2 <- MCMC_model_fitting(params = params.aCO2.R3, 
                                            params.lower = params.aCO2.lower.R3,
                                            params.upper = params.aCO2.upper.R3,
                                            obs=obsDF[2,],
                                            chainLength=chainLength,
                                            dist.type=dist.type,
                                            step.size=step.size.aCO2)
        
        generate_most_likely_outcome(inDF=pChain_aCO2_2,
                                     obs=obsDF[2,])
        
        
        plot_parameter_trace_within_parameter_space(params= params.aCO2.R3, 
                                                    params.lower = params.aCO2.lower.R3,
                                                    params.upper = params.aCO2.upper.R3,
                                                    inDF = pChain_aCO2_2,
                                                    dist.type=dist.type,
                                                    step.size=step.size.aCO2,
                                                    chainLength=chainLength,
                                                    Trt = "aCO2_2")
        
        # Ring 6
        step.size.aCO2 <- 0.002 
        chainLength <- 50000
        pChain_aCO2_3 <- MCMC_model_fitting(params = params.aCO2.R6, 
                                            params.lower = params.aCO2.lower.R6,
                                            params.upper = params.aCO2.upper.R6,
                                            obs=obsDF[3,],
                                            chainLength=chainLength,
                                            dist.type=dist.type,
                                            step.size=step.size.aCO2)
        
        
        generate_most_likely_outcome(inDF=pChain_aCO2_3,
                                     obs=obsDF[3,])
        
        
        plot_parameter_trace_within_parameter_space(params= params.aCO2.R6, 
                                                    params.lower = params.aCO2.lower.R6,
                                                    params.upper = params.aCO2.upper.R6,
                                                    inDF = pChain_aCO2_3,
                                                    dist.type=dist.type,
                                                    step.size=step.size.aCO2,
                                                    chainLength=chainLength,
                                                    Trt = "aCO2_3")
        
        
        ### step C3: 
        ### combine the results, and make some plots
        pChain.aCO2 <- rbind(pChain_aCO2_1, pChain_aCO2_2, pChain_aCO2_3)
        
        plot_posterior(inDF = pChain.aCO2, Trt = "aCO2", dist.type = dist.type,
                       chainLength = chainLength)
        
        ### step C4: 
        ### predict final output, at mean aCO2
        ### print out the final predicted results 
        ### check if the Rhet is OKish?
        predict_final_output(pChain = pChain.aCO2, 
                             obs = obsDF[4,],
                             return.option = "Check result")
        
        
        ########################################################################################
        #### D: Check what parameters are needed for the eCO2 response
        ### step D1: 
        predict_final_output(pChain = pChain.aCO2, 
                             obs = eco2DF[4,],
                             return.option = "Check result")
        
        ### step D2: 
        ### set up step size for aCO2 and eCO2 
        chainLength <- 5000
        
        ### step D3:
        ### fit the model with eCO2 parameter space to get parameter uncertainties
        # Ring 1
        step.size.eCO2 <- 0.006 
        pChain_eCO2_1 <- MCMC_model_fitting(params = params.eCO2.R1, 
                                            params.lower = params.eCO2.lower.R1,
                                            params.upper = params.eCO2.upper.R1,
                                            obs=eco2DF[1,],
                                            chainLength=chainLength,
                                            dist.type=dist.type,
                                            step.size=step.size.eCO2)
        
        generate_most_likely_outcome(inDF=pChain_eCO2_1,
                                     obs=eco2DF[1,])
        
        plot_parameter_trace_within_parameter_space(params= params.eCO2.R1, 
                                                    params.lower = params.eCO2.lower.R1,
                                                    params.upper = params.eCO2.upper.R1,
                                                    inDF = pChain_eCO2_1,
                                                    dist.type=dist.type,
                                                    step.size=step.size.eCO2,
                                                    chainLength=chainLength,
                                                    Trt = "eCO2_1")
        
        
        # ring 4
        step.size.eCO2 <- 0.002 
        pChain_eCO2_2 <- MCMC_model_fitting(params = params.eCO2.R4, 
                                            params.lower = params.eCO2.lower.R4,
                                            params.upper = params.eCO2.upper.R4,
                                            obs=eco2DF[2,],
                                            chainLength=chainLength,
                                            dist.type=dist.type,
                                            step.size=step.size.eCO2)
        
        generate_most_likely_outcome(inDF=pChain_eCO2_2,
                                     obs=eco2DF[2,])
        
        
        plot_parameter_trace_within_parameter_space(params= params.eCO2.R4, 
                                                    params.lower = params.eCO2.lower.R4,
                                                    params.upper = params.eCO2.upper.R4,
                                                    inDF = pChain_eCO2_1,
                                                    dist.type=dist.type,
                                                    step.size=step.size.eCO2,
                                                    chainLength=chainLength,
                                                    Trt = "eCO2_2")
        
        
        # ring 5
        step.size.eCO2 <- 0.007 
        pChain_eCO2_3 <- MCMC_model_fitting(params = params.eCO2.R5, 
                                            params.lower = params.eCO2.lower.R5,
                                            params.upper = params.eCO2.upper.R5,
                                            obs=eco2DF[3,],
                                            chainLength=chainLength,
                                            dist.type=dist.type,
                                            step.size=step.size.eCO2)
        
        generate_most_likely_outcome(inDF=pChain_eCO2_3,
                                     obs=eco2DF[3,])
        
        plot_parameter_trace_within_parameter_space(params= params.eCO2.R5, 
                                                    params.lower = params.eCO2.lower.R5,
                                                    params.upper = params.eCO2.upper.R5,
                                                    inDF = pChain_eCO2_1,
                                                    dist.type=dist.type,
                                                    step.size=step.size.eCO2,
                                                    chainLength=chainLength,
                                                    Trt = "eCO2_3")
        
        
        ### step D4: 
        ### combine the results, and make some plots
        pChain.eCO2 <- rbind(pChain_eCO2_1, pChain_eCO2_2, pChain_eCO2_3)
        
        plot_posterior(inDF = pChain.eCO2, Trt = "eCO2", dist.type = dist.type,
                       chainLength = chainLength)
        
        ### step D5: 
        ### predict final output, at mean eCO2 values
        ### print out the final predicted results 
        ### check if the Rhet is OK?
        predict_final_output(pChain = pChain.eCO2, 
                             obs = eco2DF[4,],
                             return.option = "Check result")
        
        
        #### E: Make aCO2 and eCO2 comparison summaries
        ### compute a output table to summarize parameters and their uncertainties
        make_parameter_summary_table()
    }
}