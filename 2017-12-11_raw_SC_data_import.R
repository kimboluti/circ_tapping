# Import data for paced (synchronize-continue) motor tempo tasks
# Run prior to task-specific script(s) (separate files)


# set up------------------------------
    
    # VALUES TO UPDATE:
    # input -- choose IU or MSU data
    
    # IU data
    filename <- 'data raw/2017-12-11_IU_SC_P246_export.txt' # Update with current merged, exported e-dat file that will be imported as data
    
    # MSU data 
    filename <- 'data raw/2017-12-11_MSU_SC_P100_export.txt'
    
    # outputs...
    # maintain rawdata df in R environment for use in task-specific scripts
    
    
    # Load libraries 
    library(plyr)
    library(stringi)
    library(stringr)
    library(dplyr)
    library(tidyr)
    library(tidyverse)

    
# load data & basic clean up------------------------------
    
    rawdata_raw <- read.delim(filename) 
    
    rawdata <- rawdata_raw %>%
      # # MSU ONLY: participants with missing Procedure.Block. info
      # filter(Subject != 19 & Subject != 28 & Subject != 34 & Subject != 45) %>%
      # clean up task type and block count
      mutate(task = ifelse(Procedure.Block. == "FastBlockProc", "fast",
                           ifelse(Procedure.Block. == "SlowBlockProc", "slow",
                                  ifelse(Procedure.Block. == "SMTBlockProc", "smt",""))),
             block = ifelse(task == "fast", FastBlock,
                            ifelse(task == "slow", SlowBlock,
                                   ifelse(task == "smt" & ExperimentName == "SMT1&TLimits_ard_audioInstruct", 1,
                                          ifelse(task == "smt" & ExperimentName == "SMT1&TLimits_ard_audioInstruct_slowTapTimeout", 1,
                                                 ifelse(task == "smt" & ExperimentName == "SMT2&3_ard_audioInstruct_mod", SMTBlock+1, "")))))) %>%
      # generic re-naming and column selection
      transmute(subject = as.integer(Subject),
                task = as.factor(task),
                block = as.factor(block),
                tap_index = as.numeric(SubTrial),
                tap_int = as.numeric(Tap.dt),
                tap_series = as.integer(Tap.t))
    
    rm(rawdata_raw) # optional to keep environment tidy
    
    # quick view of data
    rawdata %>%
      ggplot(aes(x=tap_int)) +
      geom_histogram(binwidth = 100) + coord_cartesian(xlim=c(0,3500)) + 
      facet_grid(block ~ task, scales = "free")
    
    # # msu empty task and block mystery
    # rawdata %>% filter(task=="") %>%
    #   ggplot(aes(x=tap_index, y=tap_int)) + geom_point() +
    #   facet_grid(subject ~ task, scales = "free")
    # 
    # rawdata_raw %>% filter(Procedure.Block.=="") %>%
    #   ggplot(aes(x=SubTrial, y=Tap.dt, group=SMTBlock, color=SMTBlock)) + geom_point() +
    #   facet_grid(Subject ~ ExperimentName, scales = "free")
    # 
    # tmp <- rawdata_raw %>% filter(Procedure.Block.=="")
    # write.csv(tmp, "2017-11-29_MSU_taskblock_issue.csv")
    
# next, run task-specific script (separate file)
