#
# script for circular analysis of nih-spin sc data
# 2017-12-11 updating (older version maintains data format review steps eliminated here)
# 2016-08-31 E.K. Fromboluti (frombo@msu.edu)
# 
# Prior to running script (see also spin_sc_circ_processing_protocol_083116):
# - Merge E-Data Aid Files
# - Export E-Data Aid Files (leave on first option, SPSS, but uncheck unicode)
# - ?? read data in via 2017-12-11_raw_SC_data_import.R script (or just do that here...)
#
#     
# TO DO ------------------------------
(0) circ summary measures II!
(1) Make sense of the ep/ino time audit measures (section started in script)
(2) Continuation lag assemssment (section started in script)
(3) Look at improvement across blocks
(4) ...

# set up------------------------------
  # VALUES TO UPDATE:
  # input -- choose IU or MSU data
  
  # IU data
  filename <- 'data raw/2017-12-11_IU_SC_P246_export.txt' # Update with current merged, exported e-dat file that will be imported as data
  
  # MSU data 
  filename <- 'data raw/2017-12-11_MSU_SC_P100_export.txt'
  
  # OUTPUTS
  output_sync_orig_flagged_filename <- '2017-12-11_MSU_N=82_all_sync.csv' # rename descriptively...all data with flags?
  output_sync_filt_filename <- '2017-12-11_MSU_N=82_filtered_sync.csv' # rename descriptively...filtered data only?
  
  # Load libraries 
  library(circular)
  library(plyr)
  library(stringi)
  library(stringr)
  library(dplyr)
  library(tidyr)
  library(tidyverse)


# load data & clean up------------------------------
      
    rawdata_raw <- read.delim(filename) 

      rawdata <- rawdata_raw %>%
        # generic re-naming and column selection
        transmute(subject = as.integer(Subject),
                  block = as.factor(Procedure.Block.),   # block: 1, 2, or 3
                  proc = as.factor(Procedure.SubTrial.), # procedure: sync or contin
                  pace = as.numeric(gsub("pace","",paceFile)),
                  tap_index = as.numeric(LogLevel5),
                  tap_int = as.numeric(Tap.dt),
                  tap_series = as.integer(Tap.t),
                  start_ino = Tap.StartTime_us/1000, # converting to ms
                  start_ep = Tap.StartTime_us.EP/1000,
                  done_ino = Tap.DoneTime_us/1000,  
                  done_ep = Tap.DoneTime_us.EP/1000,
                  sync_onset = SyncSlide.OnsetTime,
                  sync_offset = SyncSlide.OffsetTime,
                  sync_finish = SyncSlide.FinishTime,
                  cont_onset = ContSlide.OnsetTime,
                  cont_offset = ContSlide.OffsetTime,
                  cont_finish = ContSlide.FinishTime,
                  refresh_rate = Display.RefreshRate,
                  tap_string = Tap.ResultsStr) %>% # redundant, but might as well transfer to R df in case
       arrange(subject)
     
  rm(rawdata_raw)
      
# sync taps: with "include" filter columns for data analysis ----
        sync_data_orig <- rawdata %>% 
          filter(proc == 'SyncProc') %>%
          # includes by criterion (for summary of exclusion by crit)
          mutate(tap_incl_rmfirst = ifelse(tap_index == 1, 0, 1),
                 tap_incl_rm50ms = ifelse(tap_int < 50, 0, 1),
                 tap_incl_rm.5 = ifelse(tap_int < .5*pace, 0, 1),
                 tap_incl_rm1.5 =ifelse(tap_int > 1.5*pace, 0, 1)) %>%
          # include summary column
          mutate(tap_include = ifelse(tap_index == 1, 0, # exclude if first tap of trial
                                  ifelse(tap_int < 50, 0, # exclude if tap is < 50ms
                                         ifelse(tap_int < .5*pace, 0, # exclude if < 50% target rate
                                                ifelse(tap_int > 1.5*pace, 0, 1))))) %>% # exclude if > 150% target rate
          mutate(tap_series_orig = tap_series, # retain original tap timeseries in a new variable
                 tap_series = tap_series_orig - 20) %>% # adjust tap timeseries by 20-ms for lag embedded in metronome audio
          select(subject, block, proc, pace, tap_index, tap_int, tap_series_orig, tap_series,
                 tap_include, tap_incl_rmfirst, tap_incl_rm50ms, tap_incl_rm.5, tap_incl_rm1.5) %>%
          na.omit() 

        write.csv(sync_data_orig, output_sync_orig_flagged_filename)
        
        # for filtered (preprocessed) data
        sync_data <- sync_data_orig %>%
          filter(tap_include == 1) %>%
          select(subject, block, proc, pace, tap_index, tap_int, tap_series_orig, tap_series)

        write.csv(sync_data, output_sync_filt_filename)

        # preprocessing exclusion summary - not doing this. 
        # info is available in sync_data_orig; allows for flexible filtering (e.g, by P, or by pace)



    # CONTINUE taps: select and clean-up
        # to do: filters?
        cont_data <- rawdata %>% 
          select(subject,block,proc,pace,tap_index,tap_int,tap_series, # data
                 start_ino,start_ep,done_ino,done_ep, # arduino communication timestamps
                 cont_onset,cont_offset,cont_finish) %>% # ep slide object timestamps
          filter(proc == 'ContinProc') %>%
          na.omit() 
      
        

# tap count summaries ------------------------------   

  ## ALL DATA

  # counts of number of taps by subject, pace (150::1709), block (1,2,3), proc (s,c)
  raw_counts <- rawdata %>%
    group_by(subject, pace, block, proc) %>%
    summarize(n=n()) %>%
    na.omit()
  
  # spread pace across participants for easier viewing
  wide_raw_counts <- raw_counts %>%
    spread(pace,n)
  
  # make a list of 'flagged' participants based on filter criterion
  flag_list <- raw_counts %>%
    group_by(subject) %>%
    filter(n < 20) %>%
    spread(pace,n)
  
  ## SYNC DATA COUNTS
  
  ## CONTIN DATA COUNTS

           

# synchronization taps ------------------------------

  # basic info about sync data set ------------------------------
    
    # number of taps by P by pace by block
    sync_counts <- sync_data %>%
      group_by(subject, pace, block) %>%
      summarize(n=n()) %>%
      spread(pace,n)


    # list of participants(blocks) who should probably be excluded
    # based on small number of taps
    sync_flag <- sync_data %>%
      group_by(subject, pace, block) %>%
      summarize(n=n()) %>%
      filter(n < 20) %>%
      spread(pace,n)
      

# circular tap dataframe ------------------------------  
  # modeling from excel sample
  # converts timeseries to circular 
  
    sync_circ <- sync_data %>%
      mutate(mod_remainder = tap_series %% pace,
             remainder_norm = mod_remainder/pace, # modulo remainder normalized to 0,1 by pace
             degrees = remainder_norm*360,
             radians = rad(degrees),
             phase_deg = ifelse(degrees > 180,  
                                ((degrees-360)/360),
                                (degrees/360)),
             x = cos(radians),
             y = sin(radians),
             x2 = cos(2*radians),
             y2 = sin(2*radians)) %>%
      mutate(pace = as.factor(pace))

      # some plots to explore
      plot(sync_circ$pace,sync_circ$remainder_norm)
      plot(sync_circ$pace,sync_circ$degrees)
      plot(sync_circ$pace,sync_circ$radians)
      plot(sync_circ$pace,sync_circ$phase_rad) # this looks like the most 'useful' for assessing phase
    
      # okay: boxplot of phase by pace (by block)
      ggplot(sync_circ, aes(x = pace, y = phase_rad)) +
        geom_boxplot() +
        facet_grid(.~block) +
        theme_bw()
    
      # ok: plot x,y circle coords
      ggplot(sync_circ, aes(x = x, y = y)) +
        geom_point() +
        facet_grid(pace~block)

      # working/not useful: plots phase by tap number for all participants by block by pace
      ggplot(sync_circ, aes(x = tap_index, y = phase_rad, group = subject, color = pace)) +
        geom_point() +
        geom_line() +
        labs(x = 'tap number', 
             y = 'phase (radians)') + 
        ggtitle('Phase Timeseries plot (MSU 8-17-16)') +
        facet_grid(pace~.) +
        theme_bw()


# filter BY PARTICIPANT 11 to check against excel version from lab --------------------------
    check_lab <- sync_circ %>%
      filter(subject == 11, pace == 759) %>% #block == "SyncContBlockProc1"
      mutate(pace = as.factor(pace))
      
      # plot ITIs
      ggplot(check_lab, aes(x = tap_index, y = tap_int)) +
        geom_point() +
        geom_line() +
        labs(x = 'tap number', 
             y = 'tap interval (ms)') + 
        ggtitle('Tap Intervals (P11, sync, pace 759)') +
        facet_grid(block~pace) +
        theme_bw()

      # recreates unit circle plot in excel
      ggplot(check_lab, aes(x = x, y = y)) +
        geom_point() +
        labs(x = 'cosine(theta)', 
             y = 'sine(theta)') + 
        ggtitle('Circular Plot (P11, Sync, Pace 759)') +
        facet_grid(block~pace) +
        theme_bw()
      
      # phase by tap number
      ggplot(check_lab, aes(x = tap_index, y = phase_deg, group = subject, color = pace)) +
        geom_point() +
        geom_line() +
        coord_cartesian(ylim=c(-.5, .5), xlim=c(0,31)) +        
        labs(x = 'tap number', 
             y = 'phase') + 
        ggtitle('Phase Timeseries plot (P11, Sync, Pace 759)') +
        facet_grid(block~pace)+
        theme_bw()


  
  
  # summary by participant for each pace
  sync_p_summary <- sync_circ %>%
    group_by(subject,pace) %>%
    summarize(mean_iti = mean(tap_int), sd_iti = sd(tap_int), 
              mean_deg = mean(degrees), sd_deg = sd(degrees),
              mean_rad = mean(radians), sd_rad = sd(radians),
              mean_phase = mean(phase_deg), sd_phase = sd(phase_deg),
              mean_x = mean(x), sd = sd(x),
              mean_y = mean(y), sd = sd(y))

    # USEFUL TEST: summary by tap number for each pace
    sync_index_summary <- sync_circ %>%
      group_by(pace, tap_index) %>%
      summarize(mean_iti = mean(tap_int), sd_iti = sd(tap_int), 
                mean_deg = mean(degrees), sd_deg = sd(degrees),
                mean_rad = mean(radians), sd_rad = sd(radians),
                mean_phase = mean(phase_deg), sd_phase = sd(phase_deg),
                mean_x = mean(x), sd = sd(x),
                mean_y = mean(y), sd = sd(y))
    
          ggplot(sync_index_summary, aes(x = tap_index, y = mean_phase, color = pace)) +
            geom_point() +
            geom_line() +
            coord_cartesian(ylim=c(-.5, .5), xlim=c(0,31)) +        
            labs(x = 'tap number', 
                 y = 'phase') + 
            ggtitle('Phase Timeseries plot (MSU, N = 82') +
            facet_grid(pace~.)
            theme_bw()

  # WORKING: summary across participants...
    sync_summary <- sync_p_summary %>%
    group_by(pace) %>%
    summarize(mean_iti = mean(mean_iti), sd_iti = sd(tap_int), 
              mean_deg = mean(degrees), sd_deg = sd(degrees),
              mean_rad = mean(radians), sd_rad = sd(radians),
              mean_phase = mean(phase_rad), sd_phase = sd(phase_rad),
              mean_x = mean(x), sd = sd(x),
              mean_y = mean(y), sd = sd(y))
    

# CIRCULAR SUMMARY MEASURES 1 (based on McAuley Circ Workbook) ----------------------------
# by participant, by rate, by block

circ_summary_I <- sync_circ %>%
  group_by(subject, pace, block) %>%
  summarize(x_bar = mean(x),
            y_bar = mean(y),
            R_bar = sqrt(x_bar*x_bar + y_bar*y_bar),
            V = (1-R_bar),
            v_lil = (sqrt(-2*log(1-V))))

      check_lab_summary <- sync_circ %>%
        filter(subject == 11, pace == 759) %>%
        mutate(pace = as.factor(pace)) %>%
        group_by(subject, pace, block) %>%
        summarize(x_bar = mean(x),
                  y_bar = mean(y),
                  R_bar = sqrt(x_bar*x_bar + y_bar*y_bar),
                  V = (1-R_bar),
                  v_lil = (sqrt(-2*log(1-V))))
      
      all759_summary <- sync_circ %>%
        filter(pace == 759) %>%
        mutate(pace = as.factor(pace)) %>%
        group_by(subject, pace, block) %>%
        summarize(x_bar = mean(x),
                  y_bar = mean(y),
                  R_bar = sqrt(x_bar*x_bar + y_bar*y_bar),
                  V = (1-R_bar),
                  v_lil = (sqrt(-2*log(1-V))))
    
    # version in lab script -- includes measures from summaries I and II in McAuley workbook
    circ_summary_long <- sync_circ %>%
      group_by(subject, pace, block) %>%
      summarize(x_bar = mean(x),
                y_bar = mean(y),
                R_bar = sqrt(x_bar*x_bar + y_bar*y_bar),
                V = (1-R_bar),
                v_lil = (sqrt(-2*log(1-V))),
                x2_bar = mean(x2),
                y2_bar = mean(y2),
                R2_bar = sqrt(x2_bar*x2_bar + y2_bar*y2_bar),
                n = n()) %>%
      mutate(d_hat = (1-R2_bar)/(2*R_bar*R_bar),
             d_hat_se = sqrt(d_hat/n),
             th_bar = ifelse(x_bar < 0, atan(y_bar/x_bar) + pi,
                             ifelse(x_bar > 0 & y_bar >= 0, atan(y_bar/x_bar),
                                    ifelse(x_bar == 0 & y_bar > 0, pi/2,
                                           ifelse(x_bar >= 0 & y_bar < 0, atan(y_bar/x_bar) + 2*pi, NA)))))
    
    # confidence intervals generating many NaNs for some reason; add back to mutate when i figure that out...
    #          ci_lo = th_bar - asin(1.9604*d_hat_se),
    #          ci_hi = th_bar + asin(1.9604*d_hat_se))         
    
    # ?necessary? convert to wide format...necessary? depends how block mean is calculated...
    # circ_summary <- circ_summary_long %>% 
    #   separate(x_bar, c('b1','b2','b3')) 



      # check_lab: plot on circle - big dots for each block R_bar  
      ggplot(check_lab_summary, aes(x = x_bar, y = y_bar, color = block)) +
        geom_point(size = 5, alpha = .75) +
        geom_point(data = check_lab, aes(x=x,y=y)) +
        labs(x = 'cosine(theta)', 
             y = 'sine(theta)') + 
        ggtitle('Circular Summary (P11, Sync, Pace 759)') +
        #facet_grid(pace~block) +
        theme_bw()

    # all759_summary: plot on circle - big dots for each block R_bar  
    ggplot(all759_summary, aes(x = x_bar, y = y_bar, color = block)) +
      geom_point(size = 5, alpha = .75) +
      geom_point(data = check_lab, aes(x=x,y=y)) + #note: this part not all ps - just p11
      labs(x = 'cosine(theta)', 
           y = 'sine(theta)') + 
      ggtitle('Circular Summary (n=82, Sync, Pace 759)') +
      #facet_grid(pace~block) +
      theme_bw()

    # circ_summary_I: plot on circle - big dots for each block R_bar  
    ggplot(circ_summary_I, aes(x = x_bar, y = y_bar, color = block)) +
      geom_point(size = 5, alpha = .75) +
      geom_point(data = sync_circ, aes(x=x,y=y)) +
      labs(x = 'cosine(theta)', 
           y = 'sine(theta)') + 
      ggtitle('Circular Summary (n=82, Sync, All paces)') +
      facet_grid(pace~block) +
      theme_bw()

    # WORKING: density plot - looks kinda cool, but useful?
    ggplot(circ_summary_I, aes(x = x_bar, y = y_bar, color = block)) +
      geom_density2d()



# CIRCULAR SUMMARY MEASURES 2 ----------------------------
# by participant, by rate, vector-averaged across by-block calculations
# starts with circ_summary_long
#
    # to do: compare to calculating across all blocks (instead of taking mean of by-block calculations)
    
    circ_summary_2_long <- circ_summary_long %>%
      group_by(subject, pace) %>%
      summarize(x_bar_mean = mean(x_bar),
                y_bar_mean = mean(y_bar),
                R_bar_mean = sqrt(x_bar_mean*x_bar_mean + y_bar_mean*y_bar_mean),
                V_mean = (1-R_bar_mean),
                v_lil = (sqrt(-2*log(1-V_mean))),
                x2_bar_mean = mean(x2_bar),
                y2_bar_mean = mean(y2_bar),
                R2_bar_mean = sqrt(x2_bar_mean*x2_bar_mean + y2_bar_mean*y2_bar_mean)) %>%         
      mutate(d_hat_mean = (1-R2_bar_mean)/(2*R_bar_mean*R_bar_mean),
             d_hat_se_mean = sqrt(d_hat_mean/3), # probably not done correctly...n=3 for 3 blocks
             th_bar_mean = ifelse(x_bar_mean < 0, atan(y_bar_mean/x_bar_mean) + pi,
                                  ifelse(x_bar_mean > 0 & y_bar_mean >= 0, atan(y_bar_mean/x_bar_mean),
                                         ifelse(x_bar_mean == 0 & y_bar_mean > 0, pi/2,
                                                ifelse(x_bar_mean >= 0 & y_bar_mean < 0, atan(y_bar_mean/x_bar_mean) + 2*pi, NA)))))
    
    # convert to wide format for Excel database & SPSS
    circ_summary_2 <- circ_summary_2_long %>%
      select(subject, pace, R_bar_mean, V_mean, v_lil, d_hat_mean, d_hat_se_mean, th_bar_mean) %>%
      gather(variable,value,-(subject:pace)) %>%
      unite(temp,pace,variable) %>%
      spread(temp,value)
    write.csv(circ_summary_2, output_sync_circ_summary_2_fname)
    
    # some representative variables selected from the summary
    R_summary <- circ_summary_2_long %>%
      select(subject, pace, R_bar_mean) %>%
      spread(pace, R_bar_mean)
    write.csv(R_summary, output_R_summary_fname)
    
    theta_summary <- circ_summary_2_long %>%
      select(subject, pace, th_bar_mean) %>%
      spread(pace, th_bar_mean)
    write.csv(theta_summary, output_theta_summary_fname)




# sync audio to tap offsets------------------------------
# confusion. not making sense (8-26-16)

    # start with data frames: sync_circ or sync_data
    
    offsets <- sync_circ %>%
      mutate(ep_begin_offset = abs(start_ep - sync_onset),
             ep_end_offset = abs(done_ep - sync_offset),
             ep_end_offset_2 = abs(done_ep - sync_finish),
             start_offset = abs(start_ino - start_ep),
             done_offset = abs(done_ino - done_ep),
             trial_dur_ino = abs(done_ino - start_ino),
             trial_dur_ep = abs(done_ep - start_ep),
             trial_dur_offset = abs(trial_dur_ino - trial_dur_ep))
  
    offset_summary <- offsets %>%
      group_by(subject, pace, block) %>%
      summarize(ep_begin_offset_mean = mean(ep_begin_offset),
                ep_end_offset_mean = mean(ep_end_offset),
                ep_end__offset_2_mean = mean(ep_end_offset_2),
                start_offset_mean = mean(start_offset),
                done_offset_mean = mean(done_offset),
                trial_dur_ino_mean = mean(trial_dur_ino),
                trial_dur_ep_mean = mean(trial_dur_ep),
                trial_dur_offset_mean = mean(trial_dur_offset))
        
    offset_summary_by_pace <- offset_summary %>%
      group_by(pace) %>%
      summarize(mean_start = mean(start_offset),
                mean_done = mean(done_offset),
                mean_dur = mean(trial_dur_offset)) %>%
      mutate(expected_dur = 31*c(150,225,337,506,759,1139,1709),
             diff_dur = expected_dur - mean_dur)
    
    # check lag from ep "onset" to ep "tap start"
    ggplot(offset_summary, aes(x = pace, y = ep_begin_offset_mean)) +
      geom_point() +
      #coord_cartesian(ylim=c(0, 500)) +        
      labs(x = 'pace IOI (ms)', 
           y = 'ep start lag (ms)') + 
      ggtitle('E-Prime Start time lags (MSU 8-17-16, N = 48') +
      facet_grid(.~block)
      theme_bw()


    # plot start offsets. predict: close to 0
    ggplot(offsets, aes(x = pace, y = start_offset)) +
      geom_boxplot() +
      #geom_line() +
      #coord_cartesian(ylim=c(0, 500)) +        
      labs(x = 'pace IOI (ms)', 
           y = 'start offset (ms)') + 
      ggtitle('Arduino To E-Prime Start time offsets (MSU 8-17-16, N = 48') +
      #facet_grid(pace~block)
      theme_bw()

    # plot done offsets. predict: close to 0
    ggplot(offsets, aes(x = pace, y = done_offset)) +
      geom_boxplot() +
      #geom_line() +
      #coord_cartesian(ylim=c(0, 500)) +        
      labs(x = 'pace IOI (ms)', 
           y = 'done offset (ms)') + 
      ggtitle('Arduino To E-Prime Done time offsets (MSU 8-17-16, N = 48') +
      #facet_grid(pace~block)
      theme_bw()

    # plot trial duration offsets. predict: close to 0
    ggplot(offsets, aes(x = pace, y = trial_dur_offset)) +
      geom_boxplot() +
      #geom_line() +
      #coord_cartesian(ylim=c(0, 500)) +        
      labs(x = 'pace IOI (ms)', 
           y = 'trial duration offset (ms)') + 
      ggtitle('Arduino To E-Prime Trial Duration time offsets (MSU 8-17-16, N = 48') +
      #facet_grid(pace~block)
      theme_bw()



# continuation taps------------------------------
# from it03 -- to be edited for nih spin

        c_taps <- paced_data %>%
          filter(tap_type == 'ContinProc') #%>%
       
        # check single participant
        c_taps_4400 <- c_taps %>%
          filter(chain_name == '4400')
         
        # taps by seed by chain
        ggplot(c_taps, aes(x = tap_number, y = tap_int, group = seed, color = seed)) +
          geom_point() +
          #geom_line() +
          facet_grid(.~chain_name)
        
        # taps by seed by chain by generation  
        ggplot(c_taps, aes(x = tap_number, y = tap_int, group = seed, color = seed)) +
          geom_point() +
          geom_line() +
          coord_cartesian(ylim=c(0, 4000)) +
          facet_grid(chain_name~generation)

        # SINGLE PARTICIPANT CHECK: taps by seed by chain by generation  
        ggplot(c_taps_4400, aes(x = tap_number, y = tap_int, group = seed, color = seed)) +
          geom_point() +
          geom_line() +
          facet_grid(chain_name~generation)
        
        
        
        c_tap_summary <- paced_data %>%
          filter(tap_type == 'ContinProc') %>%
          group_by(chain_name, seed, generation) %>% 
          summarize(avg_rate = mean(tap_int), sd_rate = sd(tap_int)) 
        
        # check selection of Ps or single participant:
        c_tap_summary_no500 <- c_tap_summary %>%
          filter(chain_name != '500')
        
        # change in produced rate each generation
        # to add - facet by condition (need to add condition column)
        ggplot(c_tap_summary, aes(x = generation, y = avg_rate, group = seed, color = seed)) +
          geom_point() + 
          geom_line() +
          coord_cartesian(ylim=c(0, 4000)) +
          facet_grid(chain_name~.) +
          theme_bw()
        
        # variability (SD of taps)
        ggplot(c_tap_summary, aes(x = generation, y = sd_rate, group = seed, color = seed)) +
          geom_point() + 
          geom_line() +
          facet_grid(chain_name~.) +
          theme_bw()
        
        # produced rate by seed
        ggplot(c_tap_summary, aes(x = seed, y = avg_rate, group = chain_name, color = chain_name)) +
          geom_point() + 
          #geom_line() +
          #facet_grid(type~mode) + 
          theme_bw()

        
        
        # DRIFT
        
        

        
        
        
        
        
        
        
        

