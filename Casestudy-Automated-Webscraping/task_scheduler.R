# Task Scheduler
#=====================


taskscheduler_create(taskname = "auto_scrape", 
                     rscript = scheduled_script_path,
                     schedule = "MINUTE", 
                     starttime = format(Sys.time(), "%H:%M:%S"), 
                     startdate = format(Sys.time(), "%m/%d/%Y"))

taskscheduler_stop("auto_scrape")
taskscheduler_delete("auto_scrape")
