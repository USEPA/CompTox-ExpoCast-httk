# This function calculates the peak concentration given a PK timecourse (concentration vs. time):
calc_timecourse_peak <- function(timecourse,conc.col=2,time.col=1)
{
  return(set_httk_precision(max(timecourse[,conc.col])))
}
