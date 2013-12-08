#lang pop-pl

time glucose_read_rate = 6 hours;
  do glucometry at glucose_read_rate;
  
  feeding_rate = 80 mL/hour;
  feeding_duration = 16 hours;
  feeding_daily_start_time = 0600;
  do enteral_tube_feeding at (feeding_rate feeding_duration feeding_daily_start_time)
  
  insulin_rate = 80 mL/hour;
  insulin_duration = 16 hours;
  insulin_daily_start_time = 0600;
  do regular_insulin_intravenous at (insulin_rate insulin_duration insulin_daily_start_time)
  
  when(feeding_tube.blocked() || feeding_tube.fallen_out()) {
    insulin_infusion.rate = 1 unit/hour;
    glucose_read_rate = 2 hours;
  }
   
  when(feeding_tube.rate() < 80 mL/hr) {
    insulin_infusion.rate = feeding_tube.rate() * ???;
    glucose_read_rate = 2 hours;
  }
  
  when(insulin_infusion.interrupted()) {
    feeding_tube.rate = 20mL/hr;
    glucose_read_rate = 2 hours;
  }
  
  when(glucose_reading.date() + glucose_read_rate < now()) {
    initiate_stale_glucometry_notification_protocol();
  }
  
  when(glucose_reading.value() < 40) {
    Hypoglycemia_resuscitation_protocol();
  }
  
  when(40 <= glucose_reading.value() < 60) {
    hypoglycemia_notification_protocol();
  }
