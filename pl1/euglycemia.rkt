#lang pop-pl

// Device fields initialized here are intended as outputs,
// which a real device would produce an a `scenario` blocks
// sets inside `events`. Device inputs are not declared here
// and are intended to be set to defaults below.

  device feeding_tube { blocked = false, fallen_out = false }
  device insulin_infusion { interrupted = false }
  device glucose_reading { date = 0, value = 50 }
  device clock { now = 0 }

// Protocols can be signaled by `whenever` blocks:

  import initiate_stale_glucometry_notification_protocol;
  import hypoglycemia_resuscitation_protocol;
  import hypoglycemia_notification_protocol;

// A scenario is a kind of test case. All top-level
// bindings are remembered and restored after the
// scenario. Similarly, all device settings are reset
// to defaults as the beginning of an `events` block.
// An `events` block sets device values and
// then runs a `whenever`s pass.

  scenario {
    assert glucose_reading.rate = 6 hours;
    assert insulin_infusion.rate = 80 mL/hour;

    events {
      feeding_tube.blocked <- true;
    }
    assert insulin_infusion.rate = 1 mL/hour;
    assert signaled hypoglycemia_notification_protocol;
    assert not signaled initiate_stale_glucometry_notification_protocol;

    events {
      feeding_tube.blocked <- false;
    }
    assert insulin_infusion.rate = 80 mL/hour;

    events {
     clock.now <- clock.now + 10;
    }
    assert signaled initiate_stale_glucometry_notification_protocol;
  }

============================================================

// Device inputs here act as defaults that are adjusted
// by `whenever` blocks.

  glucose_reading.rate <- 6 hours;
  
  feeding_tube.rate <- 80 mL/hour;
  feeding_tube.duration <- 16 hours;
  feeding_tube.daily_start_time <- 0600;

  insulin_infusion.rate <- 80 mL/hour;
  insulin_infusion.duration <- 16 hours;
  insulin_infusion.daily_start_time <- 0600;
  

// The order of `whenever` clasues matters, since a variable
// or device field cannot be written after it is read on
// a given pass through the triggers.
// Similarly, conflicting writes signal an error.

  whenever(feeding_tube.blocked || feeding_tube.fallen_out) {
    insulin_infusion.rate <- 1 mL/hour;
    glucose_reading.rate <- 2 hours;
  }
   
  whenever(feeding_tube.rate < 80 mL/hour) {
    insulin_infusion.rate <- feeding_tube.rate * 42;
    glucose_reading.rate <- 2 hours;
  }
  
  whenever(insulin_infusion.interrupted) {
    feeding_tube.rate <- 20mL/hour;
    glucose_reading.rate <- 2 hours;
  }
  
  whenever(glucose_reading.date + glucose_reading.rate < clock.now) {
    initiate_stale_glucometry_notification_protocol();
  }
  
  whenever(glucose_reading.value < 40) {
    hypoglycemia_resuscitation_protocol();
  }
  
  whenever(40 <= glucose_reading.value < 60) {
    hypoglycemia_notification_protocol();
  }
