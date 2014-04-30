#lang pop-pl

// example to illustrate multiple read/write

  device pain_meter { value = 8 unit }
  device unpainifier { value = 20 mL/hour }

  import notify_nurse;

  scenario {
    events {
     pain_meter.value <- 8 unit;
    }
    
    assert unpainifier.value = 30 mL/hour;
    events {
     pain_meter.value <- 9 unit;
    }
    //what should happen here?
  }

====================

  whenever ( pain_meter.value >= 8 unit ) {
    unpainifier.value <- unpainifier.value + 10 mL/hour;
  }

  whenever ( unpainifier.value > 30 mL/hour ) {
    unpainifier.value <- 0 mL/hour;
    notify_nurse();
  }
