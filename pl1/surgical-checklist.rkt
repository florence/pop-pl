#lang pop-pl

Boolean in_op_consent = false;
Boolean y = false;
Boolean site_marking_necesssary = true;
Boolean site_marked? = false;
Number Or Unknown patient_weight = unknown;
Boolean site_marking_necessary = true;
Boolean Or Unknown blood_loss_risk = unknown;
Boolean Or Unknown airway_risk? = unknown;
Boolean Or Unknown Or N/A airway_equipment_assistance_available? = unknown;
Boolean Or Unknown meds = unknown;
Boolean Or Unknown anaesthesia_machine_ok? = unknown;
Boolean Or Unknown iv_fluids_planned? = unknown;
Boolean Or Unknown airway_equiptment_assistance_available? = unknown;

Boolean abort = false;
============================================================

when (not(anaesthesia_machine_ok?) or not(iv_fluids_planned?) or not(airway_equipment_assistance_available?))
  prompt(red,abort,"Abort!");

prompt(red, in_op_consent, "Has the patient confirmed identity @{patient.name}, site @{procedure.site}, procedure @{procedure.name}, and consent");
when (in_op_consent) {
  if (site_marking_necessary) then {
      prompt(red,site_marked?,"Is the site marked?");
      prompt(blue,site_marked?,"Is the site marked?");
   } else
     site_marked? <- true; 
   fi
       
   prompt(blue,
          meds,
          "Are the medications @{procedure.medication} available?");
    
    prompt(blue,
           anaesthesia_machine_ok?,
           "Is the anaesthesia machine check done?");
    
    prompt(red,
           airway_risk?,
           "Difficult airway or aspiration risk?");                             

    prompt(blue,
           blood_loss_risk,
           if (known?(patient_weight))
              then "Is there risk of > @{min(500ml, patient.weight * 7ml/kg)} blood loss?"
              else "Is there risk of > 500ml (7ml/kg for children) blood loss?" 
           fi);
  
    when (blood_loss_risk) {
       prompt(blue,
              iv_fluids_planned?,
              "Are two IVs/central access and fluids planned?");
     }
   
  if (airway_risk?) then
    prompt(blue,
           airway_equiptment_assistance_available?,
           "Is airway equipment or assistance available?");
  else
     airway_equiptment_assistance_available? <- n/a;
  fi  
}
