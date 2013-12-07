#lang pop-pl

Boolean x = false;
Boolean Or Unknown cats = unknown;

============================================================

prompt(red,cats,"Do you like cats?");
when (not(cats)) {
  prompt(blue,x,"Wise.");
}
when (cats) {
  prompt(blue,x,"Which part: the shedding or the aloofness?");            
}
