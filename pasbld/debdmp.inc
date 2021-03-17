(* DEBDMP.INC - external declarations for public routines of the Pascal
   Debugger module DEBDMP.  *)

external procedure dmp$breaks (var status_code);
external procedure dmp$files  (augmntd_source_id; var status_code);
external procedure dmp$location;
external procedure dmp$modules;
external procedure dmp$scope  (scope_type; var status_code);
external procedure dmp$stack  (stack_level);
external procedure dmp$pages  (augmntd_source_id; var status_code);
