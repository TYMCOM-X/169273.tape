DATAMODULE globel_rpt OPTIONS  XREF;
$INC EXTYPE.INC
PUBLIC  VAR data_saved: BOOLEAN;
PUBLIC  VAR conversion_factor: REAL := 1.0;
PUBLIC  VAR rate_per_mile:     REAL := 0.20;
PUBLIC VAR  exp_data : exp_record;
PUBLIC CONST table_dir : ARRAY [directive]  OF STRING [4] :=
     ('init', 'entr', 'prnt', 'save', 'load', 'quit', 'help', 'badc');
PUBLIC CONST method_of_mpayment_table : ARRAY [method_of_payment] OF 
     STRING [13] :=
          ('employee_paid', 'company_paid ');
PUBLIC CONST day_table : ARRAY [days] OF STRING [3] :=
     ('SUN', 'MON', 'TUE', 'WED', 'THR', 'FRI', 'SAT');
PUBLIC CONST category_table : ARRAY [category] OF STRING [22] :=
     ('AIR FAIR              ',
      'CAR RENTAL            ',
      'PERSONAL AUTO MILAGE  ',
      'PARKING               ',
      'LOCAL TRANSPORTATION  ',
      'LODGING               ',
      'BREAKFAST             ',
      'LUNCH                 ',
      'DINNER                ',
      'ENTERTAINMENT         ',
      'TIPS                  ',
      'TELEPHONE             ',
      'MISCELLANEOUS         ');
END.
 