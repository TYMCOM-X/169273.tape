type
  save_pointer = ^ save_record;
external procedure hp_put ( integer; integer; var save_pointer );
external procedure hp_get ( integer; integer; save_pointer );
external procedure dat_save ( file_name );
external function  dat_get  ( file_name; boolean ): boolean;
