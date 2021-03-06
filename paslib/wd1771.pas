MODULE wd1771;
  (*simulator for WD1771 disk controller*)

TYPE
  status_flag = (not_ready, write_protect, head_loaded, seek_error,
    crc_error, track_zero, index, busy, write_fault, record_not_found,
    lost_data, data_request);
  byte = 0 .. 255;
  data_num = byte;
  track_num = byte;
  sector_num = byte;
  mark_num = 0 .. 3;
  sector_length_multiplier = 0 .. 3; (*X128 if block else X16*)
  step_code = 0 .. 3; (*6, 6, 10, 20 if clock=2mhz*)
  bit: 0 .. 1;

PROCEDURE restore
  (head_load, verify_track: bit; step_rate: step_code);
  (*restore to track zero*)
  BEGIN
    END;

PROCEDURE seek
  (track: track_num; head_load, verify_track: bit; step_rate: step_code);
  (*seek to specified track*)
  BEGIN
    END;

PROCEDURE step
  (update_track, head_load, verify_track: bit; step_rate: step_code);
  (*step to next track in same direction*)
  BEGIN
    END;

PROCEDURE step_in
  (update_track, head_load, verify_track: bit; step_rate: step_code);
  (*step in to next track*)
  BEGIN
    END;

PROCEDURE step_out
  (update_track, head_load, verify_track: bit; step_rate: step_code);
  (*step out to next track*)
  BEGIN
   END;

PROCEDURE read_sector
  (sector: sector_num; VAR mark: mark_num; multiple, block, head_load: bit);
  (*read specified sector from current track*)
  BEGIN
    END;

PROCEDURE write_sector
  (sector: sector_num; mark: mark_num; multiple, block, head_load: bit);
  (*write sector to current track*)
  BEGIN
    END;

PROCEDURE read_address
  (VAR track: track_num);
  (*read current track address*)
  BEGIN
    END;

PROCEDURE read_track
  (am_sync: bit);
  (*read entire current track*)
  BEGIN
    END;

PROCEDURE write_track;
  (*write entire current track*)
  BEGIN
    END;

PROCEDURE force_interrupt
  (not_ready_to_ready, ready_to_not_ready, index, immediate: bit);
  (*force an interrupt on the specified conditions*)
  BEGIN
    END;

PROCEDURE check_status
  (VAR status: SET OF status_flag);
  (*check status of last command*)
  BEGIN
    END;

FUNCTION check_interrupt: BOOLEAN;
  (*check for interrupt*)
  BEGIN
    END;

FUNCTION check_data_request: BOOLEAN;
  (*check for data request*)
  BEGIN
    END;

PROCEDURE read_data(VAR data: data_num);
  (*read one byte of data*)
  BEGIN
    END;

PROCEDURE write_data(data: data_num);
  (*write one byte of data*)
  BEGIN
    END;

PROCEDURE wd1771;
  (*cycle wd1771 simulation*)
  BEGIN
    END.
  