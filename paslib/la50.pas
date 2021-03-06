PROGRAM la50;
(*LA50 controller*)

$SYSTEM USRLIB

VAR
horizontal_pitch: INTEGER := 1;
vertical_pitch: INTEGER := 1;
density: INTEGER := 1;
page_length: INTEGER := 66;

BEGIN
  start('LA50');
  IF NOT askyn('Standard settings?') THEN BEGIN
    CASE asktab('Horizontal:', ('NORMAL','40','48','66','80','96','132')) OF
      1: horizontal_pitch := horizontal_pitch;
      2: horizontal_pitch := 5;
      3: horizontal_pitch := 6;
      4: horizontal_pitch := 8;
      5: horizontal_pitch := 1;
      6: horizontal_pitch := 2;
      7: horizontal_pitch := 4;
    END;
    CASE asktab('Vertical:', ('NORMAL', '22','33','44','66','88','132')) OF
      1: vertical_pitch := vertical_pitch;
      2: vertical_pitch := 4;
      3: vertical_pitch := 5;
      4: vertical_pitch := 6;
      5: vertical_pitch := 1;
      6: vertical_pitch := 2;
      7: vertical_pitch := 3;
    END;
    CASE asktab('Density:', ('NORMAL', 'HIGH', 'LOW')) OF
      1: density := density;
      2: density := 1;
      3: density := 2;
    END;
  END;
  CASE vertical_pitch OF
    1: page_length := 66;
    2: page_length := 88;
    3: page_length := 132;
    4: page_length := 22;
    5: page_length := 33;
    6: page_length := 44;
  END;
  lpton;
  wrchrs((0, 27, 91, horizontal_pitch + ORD('0'), 119));
  wrchrs((0, 27, 91, vertical_pitch + ORD('0'), 122));
  wrchrs((0, 27, 91, page_length, 116));
  wrchrs((0, 27, 91, density + ORD('0'), 34, 122));
  lptoff
END.
   