$PAGE FIO.INC, last modified 5/11/84, zw
$INCLUDE fio.typ
EXTERNAL
PROCEDURE fio_open (VAR file_block; PACKED ARRAY [1..*] OF CHAR);
EXTERNAL
PROCEDURE fio_attach (VAR file_block; TEXT);
EXTERNAL
PROCEDURE fio_close (VAR file_block);
EXTERNAL
PROCEDURE fio_page (VAR file_block);
EXTERNAL
PROCEDURE fio_skip (VAR file_block);
EXTERNAL
PROCEDURE fio_nskip ( VAR file_block; INTEGER; INTEGER );
EXTERNAL
PROCEDURE fio_tab (VAR file_block; fio_width);
EXTERNAL
PROCEDURE fio_write (VAR file_block; PACKED ARRAY [1..*] OF CHAR);
EXTERNAL
PROCEDURE fio_nop (VAR file_block);
EXTERNAL
PROCEDURE fio_eject (VAR file_block);
EXTERNAL
PROCEDURE fio_reopen (VAR file_block);
EXTERNAL
PROCEDURE fio_line (VAR file_block; PACKED ARRAY [1..*] OF CHAR);
EXTERNAL
PROCEDURE fio_space (VAR file_block; fio_width);
    