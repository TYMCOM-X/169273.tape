00001 CONST line_length = 72;
00002       no_of_circuits = 4;
00003       test_line = 'The CAT IN THE HAT comes back(:13:)';
00004 
00005 TYPE line = array [1..line_length] of char;
00006 
00007      device = (disk, tape, printer, system, crash, terminal,
00008                aux, logical_volume);
00009 
00010      io_result = (complete, intervention, transmission, failure,
00011                   end_file, end_medium, start_medium,
00012                   program_error, break_key, orange_ball, busy,
00013                   time_out);
00014  
00015      needle_record = RECORD
00016                        invoice,
00017                        isis_port,
00018                        host_number,
00019                        node_number,
00020                        originating_host,
00021                        port_number,
00022                        user_name_length,
00023                        TID: integer;
00024                        user_name: array[1..14] of char
00025                      END;
00026 
00027      port_operation = (get_char, get_line, send, disconnect,
00028                        clear_break, set_system, set_terminal,
00029                        yellow_ball, detect, build_aux_circuit,
00030                        terminal_info, send_b1_msg, get_needle,
00031                        zap, sup_log_msg, set_break_char,
00032                        set_bell, build_normal_circuit);
00033 
00034      port_param = RECORD
00035                     operation: port_operation;
00036                     status: io_result;
00037                     status2: integer;
00038                     arg1: integer;
00039                     arg2: integer
00040                   END;
00041 
00042      short_line = array [1..40] of char;
00043 
00044      ascii_char = array [1..4] of char;
00045      
00046      ascii_line = array [1..line_length] of ascii_char;
00047  
00048      dump_line = array [1..15] of ascii_char;
00049 
00050      ascii_buffer = array [1..1000] of ascii_char;
00051 
00052 TYPE io_procedure = CLASS;
00053 
00054 PROCEDURE ENTRY build_circuit (io_circuit : integer;
00055                          user_string : line;
00056                          VAR result : io_result;
00057                          VAR error_code : integer);
00058 
00059 VAR data_buffer, logon_string : line;
00060     i, first_blank, string_length, circuit : integer;
00061     aux_param : port_param;
00062 
00063 BEGIN
00064   first_blank := 0;
00065   FOR i := 1 TO 40 DO
00066     IF first_blank = 0 THEN
00067       BEGIN
00068         IF user_string [i] = ' ' THEN
00069           first_blank := i;
00070         logon_string [i] := user_string [i];
00071       END;
00072 
00073   logon_string [first_blank] := '(:13:)';
00074   string_length := first_blank;
00075 
00076   WITH aux_param DO
00077   BEGIN
00078     operation := build_aux_circuit;
00079     arg1 := string_length;
00080     circuit := io_circuit;
00081     data_buffer := logon_string;
00082     
00083     io(data_buffer, aux_param, aux, circuit);
00084 
00085     result := status;
00086     error_code := arg2;
00087   END;
00088 END;
00089 
00090 PROCEDURE ENTRY check_circuit (io_circuit : integer;
00091                                VAR result : boolean);
00092 
00093 VAR data_buffer : line;
00094     circuit : integer;
00095     aux_param : port_param;
00096 
00097 BEGIN
00098   WITH aux_param DO
00099   BEGIN
00100     operation := terminal_info;
00101     circuit := io_circuit;
00102 
00103     io(data_buffer, aux_param, aux, circuit);
00104 
00105     IF (status = complete) OR (status = break_key) THEN
00106       result := TRUE
00107     ELSE
00108       result := FALSE;
00109   END;
00110 END;
00111 
00112 PROCEDURE ENTRY input_line (io_circuit : integer; VAR inline : line;
00113                       VAR inline_length : integer);
00114 VAR data_buffer : line;
00115     circuit : integer;
00116     aux_param : port_param;
00117 
00118 BEGIN
00119   WITH aux_param DO
00120   BEGIN
00121     operation := get_line;
00122     arg1 := line_length;
00123     arg2 := 0;
00124     circuit := io_circuit;
00125 
00126     io(data_buffer, aux_param, aux, circuit);
00127  
00128 
00129     inline := data_buffer;
00130     inline_length := arg2;
00131   END;
00132 END;
00133 
00134 PROCEDURE ENTRY output_line (io_circuit : integer; outline : line;
00135                        length : integer);
00136 VAR data_buffer : line;
00137     circuit : integer;
00138     aux_param : port_param;
00139 
00140 BEGIN
00141   WITH aux_param DO
00142   BEGIN
00143     operation := send;
00144     arg1 := length;
00145     arg2 := 0;
00146     data_buffer := outline;
00147     circuit := io_circuit;
00148 
00149     io(data_buffer, aux_param, aux, circuit)
00150   END;
00151 END;
00152 
00153 PROCEDURE ENTRY write_disk_line (io_circuit : integer;
00154                                        outline : line;
00155                                        length : integer);
00156 
00157 VAR write_cmd : array [1..22] of char;
00158     write_line : line;
00159     i, j, k, line_length : integer;
00160     aux_param : port_param;
00161     circuit : integer;
00162 
00163 BEGIN
00164   write_cmd := 'GO OLDWRT;BOBFOX.DMP; ';
00165   IF length > 45 THEN
00166     line_length := 45
00167   ELSE
00168     line_length := length;
00169   FOR i := 1 TO 22 DO
00170     write_line [i] := write_cmd [i];
00171   FOR i := 1 TO line_length - 1 DO
00172     write_line [i+22] := outline [i];
00173   write_line [line_length+22] := '(:13:)';
00174   WITH aux_param DO
00175   BEGIN
00176     arg1 := line_length + 22;
00177     arg2 := 0;
00178     operation := send;
00179     circuit := io_circuit;
00180 
00181     io(write_line, aux_param, aux, circuit)
00182  
00183   END;
00184 END;
00185 
00186 PROCEDURE ENTRY get_buffer_info (io_circuit : integer;
00187                            VAR chars : integer;
00188                            VAR lines : integer);
00189 
00190 VAR data_buffer : line;
00191     circuit : integer;
00192     aux_param : port_param;
00193 
00194 BEGIN
00195   WITH aux_param DO
00196   BEGIN
00197     operation := terminal_info;
00198     arg1 := 0;
00199     arg2 := 0;
00200     circuit := io_circuit;
00201 
00202     REPEAT io(data_buffer, aux_param, aux, circuit)
00203     UNTIL status = complete;
00204   
00205     chars := arg1;
00206     lines := arg2;
00207   END;
00208 END;
00209 
00210 BEGIN
00211 END;
00212 
00213 
00214 
00215 
00216 
00217 TYPE circuit_process = PROCESS;
00218 
00219 VAR i, code : integer;
00220     status : io_result;
00221     circ_io : io_procedure;
00222     user_string : array [1..no_of_circuits] of short_line;
00223     circuit_built : boolean;
00224 
00225 BEGIN
00226   user_string [1] :='SALTYRON:39;s78comn                     ';
00227   user_string [2] :='SSEREP:39;s78devl                       ';
00228   user_string [3] :='CALSTATE:39;s78devl                     ';
00229   user_string [4] :='RGFOX:39;mybuddy                        ';
00230   INIT circ_io;
00231 
00232   CYCLE
00233     FOR i := 1 TO no_of_circuits DO
00234       BEGIN
00235         circ_io.check_circuit (i, circuit_built);
00236         IF NOT circuit_built THEN
00237           BEGIN
00238             circ_io.build_circuit (i, user_string[i], status, code);
00239             circ_io.output_line (i, '(:13:) ', 1);
00240           END;
00241       END;
00242     
00243     FOR i:= 1 TO 30 DO
00244       WAIT;
00245   END;
00246 END;
00247 
00248 TYPE disk_test = PROCESS;
00249 
00250 VAR i, inline_length : integer;
00251     disk_io : io_procedure;
00252     inline : line;
00253 
00254 BEGIN
00255   INIT disk_io;
00256   CYCLE
00257   disk_io.output_line(1, 'TYPE WEB11.FIL(:13:) ', 16);
00258   FOR i := 1 TO 10 DO
00259     BEGIN
00260       disk_io.input_line(1, inline, inline_length);
00261       disk_io.write_disk_line(1, inline, inline_length);
00262     END;
00263 
00264   FOR i := 1 to 30 DO
00265     WAIT;
00266   END;
00267 END;
00268 
00269 
00270 VAR circuit_maintainer : circuit_process;
00271     disk_writer : disk_test;
00272 
00273 BEGIN
00274   INIT circuit_maintainer;
00275   INIT disk_writer;
00276 END.
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             