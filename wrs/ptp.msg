Message 132  1512 28 Feb 85  RECENT
From: Kendall Miller <kendall%smu.csnet@csnet-relay.arpa>
To: WRS@office-2.ARPA


    The tone that follows a 0+ call is usually a Touch Tone #          
(941 Hz + 1477 Hz) which changes to a standard dial tone (350 Hz +
440 Hz) in a short time and then decays to silence.  You could 
use a Touch Tone receiver to detect the # or a special call progress
tone detector to get the dial tone.  
    I measured the prompt tone for MCI and Sprint and both
appear to be 400 Hz single frequency tones. 

The following is a list of Teltone I.C's that may be helpful to you. 
 

             M957   Touch Tone receiver
             M980   Call progress tone detector
             M982   Precise call progress tone detector
             M991   Call progress tone generator


     The M982 can be used to detect the dial tone for credit card dialing,
but not the MCI or Sprint tone, because it only detects 350 Hz, 440 Hz, 
480 Hz and 620 Hz.  To detect the MCI and Sprint tone, you could use the
M980, which is basically a wideband energy detector.  Call up Teltone
and ask them to send you some literature.




Kendall Miller


kendall@smu   (csnet)
convex!smu!kendall    (uucp)

 