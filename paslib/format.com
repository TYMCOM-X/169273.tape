ru detab
nnmne.srt=nnmne

r sort
nnmne.srt=nnmne.srt/k17.1/k9.6/r50
ru retab
nnmne.srt=nnmne.srt

do qed
l nnmne.srt;!1!i
$ski%Format 1:  opcode Br%$ski
.
!2!i
$ski%Format 2:  opcode R1,R2%$ski
.
!3!i
$ski%Format 3:  opcode R1,I%$ski
.
!4!i
$ski%Format 4:  opcode R1,M1%$ski
.
!5!i
$ski%Format 5:  opcode R1,M%$ski
.
!6!i
$ski%Format 6:  opcode R1,I,M2%$ski
.
!7!i
$ski%Format 7:  opcode R1,M1,M2%$ski
.
!8!i
$ski%Format 8:  opcode subop%$ski
.
1,$sp/%/d a;w
y
q
