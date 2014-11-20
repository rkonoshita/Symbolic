.SECTION    P,CODE,ALIGN=2
_tim_b1_init:
MOV.B       #-3,R0L
MOV.B       R0L,@63328:16
MOV.B       #-125,R0L
MOV.B       R0L,@63329:16
BSET.B      #5,@65525:8
RTS
_int_tim_b1:
PUSH.W      R1
PUSH.W      R0
JSR         @_tsensor_get:16
AND.B       #5,R0L
BCLR.B      #5,@65527:8
BCLR.B      #5,@65525:8
ADD.B       #-1,R0L
CMP.B       #6,R0L
BHI         L70:8
EXTU.W      R0
MOV.B       @(L72:16,ER0),R0L
ADD.W       #LWORD L63,R0
JMP         @ER0
L1:
MOV.W       #64,R6
BRA         L71:8
L64:
MOV.W       #32,R6
BRA         L2:8
L65:
MOV.W       #96,R6
BRA         L3:8
L66:
MOV.W       #16,R6
BRA         L4:8
L67:
MOV.W       #80,R6
BRA         L5:8
L68:
MOV.W       #48,R6
BRA         L6:8
L69:
MOV.W       #112,R6
BRA         L7:8
L8:
SUB.W       R6,R6
L9:
MOV.W       R6,R0
JSR         @_led_set:16
MOV.W       @_c_start:16,R0
INC.W       #1,R0
MOV.W       R0,@_c_start:16
BSET.B      #5,@65525:8
POP.W       R0
POP.W       R1
RTE
.SECTION    C,DATA,ALIGN=2
L72:
.DATA.B L10-L10
.DATA.B L12-L11
.DATA.B L14-L13
.DATA.B L16-L15
.DATA.B L18-L17
.DATA.B L20-L19
.DATA.B L22-L21
.DATAB.B    1,0
