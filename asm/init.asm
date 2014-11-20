.SECTION    P,CODE,ALIGN=2
__INITSCT:
MOV.W       @__B_BGN:16,R1
BRA         L23:8
L24:
SUB.B       R0L,R0L
MOV.B       R0L,@ER1
INC.W       #1,R1
L25:
MOV.W       @__B_END:16,R0
CMP.W       R0,R1
BLO         L26:8
MOV.W       @__D_BGN:16,R1
MOV.W       @__D_ROM:16,R5
BRA         L27:8
L28:
MOV.B       @ER5,R0L
MOV.B       R0L,@ER1
INC.W       #1,R1
INC.W       #1,R5
L29:
MOV.W       @__D_END:16,R0
CMP.W       R0,R1
BLO         L30:8
RTS
_init:
SUB.B       R0L,R0L
MOV.B       R0L,@65524:8
MOV.B       R0L,@65525:8
MOV.B       #-8,R0H
MOV.B       R0H,@65510:8
MOV.B       #112,R0H
MOV.B       R0H,@65512:8
JSR         @_tim_b1_init:16
ANDC.B      #127,CCR
RTS
