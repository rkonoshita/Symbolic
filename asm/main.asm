.SECTION    P,CODE,ALIGN=2
_main:
JSR         @_init:16
L59:
MOV.W       @_c_start:16,R0
CMP.W       #4,R0
BLT         L0:8
ORC.B       #-128,CCR
RTS
