.SECTION    P,CODE,ALIGN=2
_tsensor_get:
MOV.B       @65494:8,R0L
NOT.B       R0L
AND.B       #7,R0L
RTS
