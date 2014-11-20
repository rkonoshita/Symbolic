.SECTION    P, CODE, ALIGN=2
_startup:
MOV.L   #H'FF80, ER7
JSR		@__INITSCT
JSR     @_main
