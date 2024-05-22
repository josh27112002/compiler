.model small
.stack
.data 
.code
INICIO: MOV AX, @DATA
        MOV DS, AX
        MOV ES, AX

------CODIGO OBJETO-------

FIN: MOV AX,4C00H
     INT 21H
     END
