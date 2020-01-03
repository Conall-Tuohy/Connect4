;
; CS1022 Introduction to Computing II 2018/2019
; Mid-Term Assignment - Connect 4 - SOLUTION
;
; get, put and puts subroutines provided by jones@scss.tcd.ie
;
ROWNUM		EQU	6
COLUMNNUM	EQU	7


PINSEL0	EQU	0xE002C000
U0RBR	EQU	0xE000C000
U0THR	EQU	0xE000C000
U0LCR	EQU	0xE000C00C
U0LSR	EQU	0xE000C014


	AREA	globals, DATA, READWRITE

	AREA	RESET, CODE, READONLY
	ENTRY
RESTART
	; initialise SP to top of RAM



	LDR	R13, =0x40001000	; initialse SP

	; initialise the console
	BL	inithw

	;
	; your program goes here
	;
    BL INITBOARD
	LDR	R0, =str_go				; R0 -> "Allocated String"
	BL	PUTS					; put string
	LDR	R0, =ROBOTCHOICE		; R0 -> "Allocated String"
	BL	PUTS					; put string
INPUTCHOICEAGAIN
	LDR R1, =0x40000200
	LDR R2, =2
	BL  GETS
	CMP R3,#0x52
	BEQ ROBOTSETUP
	CMP R3,#0x50
	BEQ NORMALMODE
	LDR	R0, =INVALIDSTARTINGCHOICE			; R0 -> "Allocated String"
	BL	PUTS								; put string
	B   INPUTCHOICEAGAIN
	
ROBOTSETUP
	LDR R5,=1
	LDR	R0, =str_go				; R0 -> "Allocated String"
	BL	PUTS					; put string
	BL PRINTBOARD
	B PLAYERONESTURN

NORMALMODE
	LDR R5,=2
	LDR	R0, =str_go				; R0 -> "Allocated String"
	BL	PUTS					; put string
	BL PRINTBOARD
	B PLAYERONESTURN


NOSPACEFOUND
	LDR	R0, =str_newline			; R0 -> "Allocated String"
	BL	PUTS						; put string
	LDR	R0, =FULLCOLUMN				; R0 -> "Allocated String"
	BL	PUTS					; put string
	B BACKTOINPUT

PLAYERONESTURN
	BL isBoardFull
	LDR R8,=0x59
	LDR	R0, =PLAYERONESGO		; R0 -> "Allocated String"
	BL	PUTS					; put string
	LDR	R0, =ASKFORINPUT				; R0 -> "Allocated String"
	BL	PUTS					; put string
	B BACKTOINPUT
		
PLAYERTWOSTURN
	BL isBoardFull
	LDR R8,=0x58
	LDR	R0, =PLAYERTWOSGO		; R0 -> "Allocated String"
	BL	PUTS					; put string
	LDR	R0, =ASKFORINPUT				; R0 -> "Allocated String"
	BL	PUTS					; put string
	B BACKTOINPUT
		
ROBOTSTURN
	BL isBoardFull
	LDR R8,=0x52
	LDR	R0, =ROBOTSGO		; R0 -> "Allocated String"
	BL	PUTS					; put string
	BL robotColumnChecker
	BL PRINTBOARD
	B PLAYERONESTURN
	
BACKTOINPUT

	LDR R1, =0x40000200
	LDR R2, =2
	BL GETS
	
	CMP R3, #0x52
	BEQ RESTART
	CMP R3, #0x37
	BHI INPUTNOTVALID
	CMP R3, #0x31
	BLO INPUTNOTVALID
	BL PLACE
	LDR	R0, =str_newline				; R0 -> "Allocated String"
	BL	PUTS						; put string
	BL PRINTBOARD
	CMP R8, #0x58
	BEQ PLAYERONESTURN
	CMP R8, #0x52	
	BEQ PLAYERONESTURN
	B ROBOTORNOT
	
ROBOTORNOT
	CMP R5, #1
	BEQ ROBOTSTURN
	CMP R5, #2
	BEQ PLAYERTWOSTURN
	
GAMEWON	
	CMP R8, #0x52
	BEQ ROBOTWIN
BACKTOGAMEWON
	LDR	R0, =str_newline			; R0 -> "Allocated String"
	BL	PUTS						; put string
	BL PRINTBOARD
	LDR	R0, =str_newline			; R0 -> "Allocated String"
	BL	PUTS						; put string
	MOV R0, R8
	BL put
	LDR	R0, =win					; R0 -> "Allocated String"
	BL	PUTS						; put string
	B stop

ROBOTWIN
	STR R8, [R4]					; Makes the last move for the robot as it tries to end the game 
	B BACKTOGAMEWON					; when it finds a winning place but doesn't place it.
	
GAMEISDRAWED
	LDR	R0, =str_newline			; R0 -> "Allocated String"
	BL	PUTS						; put string
	BL PRINTBOARD
	LDR	R0, =str_newline			; R0 -> "Allocated String"
	BL	PUTS						; put string
	LDR	R0, =DRAW					; R0 -> "Allocated String"
	BL	PUTS						; put string
	B stop

INPUTNOTVALID
	LDR	R0, =str_newline		; R0 -> "Allocated String"
	BL	PUTS					; put string
	LDR	R0, =INVALIDINPUT		; R0 -> "Allocated String"
	BL	PUTS					; put strinG

	B BACKTOINPUT




stop	B	stop


;
; hint! put the strings used by your program here ...
;


INVALIDSTARTINGCHOICE		DCB	"That didn't seem to be a valid choice.. If you could either type 'R' or 'P' please.", 0x0a, 0, 0
ROBOTCHOICE					DCB	"Would you like to play against a robot (type R) or another player (type P)", 0x0a, 0, 0
DRAW						DCB	"The game ended in a draw with neither player winning. Good job I guess.", 0x0a, 0, 0	
ASKFORINPUT					DCB	"Input a column between 1 and 7 (or type R to restart.)", 0x0a, 0, 0
FULLCOLUMN					DCB	"That column is full. Input a differant column between 1 and 7.", 0x0a, 0, 0
win							DCB	" WINS", 0x0a, 0, 0
PLAYERONESGO				DCB	"It's Player 1's turn.", 0x0a, 0, 0
PLAYERTWOSGO				DCB	"It's Player 2's turn.", 0x0a, 0, 0
ROBOTSGO					DCB	"It's the robots turn.", 0x0a, 0, 0
INVALIDINPUT				DCB	"That input didn't seem to be a valid one (BETWEEN 1 & 7).", 0x0a, 0, 0
str_go 						DCB	"Let's play Connect4!!",0xA, 0xD, 0xA, 0xD, 0
str_newline 				DCB	0xA, 0xD, 0x0


BOARD	DCD	79,79,79,79,79,79,79
		DCD	79,79,79,79,79,79,79
		DCD	79,79,79,79,79,79,79
		DCD	79,79,79,79,79,79,79
		DCD	79,79,79,79,79,79,79
		DCD	79,79,79,79,79,79,79



;
; your subroutines go here
;

;
; robotColumnChecker subroutine
; Checks what column is the best move
; parameters:
;	none
; return value:
;	none
;
robotColumnChecker
	PUSH {R1-R11, LR}
	MOV R10, LR
	LDR R2,=0x40000000
	LDR R0,=COLUMNNUM
	LDR R1,=ROWNUM
	MUL R1, R0, R1
	LDR R0,=4
	MUL R1, R0, R1
	ADD R2, R1				;R2 = End of Board (Row Check)
	SUB R2, #4
	MOV R5, R2				;copy of end of board
	LDR R6,=COLUMNNUM		;
	MUL R6, R0, R6			;R6 x 4
	SUB R5, R6				;R5 = Start of bottom row
	LDR R6,=0				;R6=MAX PLACE NUMBER 
	LDR R11,=0x0			; R11= Best address
nextrowcheck
	CMP R2, R5
	BLS ENDOFCOLUMNCHECKER
	MOV R4, R2				;Column Check
	MOV R7, LR
	BL FIND_FREE_SLOT

	
FOUNDEMPTYSLOT	
	MOV LR, R7
	BL CHECKROBOSPOTVALUE
	CMP R7, R6				; IS CURRENTS ROW POTENTIAL BETTER THAN ANY CHECKED BEFORE
	BLS ROWISFULL			; IF NOT (CHECK NEXT ROW)
	MOV R11, R9				; IF IS ()
	MOV R6, R7
ROWISFULL
	SUB  R2, #4				;R2 go left a column
	B nextrowcheck
ENDOFCOLUMNCHECKER
	MOV LR, R7
	
	;BL ROBOTPLAYERWINCHECKER
	STR R8, [R11] 
	MOV LR, R10	
	POP {R1-R11, PC}
	
;
; ROBOTPLAYERWINCHECKER subroutine
; Checks what column is the best move
; parameters:
;	R2 = END OF ARRAY 
; return value:
;	R11 = WINNING MOVE FOR PLAYER
;

ROBOTPLAYERWINCHECKER
	PUSH{r2-R9, LR}
	MOV R5, LR
	LDR R4,=0x40000000
	LDR R8,=0x59
NEXTTILE
	LDR R0,=7
	LDR R7,=7
	LDR R3,[R4]
	CMP R3, #79
	BNE SKIPTILE
	LDR R10,=0x52
	BL  checkIfWon
	CMP R12, #1
	BEQ PLAYERWINFOUND
	CMP R4, R2
	BHS NOHUMANWINFOUND
SKIPTILE	
	ADD R4, #4
	B NEXTTILE
PLAYERWINFOUND
	MOV R11, R10
NOHUMANWINFOUND	
	MOV LR, R5
	POP{r2-R9, PC}	

;
; FIND_FREE_SLOT
; Finds an empty slot in a row or says that the row is full

FIND_FREE_SLOT
ROWCHECKLOOP
	CMP R4, #0x40000000
	BLO ROWISFULL
	LDR R3, [R4]
	CMP R3, #79
	BEQ FOUNDEMPTYSLOT
	SUB R4, #28
	B ROWCHECKLOOP

;
; CHECKSPOTVALUE subroutine
; returns the POTENTIAL VALUE OF A SPOT
; parameters:
;	R4 = SPOT IN QUESTION
;	R8 = ROBOT COUNTER
; return value:
;	R7 - Amount of move
;

CHECKROBOSPOTVALUE
	PUSH{r0-r8, lr}
	LDR R7,=0					;Place Value

;horizontal checker
	LDR R1,=1					;Winning Counter
	MOV R3, R4					;R3 = TempEditable address 
;moving right
cRUNRIGHTAGAIN	
	BL SET_NEW_SPOTSCORE
	CMP R1,#4
	BHS GAMEWON
	
	ADD R3,#4
	LDR R5, [R3]
	CMP R5,	#0x52
	BNE cENDOFRIGHT
	ADD R1,#1
	B cRUNRIGHTAGAIN	
cENDOFRIGHT
;moving LEFT
	MOV R3, R4
cRUNLEFTAGAIN
	BL SET_NEW_SPOTSCORE
	CMP R1,#4
	BHS GAMEWON

	SUB R3,#4
	LDR R5, [R3]
	CMP R5,	#0x52
	BNE cENDOFLEFT
	ADD R1,#1
	B cRUNLEFTAGAIN		
cENDOFLEFT


;VERTICAL checker
	LDR R1,=1
	LDR R3,=0x400000A4
	MOV R6, R4
cGODOWNAGAIN
	BL SET_NEW_SPOTSCORE
	CMP R1,#4
	BHS GAMEWON
	ADD R6,#28
	CMP R6,	R3
	BGE cENDOFDOWN
	LDR R5, [R6]
	CMP R5,	#0x52
	BNE cENDOFDOWN
	ADD R1,#1
	B cGODOWNAGAIN	
cENDOFDOWN


;DIAGONAL CHECKER


;DIAGONAL UP-RIGHT
	LDR R1,=1
	LDR R3,=0x40000000
	MOV R6, R4
cGO_UP_RIGHT_AGAIN
	BL SET_NEW_SPOTSCORE
	CMP R1,#4
	BHS GAMEWON
	SUB R6,#28
	CMP R6,	R3
	BLT cENDOF_UP_RIGHT
	ADD R6,#4
	LDR R5, [R6]
	CMP R5,	#0x52
	BNE cENDOF_UP_RIGHT
	ADD R1,#1
	B cGO_UP_RIGHT_AGAIN	
cENDOF_UP_RIGHT


;DIAGONAL DOWN_LEFT
	MOV R6, R4
	LDR R3,=0x400000A4
cGO_DOWN_LEFT_AGAIN	
	BL SET_NEW_SPOTSCORE
	CMP R1,#4
	BHS GAMEWON
	ADD R6,#28
	CMP R6,	R3
	BGT cENDOF_DOWN_LEFT
	SUB R6,#4
	LDR R5, [R6]
	CMP R5,	#0x52
	BNE cENDOF_DOWN_LEFT
	ADD R1,#1
	B cGO_DOWN_LEFT_AGAIN	
cENDOF_DOWN_LEFT


;DIAGONAL UP-LEFT
	LDR R1,=1
	LDR R3,=0x40000000
	MOV R6, R4
cGO_UP_LEFT_AGAIN
	BL SET_NEW_SPOTSCORE
	CMP R1,#4
	BHS GAMEWON
	SUB R6,#28
	CMP R6,	R3
	BLT cENDOF_UP_LEFT
	SUB R6,#4
	LDR R5, [R6]
	CMP R5,	#0x52
	BNE cENDOF_UP_LEFT
	ADD R1,#1
	B cGO_UP_LEFT_AGAIN	
cENDOF_UP_LEFT


;DIAGONAL DOWN_RIGHT
	MOV R6, R4
	LDR R3,=0x400000A4
cGO_DOWN_RIGHT_AGAIN
	BL SET_NEW_SPOTSCORE
	CMP R1,#4
	BHS GAMEWON
	ADD R6,#28
	CMP R6,	R3
	BGT cENDOF_DOWN_RIGHT
	ADD R6,#4
	LDR R5, [R6]
	CMP R5,	#0x52
	BNE cENDOF_DOWN_RIGHT
	ADD R1,#1
	B cGO_DOWN_RIGHT_AGAIN	
cENDOF_DOWN_RIGHT

	POP{r0-r8, PC}

SET_NEW_SPOTSCORE
	PUSH {R11, LR}
	CMP R1, R7
	BLS ISNTHIGHER
	MOV R7, R1
	MOV R9, R4
ISNTHIGHER	
	POP {R11, PC}
	


;
; getS subroutine
; returns the ASCII code of the next character read on the console
; parameters:
;	R1 = ARRAYPOINTER TO CHAR[]
;	R2 = UNSIGNED INT LENGTH
; return value:
;	R0 - ASCII code of the character read on teh console (byte)
;
GETS 
	PUSH {R4-R11, LR}
	MOV R8, R2
	MOV R9, R1
	MOV R7, LR;
	LDR R4, = 0			; R4 = LENGTH_COUNTER
	SUB R8, #1
LOOP1
	ADD R4, #1			;LENGTH_COUNTER++;
	CMP R4, R2			;LENGTH_COUNTER==LENGTH-1? 
	BEQ GETSEND

	BL get	
	STRB R0, [R9], #1
	MOV R10, R0
	BL put
	B LOOP1
GETSEND	
	STRB R0, [R9], #1
	MOV R3, R10
	MOV LR, R7;
	POP {R4-R11, PC}

;
; INITBOARD subroutine
; Puts board to the memory
; parameters:
;	none
; return value:
;	none
;
INITBOARD
	push{r0-r4,lr}
	LDR R0,=BOARD
	LDR R2,=42 ;LENGTH
	LDR R3,=0
	ldr R4,=0x40000000
arrayloop	
	CMP R3,R2
	BEQ endarrayloop
	LDR R1,[R0,R3,LSL #2]
	STR R1,[R4,R3,LSL #2]
	ADD R3,#1
	B arrayloop	
endarrayloop	

	pop{r0-r4,pc}

;
; PLACE subroutine
; Puts board to the memory
; parameters:
;	R8 = Which player just moved (Colour of tile) 
; return value:
;	none
;
PLACE
	PUSH{r2-R9, LR}
	MOV R5, LR
	SUB R0, #48					;LENGTH
	MOV R7, R0
	LDR R4,=0x400000A4
	LDR R9,=COLUMNNUM			; times 
	SUB R7, R9, R7
	ldr r3,=4
	mul r7, r3, r7
	sub r4, r7
ROWCHECKINGLOOOOOP
	LDR R3, [R4]			;
	CMP R3, #0x4F
	BEQ EMPTYSLOTFOUND
	LDR R3, [R4]
	CMP R3, #0
	BEQ NOSPACEFOUND
	SUB R4, #28
	B ROWCHECKINGLOOOOOP
EMPTYSLOTFOUND
	STR R8,[R4]
	CMP R8, #0x52
	BEQ ROBOTSKIP
	BL checkIfWon
ROBOTSKIP	
	MOV LR, R5
	POP{r2-R9, PC}



;
; checkIfWon subroutine
; Checks if the last move played was a winning move
; parameters:
;	R8 = Which player just moved (Colour of tile) 
;	R4 = Position of the last move
;	R0 = Last move played (Distance from left)
;	R7 = Last move played (Distance from right)
; return value:
;	none
;

GAMEPOSSIBLYWON
	LDR R12,=1
	CMP R10, #0x52
	BNE GAMEWON
	MOV R10, R4
	POP{R0-R8, pc}
	
checkIfWon
	PUSH{r0-r8, lr}
;horizontal checker
	LDR R1,=1					;Winning Counter
	MOV R3, R4					;R3 = TempEditable address 
;moving right
RUNRIGHTAGAIN	
	CMP R1,#4
	BHS GAMEPOSSIBLYWON
	MOV R7, R7, LSL #2
	ADD R7, R3, R7	
	ADD R3,#4
	CMP R3,	R7
	BGE ENDOFRIGHT
	LDR R5, [R3]
	CMP R5,	R8
	BNE ENDOFRIGHT
	ADD R1,#1
	B RUNRIGHTAGAIN	
ENDOFRIGHT
;moving LEFT
	MOV R3, R4
RUNLEFTAGAIN	
	CMP R1,#4
	BHS GAMEPOSSIBLYWON
	MOV R0, R0, LSL #2
	ADD R0, R3, R0	
	SUB R3,#4
	CMP R3,	R0
	BGE ENDOFLEFT
	LDR R5, [R3]
	CMP R5,	R8
	BNE ENDOFLEFT
	ADD R1,#1
	B RUNLEFTAGAIN		
ENDOFLEFT
;VERTICAL checker
	LDR R1,=1
	LDR R3,=0x400000A4
	MOV R6, R4
GODOWNAGAIN	
	CMP R1,#4
	BHS GAMEPOSSIBLYWON
	ADD R6,#28
	CMP R6,	R3
	BGE ENDOFDOWN
	LDR R5, [R6]
	CMP R5,	R8
	BNE ENDOFDOWN
	ADD R1,#1
	B GODOWNAGAIN	
ENDOFDOWN
;DIAGONAL CHECKER
;DIAGONAL UP-RIGHT
	LDR R1,=1
	LDR R3,=0x40000000
	MOV R6, R4
GO_UP_RIGHT_AGAIN	
	CMP R1,#4
	BHS GAMEPOSSIBLYWON
	SUB R6,#28
	CMP R6,	R3
	BLT ENDOF_UP_RIGHT
	ADD R6,#4
	LDR R5, [R6]
	CMP R5,	R8
	BNE ENDOF_UP_RIGHT
	ADD R1,#1
	B GO_UP_RIGHT_AGAIN	
ENDOF_UP_RIGHT
;DIAGONAL DOWN_LEFT
	MOV R6, R4
	LDR R3,=0x400000A4
GO_DOWN_LEFT_AGAIN	
	CMP R1,#4
	BHS GAMEPOSSIBLYWON
	ADD R6,#28
	CMP R6,	R3
	BGT ENDOF_DOWN_LEFT
	SUB R6,#4
	LDR R5, [R6]
	CMP R5,	R8
	BNE ENDOF_DOWN_LEFT
	ADD R1,#1
	B GO_DOWN_LEFT_AGAIN	
ENDOF_DOWN_LEFT

;DIAGONAL UP-LEFT
	LDR R1,=1
	LDR R3,=0x40000000
	MOV R6, R4
GO_UP_LEFT_AGAIN	
	CMP R1,#4
	BHS GAMEPOSSIBLYWON
	SUB R6,#28
	CMP R6,	R3
	BLT ENDOF_UP_LEFT
	SUB R6,#4
	LDR R5, [R6]
	CMP R5,	R8
	BNE ENDOF_UP_LEFT
	ADD R1,#1
	B GO_UP_LEFT_AGAIN	
ENDOF_UP_LEFT
;DIAGONAL DOWN_RIGHT
	MOV R6, R4
	LDR R3,=0x400000A4
GO_DOWN_RIGHT_AGAIN	
	CMP R1,#4
	BHS GAMEPOSSIBLYWON
	ADD R6,#28
	CMP R6,	R3
	BGT ENDOF_DOWN_RIGHT
	ADD R6,#4
	LDR R5, [R6]
	CMP R5,	R8
	BNE ENDOF_DOWN_RIGHT
	ADD R1,#1
	B GO_DOWN_RIGHT_AGAIN	
ENDOF_DOWN_RIGHT
	POP{R0-R8, pc}


;
; PRINTBOARD subroutine
; PRINTS board FROM the memory
; parameters:
;	none
; return value:
;	none
;
PRINTBOARD
	push{r0-r4, R8, R7, lr}
	LDR R2,=42 ;LENGTH
	LDR R3,=0
	LDR R8,=0
	ldr R4,=0x40000000
	MOV R7, LR;
PRINTarrayloop	
	CMP R8, #COLUMNNUM 
	BEQ	ROWCHANGE
	CMP R3,R2
	BEQ PRINTendarrayloop
	LDR R0,[R4,R3,LSL #2]
	BL put
	LDR R0, =0x20
	BL put
	ADD R3,#1
	ADD R8,#1
	B PRINTarrayloop	
ROWCHANGE
	LDR R8,=0
	LDR R0, =10
	BL put
	B PRINTarrayloop
PRINTendarrayloop	
	MOV LR, R7;
	pop{r0-r4, R8, R7, pc}

;
; isBoardFull subroutine
; Checks if the board is full or not
; parameters:
;	none
; return value:
;	none
;
isBoardFull	
	PUSH{R0-R4, LR}
	LDR R4,=0x40000000
	LDR R3,=COLUMNNUM
	LDR R2,=4
	LDR R0,=79
	MUL R2, R3, R2
	ADD R2, R2, R4
BOARDLOOOP	
	CMP R4,R2
	BHS GAMEISDRAWED
	LDR R1, [R4]
	CMP R1,#79
	BEQ ENDOFBOARDCHECKER
	ADD R4,#4
	B BOARDLOOOP
ENDOFBOARDCHECKER	
	POP{R0-R4, PC}

;	
; PUTS
;
; sends NUL terminated ASCII string (address in R0) to UART #1 window
;
PUTS	
	PUSH	{R4, LR} 		; push R4 and LR
	MOV	R4, R0			; copy R0
PUTS0	
	LDRB	R0, [R4], #1		; get character + increment R4
	CMP	R0, #0			; 0?
	BEQ	PUTS1			; return
	BL	put			; put character
	B	PUTS0			; next character
PUTS1	POP	{R4, PC} 		; pop R4 and PC
	

;
; inithw subroutines
; performs hardware initialisation, including console
; parameters:
;	none
; return value:
;	none
;
inithw
	LDR	R0, =PINSEL0		; enable UART0 TxD and RxD signals
	MOV	R1, #0x50
	STRB	R1, [R0]
	LDR	R0, =U0LCR		; 7 data bits + parity
	LDR	R1, =0x02
	STRB	R1, [R0]
	BX	LR

;
; get subroutine
; returns the ASCII code of the next character read on the console
; parameters:
;	none
; return value:
;	R0 - ASCII code of the character read on teh console (byte)
;
get	LDR	R1, =U0LSR		; R1 -> U0LSR (Line Status Register)
get0
	LDR	R0, [R1]		; wait until


	ANDS R0, #0x01		; receiver data

	BEQ	get0			; ready
	LDR	R1, =U0RBR		; R1 -> U0RBR (Receiver Buffer Register)
	LDRB	R0, [R1]		; get received data
	BX	LR			; return

;
; put subroutine
; writes a character to the console
; parameters:
;	R0 - ASCII code of the character to write
; return value:
;	none
;
put	
	LDR	R1, =U0LSR				; R1 -> U0LSR (Line Status Register)
	LDRB	R1, [R1]			; wait until transmit
	ANDS	R1, R1, #0x20		; holding register
	BEQ	put						; empty
	LDR	R1, =U0THR				; R1 -> U0THR
	STRB	R0, [R1]			; output charcter
put0	LDR	R1, =U0LSR			; R1 -> U0LSR
	LDRB	R1, [R1]			; wait until
	ANDS	R1, R1, #0x40		; transmitter
	BEQ	put0					; empty (data flushed)
	BX	LR						; return

;
; puts subroutine
; writes the sequence of characters in a NULL-terminated string to the console
; parameters:
;	R0 - address of NULL-terminated ASCII string
; return value:
;	R0 - ASCII code of the character read on teh console (byte)
;
puts	STMFD	SP!, {R4, LR} 		; push R4 and LR
	MOV	R4, R0			; copy R0
puts0	LDRB	R0, [R4], #1		; get character + increment R4
	CMP	R0, #0			; 0?
	BEQ	puts1			; return
	BL	put			; put character
	B	puts0			; next character
puts1	LDMFD	SP!, {R4, PC} 		; pop R4 and PC


	END
