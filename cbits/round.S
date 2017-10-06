#define REG_R1 %rbx
#define REG_R2 %r14
#define REG_R3 %rsi
#define REG_R4 %rdi
#define REG_R5 %r8
#define REG_R6 %r9
#define REG_Sp %rbp

#define REG_D1 %xmm1
#define REG_D2 %xmm2
#define REG_D3 %xmm3
#define REG_D4 %xmm4
#define REG_D5 %xmm5
#define REG_D6 %xmm6

#define ROUNDUP   $0b0101111110000000
#define ROUNDDOWN $0b0011111110000000
#define ROUNDNEAR $0b0001111110000000
#define ROUNDZERO $0b0111111110000000

#define ROUNDHASK $0b0001111110000000

// Set round mode to \mode using \set offset on stack, saving current in \store offset on stack
.macro roundmodeS mode store set
  stmxcsr -\store(REG_Sp)
	movl \mode, -\set(REG_Sp)
	ldmxcsr -\set(REG_Sp)
.endm

// Restore rounding mode from \store offset on stack
.macro restoremodeS store
 	ldmxcsr -\store(REG_Sp)
.endm

// Set rounding mode to \mode using \set offset on stack without saving current mode
.macro roundmodeU mode set
  movl \mode, -\set(REG_Sp)
  ldmxcsr -\set(REG_Sp)
.endm

// Restore round mode to default using \set offset on stack
.macro restoremodeU set
  roundmodeU ROUNDHASK \set
.endm

// Return to Haskell
.macro haskret
  jmp *(REG_Sp)
.endm

// Operation D1 = D1 \op D2 in rounding mode \mode, restoring rounding mode when done
.macro binopS mode op
  roundmodeS \mode 16 8
	\op REG_D2, REG_D1
	restoremodeS 16
	haskret
.endm

// Operation D1 = D1 \op D2 in rounding mode \mode, setting default rounding mode when done
.macro binopU mode op
  roundmodeU \mode 8
	\op REG_D2, REG_D1
	restoremodeU 8
	haskret
.endm

// Operation D1 = \op D1 in rounding mode \mode, restoring rounding mode when done
.macro unopS mode op
  roundmodeS \mode 16 8
  \op REG_D1, REG_D1
  restoremodeS 16
  haskret
.endm

// Operation D1 = \op D1 in rounding mode \mode, setting default rounding mode when done
.macro unopU mode op
  roundmodeU \mode 8
	\op REG_D1, REG_D1
	restoremodeU 8
	haskret
.endm

/////////////////////////////
// Some special testing stuff
.globl getmxcsr
getmxcsr:
  stmxcsr -8(REG_Sp)
  movl -8(REG_Sp), %ebx
  haskret

.globl plusn64U
plusn64U:
	  addsd REG_D2, REG_D1
	  haskret

// Addition
.globl plusu64S
plusu64S:
  binopS ROUNDUP addsd

.globl plusd64S
plusd64S:
  binopS ROUNDDOWN addsd

.globl plusu64U
plusu64U:
  binopU ROUNDUP addsd

.globl plusd64U
plusd64U:
  binopU ROUNDDOWN addsd

// Subtraction
.globl minusu64S
minusu64S:
  binopS ROUNDUP subsd

.globl minusd64S
minusd64S:
  binopS ROUNDDOWN subsd

.globl minusu64U
minusu64U:
  binopU ROUNDUP subsd

.globl minusd64U
minusd64U:
  binopU ROUNDDOWN subsd

// Multiplication
.globl multu64S
multu64S:
  binopS ROUNDUP mulsd

.globl multd64S
multd64S:
  binopS ROUNDDOWN mulsd

.globl multu64U
multu64U:
  binopU ROUNDUP mulsd

.globl multd64U
multd64U:
  binopU ROUNDDOWN mulsd

// Division
.globl divu64S
divu64S:
  binopS ROUNDUP divsd

.globl divd64S
divd64S:
  binopS ROUNDDOWN divsd

.globl divu64U
divu64U:
  binopU ROUNDUP divsd

.globl divd64U
divd64U:
  binopU ROUNDDOWN divsd

// From integer
.globl fromintu64U
fromintu64U:
  roundmodeU ROUNDUP 8
  cvtsi2sd REG_R1, REG_D1
	restoremodeU 8
	haskret

.globl fromintd64U
fromintd64U:
  roundmodeU ROUNDDOWN 8
  cvtsi2sd REG_R1, REG_D1
  restoremodeU 8
  haskret

// Square root
.globl sqrtu64S
sqrtu64S:
  unopS ROUNDUP sqrtsd

.globl sqrtd64S
sqrtd64S:
  unopS ROUNDDOWN sqrtsd

.globl sqrtu64U
sqrtu64U:
  unopU ROUNDUP sqrtsd

.globl sqrtd64U
sqrtd64U:
  unopU ROUNDDOWN sqrtsd
