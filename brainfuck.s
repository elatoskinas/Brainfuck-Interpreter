# ******************************************************************
# * Program name: brainfuck                                        *
# * Name: Evaldas Latoskinas                                       *
# * NetID: elatoskinas                                             *
# ******************************************************************

# ASSEMBLY DIRECTIVES
.bss
ARRAY:   .skip 30000     # Reserve 30000 bytes (~30kb) to use for Brainfuck
EX_TIMES: .skip 1000000  # Stores instruction execution count
LOOKUP_CONVERT: .skip 750 # Lookup table to convert ><+-.,[]0() chars to a consistent format (0,1,2,..)

.data
format_str:  .asciz "We should be executing the following code:\n%s\n"
#format_char: .asciz "%c\n" # For testing Rpurposes
#teststr:     .asciz "%d\n" # For testing purposes
#format_char2: .asciz "%c %c\n"
#teststr2:    .asciz "%d %c\n"

SKIP_OCCUR:  .quad 0  # The quad counts the nested loop occurences when skipping a loop, so that the end of the very outer loop that is also skipped can be found properly

LAST_BYTE: .byte 0    # Stores last byte of the previous initial brainfuck code string

COPY_LOOP: .byte 0      # 0 = true, 1 = false
COPY_LOOP_MOV: .quad 0  # Calculates total sum of < and > movements
COPY_LOOP_SUM: .quad 0  # Calculates total sum of p[curr] from + and - instructions

jmp_table:
.quad incptr, decptr, incr, decr, putch, getch, while_start, while_end, setZero, startCopyLoop, endCopyLoop

.global brainfuck

# %rbx - full brainfuck code string (e.g. +++++++>+.<<)
# %r12 - ARRAY current index

# Your brainfuck subroutine will receive one argument:
# a zero terminated string containing the code to execute.
brainfuck:
   # Open stack frame
   pushq  %rbp              # push the base pointer
   movq   %rsp, %rbp        # copy the stack pointer to the base pointer

   movq   %rdi, %rbx        # Reserve enough space in RBX to hold optimized brainfuck code string

   movq $0, %rcx            # Initialize byte count to 0

   movq $62, %rdx                  # Index for char 62 (>)
   movb $48, LOOKUP_CONVERT(%rdx)  # Move '0' char

   movq $60, %rdx                  # Index for char 60 (<)
   movb $49, LOOKUP_CONVERT(%rdx)  # Move '1' char

   movq $43, %rdx                  # Index for char 43 (+)
   movb $50, LOOKUP_CONVERT(%rdx)  # Move '2' char

   movq $45, %rdx                  # Index for char 45 (-)
   movb $51, LOOKUP_CONVERT(%rdx)  # Move '3' char

   movq $46, %rdx                  # Index for char 46 (.)
   movb $52, LOOKUP_CONVERT(%rdx)  # Move '4' char

   movq $44, %rdx                  # Index for char 44 (,)
   movb $53, LOOKUP_CONVERT(%rdx)  # Move '5' char

   movq $91, %rdx                  # Index for char 91 ([)
   movb $54, LOOKUP_CONVERT(%rdx)  # Move '6' char

   movq $93, %rdx                  # Index for char 93 (])
   movb $55, LOOKUP_CONVERT(%rdx)  # Move '7' char

   movq $48, %rdx                  # Index for char 48 (0)
   movb $56, LOOKUP_CONVERT(%rdx)  # Move '8' char

   movq $40, %rdx                  # Index for char 40 (()
   movb $57, LOOKUP_CONVERT(%rdx)  # Move '9' char

   movq $41, %rdx                  # Index for char 41 ())
   movb $58, LOOKUP_CONVERT(%rdx)  # Move 'A' char

   while:
    cmpb $0, (%rdi)         # check whether end of code reached
    je   begin_interpreter  # move on to next loop if true
    movb (%rdi), %al        # move brainfuck instruction to register AH
    inc  %rdi               # move on to next brainfuck instruction

    # Check for [-] which is basically setting a value to 0

    cmpb $'[', %al
    jne  whileCont

    cmpb $'-', (%rdi)
    jne  whileCont

    cmpb $']', 1(%rdi)
    jne  whileCont

    addq $2, %rdi          # Skip through [-] pattern
    movb $48, %al          # [-] patterns will be indicated as '0'
    jmp  while_move_char   # Move 0 to newly constructed brainfuck code string

    whileCont:
     cmpb (LAST_BYTE), %al   # Check whether last checked byte is the same as the current one
     jne  checkValidChars    # If not, continue checking for valid chars

    # Do not truncate the following characters
    cmpb $']', %al
    je   while_move_char
    cmpb $'[', %al
    je   while_move_char
    cmpb $'.', %al
    je   while_move_char
    cmpb $',', %al
    je   while_move_char

    decq %rcx               # Move the index 1 char back
    incq EX_TIMES(,%rcx,8)  # Increment the instruction count
    incq %rcx               # Move the index back to normal
    jmp  while              # Repeat loop

    # Only insert valid characters to final brainfuck string
    checkValidChars:
    cmpb $'+', %al
    je   while_move_char

    cmpb $'-', %al
    je   while_move_char

    cmpb $'>', %al
    je   while_move_char

    cmpb $'<', %al
    je   while_move_char

    cmpb $'[', %al
    je   while_move_char

    cmpb $']', %al
    je   while_move_char

    cmpb $',', %al
    je   while_move_char

    cmpb $'.', %al
    je   while_move_char

    jmp  while               # If no valid character found, repeat loop

    while_move_char:
     movb %al, (LAST_BYTE)  # Set last byte

     movzbq %al, %rdx
     movb   LOOKUP_CONVERT(%rdx), %al

     movb %al, (%rbx)       # Move byte to register B's current byte
     incq %rcx              # Increment byte count
     inc  %rbx              # Move on to next byte in register B
     jmp  while             # Repeat loop


   begin_interpreter:
    movb $0, (%rbx)         # Move zero char at the end of register B
    subq %rcx, %rbx         # Restore brainfuck code to original position

   movq   $0, %r12          # initialize index as 0

    # Initialize ARRAY  with zero char values
    initloop:
     movb  $0, ARRAY(%r12)         # initialize ARRAY element to zero char
     incq  %r12                    # increment index
     cmpq  $30000, %r12            # check index < 30000 condition
     jl    initloop                # repeat loop if condition does not hold

   movq   $0, %r12          # reset index at 0
   movq   $0, %r13          # initialize LOOP COUNT to 0
   movq   $0, SKIP_OCCUR    # initialize SKIP OCCURENCES value to 0

   movq   $0, %r14          # initialize STEP COUNT to 0
   movq   $0, %r15          # initialize CURRENT BYTE to 0

   movq $0, %rcx               # Initialize current byte to 0

   while_loop_count:
    cmpb $0, (%rbx)            # Check whether end of string has been reached
    je   parseLoopPre

    cmpq $0, %r13              # Check whether LOOP COUNT is > 0
    je   while_loop_count_cont # If it's not, move along
    incq %r14                  # Else increment STEP COUNT

    cmpb $0, COPY_LOOP          # Check whether still checking for copy loop
    jne  while_loop_count_cont  # If not, skip the logic for copy loop

    cmpq $1, %r13               # Check if we're only inside 1 loop
    jne  while_loop_count_cont  # If not, skip the logic for copy loop
    
    # Copy loop check valid characters
    checkInnerCL:
     cmpb $57, (%rbx)            # Check for '(' char
     jne  checkInnerCL2         # Else check for ')' char
     movb $1, COPY_LOOP         # If '(' found, this cannot be a copy loop
     jmp  while_loop_count_cont # Continue further in the loop


     checkInnerCL2:
      cmpb $58, (%rbx)           # Check for ')' char
      jne  checkPrint            # Else check for '.' char
      movb $1, COPY_LOOP         # If ')' found, this cannot be a copy loop
      jmp  while_loop_count_cont # Continue further in the loop


    checkPrint:
     cmpb $52, (%rbx)            # Check for '.' char
     jne  checkGet              # Else check for ','
     movb $1, COPY_LOOP         # If '.' found, this cannot be a copy loop
     jmp  while_loop_count_cont # Continue further in the loop

    checkGet:
     cmpb $53, (%rbx)            # Check for ',' char
     jne  checkMovL             # Else check for '<'
     movb $1, COPY_LOOP         # If ',' found, this cannot be a copy loop
     jmp  while_loop_count_cont # Continue further in the outer loop

    checkMovL:
     cmpb $49, (%rbx)            # Check for '<' char
     jne  checkMovR             # Else check for '>'
     decq (COPY_LOOP_MOV)       # Decrement total MOV count
     jmp  while_loop_count_cont # Continue further in the outer loop

    checkMovR:
     cmpb $48, (%rbx)              # Check for '>' char
     jne  check0                  # Else check if we're at initial counter
     incq (COPY_LOOP_MOV)         # Increment total MOV count
     jmp  while_loop_count_cont   # Continue further in the outer loop

     # Check +- instructions
     check0:
      cmpq $0, (COPY_LOOP_MOV)     # Check if we're at initial counter position
      jne  while_loop_count_cont   # If not, continue further in the outer loop

      checkAdd0:
       cmpb $50, (%rbx)              # Check for '+' char
       jne  checkSub0               # Else check for '-' char
       incq (COPY_LOOP_SUM)         # Increment total counter
       jmp  while_loop_count_cont   # Continue further in the outer loop

      checkSub0:
       cmpb $51, (%rbx)               # Check for '-' char
       jne  while_loop_count_cont    # Else continue further in the outer loop
       decq (COPY_LOOP_SUM)          # Decrement total counter
       jmp  while_loop_count_cont    # Continue further in the outer loop


    while_loop_count_cont:
     cmpb $54, (%rbx)           # Check for start of loop
     jne  loopEndCheck         # Else check for end of loop
     incq %r13                 # Increment LOOP COUNT if start of loop found

     cmpq $1, %r13             # Check if LOOP COUNT is equal to 1
     jne  loopEndCheckPre      # If not, check for end of loop condition

     movb $0, COPY_LOOP        # Else set COPY LOOP to true (assume)
     movq $0, (COPY_LOOP_MOV)  # Set COPY LOOP MOV sum to 0
     movq $0, (COPY_LOOP_SUM)  # Set COPY LOOP SUM (counter) to 0
     jmp  loopEndCheck         # Skip setting COPY LOOP to FALSE

    loopEndCheckPre:
     movb $1, COPY_LOOP        # Set COPY LOOP to FALSE (> 1 loop present)

    loopEndCheck:
     cmpb $55, (%rbx)           # Check for end of loop
     jne  while_loop_count_end  # If not found, continue loop

     cmpq $0, %r13              # Check if LOOP COUNT > 0
     je   while_loop_count_end  # If it's 0, continue loop

     incq (SKIP_OCCUR)           # Else increment SKIP OCCUR

     cmpq %r13, (SKIP_OCCUR)     # Check whether SKIP OCCUR equals to LOOP COUNT
     jne  while_loop_count_end   # If it's not, continue loop

     cmpb $0, COPY_LOOP          # Check whether still checking for copy loop
     jne  loopEndCheckCont
     cmpq $0, (COPY_LOOP_MOV)    # Check whether the counter remains the same
     jne  loopEndCheckCont
     cmpq $-1, (COPY_LOOP_SUM)   # Check whether the counter evaluates to -1 at the end of the loop
     jne  loopEndCheckCont

     # If all conditions meet, this is a copy loop, therefore [] brackets are changed to () brackets.

     movb $58, (%rbx)            # Exchange ] with )

     subq %r14, %rbx             # Jump back to the beginning of the loop
     subq %r14, %rcx             # Jump back to the beginning of the loop (move index)

     movb $57, (%rbx)            # Exchange [ with (
     jmp  loopEndCheckReinit     # Skip moving loop count (simply not needed here)


     loopEndCheckCont:
      movq %r14, EX_TIMES(,%rcx,8) # Else move step count to ] instruction

      subq %r14, %rbx              # Move back to beginning of loop
      subq %r14, %rcx              # Move back to beginning of loop

      movq %r14, EX_TIMES(,%rcx,8) # Move step count to [ instruction

      loopEndCheckReinit:
       movq $0, %r14                # Initialize STEP COUNT as 0
       movq $0, %r13                # Initialize LOOP COUNT as 0
       movq $0, (SKIP_OCCUR)        # Reset SKIP_OCCUR
       movb $0, COPY_LOOP           # Reset COPY_LOOP (set to TRUE)
       movq $0, (COPY_LOOP_MOV)     # Reset COPY_LOOP_MOV
       movq $0, (COPY_LOOP_SUM)     # Reset COPY_LOOP_SUM

    while_loop_count_end:
     inc  %rbx                    # Move on to next byte
     incq %rcx                    # Increment byte count
     jmp  while_loop_count        # Repeat loop

   parseLoopPre:
    subq %rcx, %rbx               # Return register B to original place
    movb $0, COPY_LOOP            # Reset COPY_LOOP to false (now 0 is false, 1 is true)

#   whileTest:
#   movq  $0, %rax
#   movq  $teststr2, %rdi
#   movq  EX_TIMES(,%r15,8),%rsi
#   movq  (%rbx),%rdx
#   call  printf

#   incq %r15
#   inc  %rbx

#   cmpb $0, (%rbx)
#   jne  whileTest

#     movq   %rbx, %rsi        # move address of code in brainfuck syntax (second arg for printf)
#     movq   $format_str, %rdi # first arg for printf (format)
#     movq   $0, %rax          # zero vector arguments
#     call   printf            # call the printf subroutine

#   jmp   endbrainfuck


     # Parse byte by byte (char by char) from specified brainfuck code 
     parseloop:
      movq     EX_TIMES(,%r15,8), %r11  # Move instruction execution count (0-based, so 0 means 1) to register 11
      incq     %r11                     # Increment the value by 1 (so it's 1-based and not 0-based now)

#      movq $0, %rax
#      movq $format_char, %rdi
#      movq (%rbx), %rsi
#      call printf

       # Parse char (byte) from brainfuck code to actual instructions
       parsechar:
         movzbq   (%rbx), %rdx
         subq     $48, %rdx
         jmp      *jmp_table(,%rdx,8)

           incptr:
             addq     %r11, %r12       # Increment pointer by instruction count
             jmp      parseloop_check

           decptr:
             subq     %r11, %r12       # Decrement pointer by instruction count
             jmp      parseloop_check

           incr:
             cmpb     $0, COPY_LOOP    # Check if in copy loop
             je       incr_cont

             movq     %r11, %rax       # Move instruction count to register A
             mulq     %r10             # Multiply by counter initial value
             movq     %rax, %r11       # Move result back to register 11

             incr_cont:
              addq     %r11, ARRAY(%r12)  # increment dereferenced pointer (actual value) by instruction count
              jmp      parseloop_check

           decr:
             cmpb     $0, COPY_LOOP    # Check if in copy loop    
             je       decr_cont

             movq     %r11, %rax       # Move instruction count to register A
             mulq     %r10             # Multiply by counter initial value
             movq     %rax, %r11       # Move result back to register 11

             decr_cont:
              subq     %r11, ARRAY(%r12) # decrement dereferenced pointer (actual value) by instruction count
              jmp      parseloop_check

           putch:
             mov      ARRAY(%r12), %rdi  # moves argument for putchar (we print ARRAY[%r12] character
             call     putchar            # calls putchar with specified parameter
             jmp      parseloop_check

           getch:
             call     getchar            # call the getchar subroutine, which returns the result in Register A
             mov      %rax, ARRAY(%r12)  # moves the result from register A to ARRAY(%r12)
             jmp      parseloop_check

          # Start a new while loop
          while_start:
             cmpb     $0, ARRAY(%r12)         # check whether while condition does not hold (on start)
             jne      parseloop_check         # If it does not, move on
             decq     %r11                    # Decrement jump count
             addq     %r11, %rbx              # Move forward by jump count (skip through loop)
             addq     %r11, %r15              # Increase current byte by required amount
             jmp      parseloop               # Repeat loop

          # Check while loop end condition
          while_end:
             cmpb     $0, ARRAY(%r12)     # check whether (ARRAY(%r12) == 0) holds
             je       parseloop_check     # If it does, move on
             decq     %r11                # Decrement jump count
             subq     %r11, %rbx          # Move backward by jump count (return to loop beginning)
             subq     %r11, %r15          # Decrease current byte by required amount
             jmp      parseloop           # Repeat loop

          startCopyLoop:
           movb     $1, COPY_LOOP  # Set COPY_LOOP boolean to true

           movzbq   ARRAY(%r12), %r10 # Move byte as quad to register 10

           jmp      parseloop_check
           
          endCopyLoop:
            movb     $0, COPY_LOOP     # Set COPY_LOOP boolean to false
            movb     $0, ARRAY(%r12)   # Reset counter to 0
            jmp      parseloop_check


          setZero:
            movb     $0, ARRAY(%r12)    # Initialize current byte to 0
            jmp      parseloop_check

      parseloop_check:
        incq     %r15                # Increment current byte
        inc      %rbx                # Increment register B (so we move on to next byte)
        cmpb     $0, (%rbx)          # Check whether we have reached 0 char (therefore end of code)
        jne      parseloop           # repeat loop if we have not reached 0 char

    # Close stack frame (restore stack pointer)
    endbrainfuck:
     movq    %rbp, %rsp
     popq    %rbp
     ret
