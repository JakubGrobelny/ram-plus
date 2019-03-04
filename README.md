# ram-plus
Random-access machine with instruction pointer access and built-in stack.

# instructions
```
ADD    «arg»       addition
MULT   «arg»       multiplication
DIV    «arg»       division
SUB    «arg»       subtraction

READ   «arg»       reads from input tape
WRITE  «arg»       writes to output tape
STORE  «arg»       store value from accumulator
LOAD   «arg»       load value from register into accumulator

JUMP   «label»     jump unconditionally
JZERO  «label»     jump if zero
JGTZ   «label»     jump if greater than zero
JLTZ   «label»     jump if lesser than zero

PUSH   «arg»       push value from an address to the stack
POP    «arg»       pop value from stack to an addressi
CALL   «label»     store instruction pointer and jump
RET                load instruction pointer and jump back

HALT               ends program execution
```

# argument types
```
=«val»    constant
*«val»    dereference
«val»     address
```
