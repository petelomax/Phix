lex.exw         lexer/tokeniser
  lex.e
    core.e
parse.exw       parser/AST generator
  parse.e
    lex.e [etc]
interp.exw      interpreter
  parse.e [etc]
cgen.exw        code generator
  cgen.e
vm.exw          virtual machine
  cgen.e

