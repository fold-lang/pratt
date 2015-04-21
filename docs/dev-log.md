
# Development Log


## [Apr 17, 2015]

- The handling of sequences and applications seems to be ok.
- Figure out a way to make the \n work with applications,
  _eg_: do not append if a \n is found.
- The handling of indirect applicative expressions cannot be expressed with
  prefix rules. For example, in `(x => x + x) 2` the application is defined as
  a sequence.
- The previous issue may be solved with the addition of an explicit group
  expression.
- It may not be a good idea to explicitly handle function appliation in the AST.

