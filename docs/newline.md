
# Newline handling in Fold


## Expression Group

    (x)                 == x
    (((x)))             == x
    (x + y)             == (add x y)
    (x + y) * z         == (mul (add x y) z)
    (x + (y + y)) * z   == (mul (add x (add y y)) z)
    (f x y)             == (f x y)


## Expression Blocks

    {2}           == (int 2)
    {2 + 2}       == (add (int 2) (int 2))
    f x {2 + 2}   == (f x (add (int 2) (int 2)))
    {x\ny\nz}     == (seq x (seq y z))
    {}            == (unit)
    {\n}          == (unit)


