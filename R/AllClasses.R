setClassUnion("numericORNULL", c("numeric", "NULL"))
setClassUnion("listORNULL", c("list", "NULL"))
setClassUnion("characterORlistORNULL", c("character", "list", "NULL"))
setClassUnion("logicalORNULL", c("logical", "NULL"))
setClassUnion("logicalORlistORNULL", c("logical", "list" ,"NULL"))
setClassUnion("functionORNULL", c("function", "NULL"))
setClassUnion("integerORNULL", c("integer", "NULL"))

