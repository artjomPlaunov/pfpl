structure TypeOps  = 
struct
  datatype t = NUM | STR
  
  fun arity NUM = []
    | arity STR = []

  fun equal (x : t, y : t) = x = y

  fun toString NUM = "num"
    | toString STR = "str"
end
