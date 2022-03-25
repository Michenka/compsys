// This file implements a module where we define a data type "expr"
// to store represent arithmetic expressions
module CalculatorTypesAST

    type arithmetic1 = 
    | Num of int
    | Name of string
    | TimesExpr of (arithmetic1 * arithmetic1)
    | DivExpr of (arithmetic1 * arithmetic1)
    | PlusExpr of (arithmetic1 * arithmetic1)
    | MinusExpr of (arithmetic1 * arithmetic1)
    | PowExpr of (arithmetic1 * arithmetic1)
    | UPlusExpr of (arithmetic1)
    | UMinusExpr of (arithmetic1)
    | Array of (string*arithmetic1)


    and Boolean1 =
    | True of string
    | False of string 
    | AndExpr of (Boolean1*Boolean1)
    | OrExpr of (Boolean1*Boolean1)
    | UAndExpr of (Boolean1*Boolean1)
    | UOrExpr of (Boolean1*Boolean1)
    | NotExpr of (Boolean1)
    | GrExpr of (arithmetic1 * arithmetic1)
    | LeExpr of (arithmetic1 * arithmetic1)
    | GrEqExpr of (arithmetic1 * arithmetic1)
    | LeEqExpr of (arithmetic1 * arithmetic1)
    | EqualsExpr of (arithmetic1 * arithmetic1)
    | NotEqualsExpr of (arithmetic1 * arithmetic1)

    and TypeC = 
    | AssExpr of (arithmetic1 * arithmetic1)
    | IfExpr of (GC)
    | DoExpr of (GC)
    | SemiExpr of (TypeC*TypeC)
    | Skip of string

    and GC =
    | ArrExpr of (Boolean1*TypeC)
    | ElseExpr of (GC*GC)

    type labelExpr = 
    | SkipLabel
    | AssLabel of (arithmetic1 * arithmetic1)
    | BoolLabel of Boolean1
    | Done of list<Boolean1>

