﻿
#r "FsLexYacc.Runtime.10.0.0/lib/net46/FsLexYacc.Runtime.dll"

open FSharp.Text.Lexing
open System

#load "CalculatorTypesAST.fsx"
open CalculatorTypesAST
#load "Parser.fs"
open Parser
#load "Lexer.fs"
open Lexer

exception ParseError of Position * string * Exception

let parse src =
    let lexbuf = LexBuffer<char>.FromString src

    let parser = Parser.start Lexer.tokenize
    try
        Ok(parser lexbuf)
    with e ->
        let pos = lexbuf.EndPos
        let line = pos.Line
        let column = pos.Column
        let message = e.Message
        let lastToken = new System.String(lexbuf.Lexeme)
        printf "Parse failed at line %d, column %d:\n" line column
        printf "Last token: %s" lastToken
        printf "\n"
        Error(ParseError(pos, lastToken, e))

let unwrap =
    function
    | Ok r -> r
    | Error e -> raise e

// We define the evaluation function recursively, by induction on the structure
// of arithmetic expressions (AST of type expr)
let rec evalC (e:typeC) =
  match e with
  | Skip(x) -> ""
  | AssExpr(x,y) -> evalS(x)+":="+evalA(y)
  | IfExpr(x) -> "if " + evalGC(x) + " fi"
  | DoExpr(x) -> "do " + evalGC(x) + " od"
  | SemiExpr(x,y) -> evalC(x) + ";\n" + evalC(y)

and evalGC (e:GC) = 
    match e with 
    | ArrExpr(b,C) -> if evalB(b) then evalC(C)
    | ElseExpr(gc1,gc2) -> evalGC(gc1); evalGC(gc2)

and evalA (e:arithmetic1) = 
    match e with
    | Num(x) -> x
    | TimesExpr(x,y) -> evalA(x) * evalA(y)
    | DivExpr(x,y) -> evalA(x) / evalA(y)
    | PlusExpr(x,y) -> evalA(x) + evalA(y)
    | MinusExpr(x,y) -> evalA(x) - evalA(y)
    | PowExpr(x,y) -> Math.Pow(evalA(x),evalA(y))
    | UPlusExpr(x) -> evalA(x)
    | UMinusExpr(x) -> -evalA(x)

and evalS (e:string1) = 
    match e with
    | Name(x) -> x

and evalB (e:boolean1) = 
    match e with
    | True(x) -> true
    | False(x) -> false
    | AndExpr(x,y) -> x && y
    | OrExpr(x,y) ->  x || y
    | UAndExpr(x,y) -> x & y
    | NotExpr(x) -> not x
    | GrExpr(x,y) -> x > y
    | LeExpr(x,y) -> x < y
    | GrEqExpr(x,y) -> x >= y
    | LeEqExpr(x,y) ->  x <= y
    | EqualsExpr(x,y) -> x = y
    | NotEqualsExpr(x,y) -> not (x = y)
    | UOrExpr(x,y) -> true



// We implement here the function that interacts with the user
let rec compute n =
    if n = 0 then
        printfn "Bye bye"
    else
        printf "Enter a command: "
        try
        // We parse the input string
        let e = parse (Console.ReadLine()) |> unwrap
        // and print the result of evaluating it
        printfn "Result: %A" (e)
//        printfn "Result: %s" (evalC(e))
        compute n
        with err -> compute (n-1)

// Start interacting with the user
compute 3

