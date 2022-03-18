

open System.IO



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


let rec evalC (e:TypeC) =
  match e with
  | Skip(x) -> "skip"
  | AssExpr(x,y) -> evalA(x)+":="+evalA(y)
  | IfExpr(x) -> "if " + evalGC(x) + " fi"
  | DoExpr(x) -> "do " + evalGC(x) + " od"
  | SemiExpr(x,y) -> evalC(x) + ";\n" + evalC(y)

and evalGC (e:GC) = 
    match e with 
    | ArrExpr(b,C) -> evalB(b)+"->"+evalC(C)
    | ElseExpr(gc1,gc2) -> evalGC(gc1)+"[]"+evalGC(gc2)

and evalA (e:arithmetic1) = 
    match e with
        | Name(x) -> x
        | Num(x) -> string x
        | TimesExpr(x,y) -> "(" + evalA(x) + "*" + evalA(y) + ")"
        | DivExpr(x,y) -> "(" + evalA(x) + "/" + evalA(y) + ")"
        | PlusExpr(x,y) -> evalA(x) + "+" + evalA(y)
        | MinusExpr(x,y) -> evalA(x) + "-" + evalA(y)
        | PowExpr(x,y) -> evalA(x) + "^" + "(" + evalA(y) + ")"
        | UPlusExpr(x) -> "(" + "+" + evalA(x) + ")"
        | UMinusExpr(x) -> "(" + "-" + evalA(x) + ")"
        | Array(n,x) -> n+"["+"]"+evalA(x)

and evalB (e:Boolean1) = 
    match e with
    | True(x) -> "true" 
    | False(x) -> "false"
    | AndExpr(x,y) -> "("+evalB(x) + "&& " + evalB(y)+")"
    | OrExpr(x,y) ->  "(" + evalB(x) + "||" + evalB(y) + ")"
    | UAndExpr(x,y) -> "(" + evalB(x) + "&" + evalB(y) + ")"
    | UOrExpr(x,y) -> "( " + evalB(x) + " | " + evalB(y) + " )"
    | NotExpr(x) -> "!(" + (evalB(x)) + ")"
    | GrExpr(x,y) -> "( " + evalA(x) + " > " + evalA(y) + " )"
    | LeExpr(x,y) -> "( " + evalA(x) + " < " + evalA(y) + " )"
    | GrEqExpr(x,y) -> "( " + evalA(x) + " >= " + evalA(y) + " )"
    | LeEqExpr(x,y) ->  "( " + evalA(x) + " <= " + evalA(y) + " )"
    | EqualsExpr(x,y) -> "( " + evalA(x) + " = " + evalA(y) + " )"
    | NotEqualsExpr(x,y) -> "( " + evalA(x) + " != " + evalA(y) + " )"

let rec extractBool e = 
  match e with
  | ArrExpr(x,y) -> [x]
  | ElseExpr(gc1,gc2) -> extractBool gc1 @ extractBool gc2

let rec edgesC start stop e (edges,n)= 
    match e with
    | Skip(x) -> ((start,SkipLabel,stop)::edges,n)
    | AssExpr(x,y) -> ((start, AssLabel(x,y), stop)::edges,n)
    | IfExpr(x) -> edgesGC start stop x (edges,n) 
    | DoExpr(x) -> edgesGC start start x ((start,Done(extractBool x),stop)::edges,n)
    | SemiExpr(x,y) -> edgesC start ("q"+string n) x (edgesC ("q"+ string n) stop y (edges,n))

and edgesGC start stop e (edges,n) = 
    match e with
    | ArrExpr(x,y) -> edgesC ("q"+ string n) stop y ((start, BoolLabel(x) , "q"+ string n)::edges,n+1)
    | ElseExpr(gc1,gc2) -> edgesGC start stop gc2 (edgesGC start stop gc1 (edges,n))

let rec detEdgesC start stop (e:TypeC) (edges,n)= 
    match e with
    | Skip(x) -> ((start,SkipLabel,stop)::edges,n)
    | AssExpr(x,y) -> ((start, AssLabel(x,y), stop)::edges,n)
    | IfExpr(x) -> detEdgesGC start stop x (edges,n) 
    | DoExpr(x) -> detEdgesGC start start x ((start,Done(extractBool x),stop)::edges,n)
    | SemiExpr(x,y) -> detEdgesC start ("q"+string n) x (detEdgesC ("q"+ string n) stop y (edges,n))

and detEdgesGC start stop e (edges,n) = 
    match e with
    | ArrExpr(x,y) -> detEdgesC ("q"+ string n) stop y ((start, BoolLabel(x), "q"+ string n)::edges,n+1)
    | ElseExpr(gc1,gc2) -> detEdgesGC start stop gc2 (detEdgesGC start stop gc1 (edges,n))

let rec Node edges node = 
    match edges with 
    | [] -> []
    | (start,label,stop)::tail -> if node = start then (label,stop)::(Node tail node) else Node tail node

//creates a list (actually a set) of all nodes
let rec Nodes edges nodes = 
  match edges with 
  | [] -> nodes
  | (start,label,stop)::tail when List.exists (fun c -> c = start) nodes -> Nodes tail nodes 
  | (start,label,stop)::tail -> Nodes tail (start::nodes)

let Labels edges = List.map (fun l -> (l,Node edges l)) (Nodes edges [])

let rec makeDet detList label= 
  match detList with
  | [] -> []
  | (x,qf)::ltail -> (UAndExpr(x,NotExpr(label)),qf):: (makeDet ltail (UOrExpr(x,label)))

let rec printDone bls =
  match bls with 
  | [] -> ""
  | b::bl -> evalB(b)+"|"+(printDone bl)

let printLabels l = 
  match l with
  | (AssLabel(x,y),qf) -> (evalC(AssExpr(x,y)),qf)
  | (BoolLabel(x),qf) -> (evalB(x),qf)
  | (SkipLabel,qf) -> ("skip",qf)
  | (Done(bls),qf) -> ("!("+printDone bls + ")",qf)

let rec neg d =
  match d with
  | [] -> False("")
  | d::dtail -> UOrExpr(d, neg dtail)


let helper2 lexpr = 
  match lexpr with
    | BoolLabel(x) -> x
    | Done(ls) -> NotExpr(neg ls)

let isBool l = 
  match l with 
    | BoolLabel(x) -> true
    | _ -> false


let rec printHelper ls = List.map (fun t -> printLabels t) ls

let printHelperDet (qs,ls) = 
    match ls with 
        | [] -> (qs,[])
        | (BoolLabel(x),qf)::ltail -> (qs,List.map (fun (x,qf) -> (evalB(x),qf)) ((makeDet (List.map (fun (c,qf) -> (helper2 c,qf)) (List.filter (fun (label,qf) -> isBool label) ls)) (False("")))@(List.map (fun (c,qf) -> (helper2 c,qf)) (List.filter (fun(k,qf) -> not(isBool k)) ls))))
        | l::ltail -> (qs,[printLabels l])


let printEdgesDet edges =  List.map (fun (qs,ls) -> printHelperDet (qs,ls)) edges;;

let printEdges edges = List.map (fun (qs,ls) -> (qs,printHelper ls)) edges

let rec strDet1 qs ls =
  match ls with
    | [] -> ""
    | (label,qf)::ltail -> qs + "->"+qf+"[label =  \" " + label + " \"]\n " + strDet1 qs ltail+" "

let rec strDet2 ls = 
  match ls with
    | [] -> ""
    |(qs,ls)::ltail -> strDet1 qs ls + strDet2 ltail

let graphStr edges = "digraph D { "+strDet2 edges+" }"

let edges e = edgesC "qs" "qf" e ([],0);;
let detEdges e = detEdgesC "qs" "qf" e ([],0);;



//all you need for the execution is this function which takes expression e (from the Parser) and a bool det indication if the
//graph should be deterministic or not
let graph e det = 
    match det with
    | false -> match edges e with
                | (edge,n) ->  edge |> Labels |> printEdges |> graphStr
    | true -> match detEdges e with
                | (detEdge,n) ->  detEdge |> Labels |> printEdgesDet |> graphStr

//to write the string to a file you'll have to pass the working dictionary (wd) as well
let writeGraphDet wd edges det = File.WriteAllText(wd+"/graph.dot", (graph edges det));;

