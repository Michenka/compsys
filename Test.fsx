
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


let rec edgesC start stop e (edges,n)= 
    match e with
    | Skip(x) -> ((start,"skip",stop)::edges,n)
    | AssExpr(x,y) -> ((start, evalC e, stop)::edges,n)
    | IfExpr(x) -> edgesGC start stop x (edges,n) 
    | DoExpr(x) -> edgesGC start start x ((start,"done",stop)::edges,n)
    | SemiExpr(x,y) -> edgesC start ("q"+string n) x (edgesC ("q"+ string n) stop y (edges,n))

and edgesGC start stop e (edges,n) = 
    match e with
    | ArrExpr(x,y) -> edgesC ("q"+ string n) stop y ((start, evalB(x) , "q"+ string n)::edges,n+1)
    | ElseExpr(gc1,gc2) -> edgesGC start stop gc2 (edgesGC start stop gc1 (edges,n))

let rec extractBool e = 
  match e with
  | ArrExpr(x,y) -> [x]
  | ElseExpr(gc1,gc2) -> extractBool gc1 @ extractBool gc2
// and extractBoolGC e =
//   match e with
//   | ArrExpr(x,y) -> x
//   | ElseExpr(gc1,gc2) -> extractBoolGC(gc1)

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






let rec strEdges edges = 
    match edges with
    | [] -> ""
    | (start,label,stop)::tail -> start + "->"+stop+"[label =  \" " + label + " \"]\n " + strEdges tail+" "

// let detEdges edges = List.map (fun (start,label,stop) -> (start,label+" & !(false)",stop)) edges 

let makeGraph edges = "digraph D { "+strEdges edges+" }"

let wd = "/Users/ninapeuker/Desktop/General Engineering/4th semester 2022/02141 Computer Science Modelling/Code/Assignment1"

let writeGraph edges = File.WriteAllText(wd+"/graph.dot", (makeGraph edges));;



//helper functions to extract all labels/ transitions associated with each node
//extracts all transitions for a particular node
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


//outs it all together in the form (node* list <(labels*node)>)
let Labels edges = List.map (fun l -> (l,Node edges l)) (Nodes edges [])





let e1 = Skip("");;
let e2 = AssExpr(Name("x"),Num(4));;
let e22 = AssExpr(Name("y"),Num(3));;
let e3 = SemiExpr(e1,e2);;
let e4 = SemiExpr(e3,e22);;
let eif = IfExpr
            (ElseExpr
              (ArrExpr (GrExpr (Name "x", Num 3), AssExpr (Name "y", Num 4)),
                  ArrExpr (LeExpr (Name "x", Num 2), AssExpr (Name "y", Num 2))));;
let earr = IfExpr(ElseExpr(ArrExpr (GrEqExpr (Name "x", Name "y"), AssExpr (Name "z", Name "x")),
            ArrExpr (GrExpr (Name "y", Name "x"), AssExpr (Name "z", Name "y"))))
//IfExpr (ArrExpr (GrExpr (Name "x", Num 3), AssExpr (Name "y", Num 4)));;

let (edges1,n1) = edgesC "qs" "qf" earr ([],0);;
let (edges2,n2) = edgesC "qs" "qf" eif ([],0);;
let (detEdges2,dn2) = detEdgesC "qs" "qf" eif ([],0);;
let str1 = strEdges edges1;;




edges2;;
Labels edges2;;
detEdges2;;
Labels detEdges2;;

//make deterministic labels out of a list of Boolean1s
let rec makeDetLabels ls label= 
  match ls,label with
    | [],_ -> []
    | x::xtail, label -> UAndExpr(x,NotExpr(label)) :: (makeDetLabels xtail (UOrExpr(x,label)))


//take the list of nodes with transition and final node and map the makeLabel function to it, updating 
//the label every time
let rec makeDet detList label= 
  match detList with
  | [] -> []
  | (x,qf)::ltail -> (UAndExpr(x,NotExpr(label)),qf):: makeDet ltail (UOrExpr(x,label))

let makeDetEdges edges = List.map (fun (qs,ls) -> (qs, makeDet ls (False("")))) edges;;

// let convertBool (BoolLabel(x)) = x;;
// let convertAss (AssLabel(x,y)) = AssExpr(x,y);;

let rec printDone bls =
  match bls with 
  | [] -> ""
  | b::bl -> evalB(b)+"|"+(printDone bl)



//convert the labels to types
let printLabels l = 
  match l with
  | (AssLabel(x,y),qf) -> (evalC(AssExpr(x,y)),qf)
  | (BoolLabel(x),qf) -> (evalB(x),qf)
  | (SkipLabel,qf) -> ("skip",qf)
  | (Done(bls),qf) -> ("!("+printDone bls + ")",qf)

// let printEdges edges =  List.map (fun (qs,ls) -> (qs, List.map (fun l -> printLabels l) ls)) edges;;
let rec neg d =
  match d with
  | [] -> False("")
  | d::dtail -> UOrExpr(d, neg dtail)

let lsBool = [GrExpr(Name "x",Num 3); LeExpr(Name "x",Num 2)];;
neg lsBool;;

let helper2 lexpr = 
  match lexpr with
  | BoolLabel(x) -> x
  | Done(ls) -> NotExpr(neg ls)

let isBool l = 
  match l with 
  | BoolLabel(x) -> true
  | _ -> false

// let printHelper (qs,ls) = 
//   match ls with 
//   | [] -> (qs,[])
//   | (BoolLabel(x),qf)::ltail -> (qs,List.map (fun (x,qf) -> (evalB(x),qf)) (makeDet((x,qf)::(List.map (fun (lexp,qf) -> (helper2 lexp,qf)) (List.filter (fun (k,qf) -> isBool k) ls))) (False(""))))
//   | l::ltail -> (qs,[printLabels l])





let printHelper (qs,ls) = 
    match ls with 
    | [] -> (qs,[])
    | (BoolLabel(x),qf)::ltail -> (qs,List.map (fun (x,qf) -> (evalB(x),qf)) ((makeDet (List.map (fun c -> helper2 c) (List.filter (fun (label,qf) -> isBool label) ls)))@(List.map (fun c -> helper2 c) (List.filter (fun(k,qf) -> !(isBool k) ls)))))
    | l::ltail -> (qs,[printLabels l])

// let printHelper (qs,ls) = 
//   match ls with 
//   | [] -> (qs,[])
//   | (BoolLabel(x),qf)::ltail -> (qs,List.map (fun (x,qf) -> (evalB(x),qf)) (makeDet((x,qf)::(List.map (fun (lexp,qf) -> (helper2 lexp,qf)) ltail)) (False(""))))
//   | l::ltail -> (qs,[printLabels l])

let printEdges edges =  List.map (fun (qs,ls) -> printHelper (qs,ls)) edges;;


detEdges2;;
let detLabels2 = Labels detEdges2;;
printEdges detLabels2;;
//this is equivalent to running the edges and Label functions for a non-deterministic graph

edges2;;
Labels edges2;;

//To Do: transform the Do expression for the non-deterministic graph
let e = DoExpr (ArrExpr (EqualsExpr (Name "x", Num 3), AssExpr (Name "y", Num 4)));;
let (edges, n) =  edgesC "qs" "qf" e ([],0);;
let strLsE = Labels edges;;

let det = DoExpr (ArrExpr (EqualsExpr (Name "x", Num 3), AssExpr (Name "y", Num 4)));;
let (detEd, nDet) = detEdgesC "qs" "qf" det ([],0);;
let detLa = Labels detEd;;
let strLsDet = printEdges detLa;;



let strDetLabels bl= List.map (fun x -> evalB(x)) bl;; 



let lsBool = [GrExpr(Name "x",Num 3); LeExpr(Name "x",Num 2)];;
let boolL = makeDetLabels lsBool (False(""));;
strDetLabels boolL;;

let x1 = lsBool.[0];;
let f = False("");;
let u1 = UAndExpr(x1,NotExpr(f));;
evalB(u1);;
makeDetLabels lsBool f;;



evalB(GrExpr(Name "x",Num 3));;

// evalB(True(""));;

// edgesC "qs" "qf" e3 ([],0);;
// edgesC "qs" "qf" e1 ([],0);;
// edgesC "qs" "qf" e4 ([],0);;
edgesC "qs" "qf" eif ([],0);;




//be back soon