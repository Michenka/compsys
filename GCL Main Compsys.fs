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

// command type for evaluation
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


// Retrieves the boolean in an arrow, or looks for it in an else condition
let rec extractBool e = 
  match e with
  | ArrExpr(x,y) -> [x]
  | ElseExpr(gc1,gc2) -> extractBool gc1 @ extractBool gc2

// splits an edge function into start node, the label it requires (which can be dependent on det or nondet), and end node.
let rec edgesC start stop e (edges,n)= 
    match e with
    | Skip(x) -> ((start,SkipLabel,stop)::edges,n) // start to stop with skip label and add it to the initial empty list called edges
    | AssExpr(x,y) -> ((start, AssLabel(x,y), stop)::edges,n) // as simple as skip, start to stop with assign label
    | IfExpr(x) -> edgesGC start stop x (edges,n) // Run edges but GC version with the same start stop, but with expression x, and same empty edges list.
    | DoExpr(x) -> edgesGC start start x ((start,Done(extractBool x),stop)::edges,n) // Similar to if, but start and end node are both "start", add Done(extractBool x) to edges list.
    | SemiExpr(x,y) -> edgesC start ("q"+string n) x (edgesC ("q"+ string n) stop y (edges,n)) // run with start, and form an end node using counter 'n', then run the second command with start node equal to the
                                                                                               // previous end node, and continue recursively. This adds numbers 'n' to the node, end of recursion.

// runs edges function to produce start stop and labels, but for guarded commands.
and edgesGC start stop e (edges,n) = 
    match e with
    | ArrExpr(x,y) -> edgesC ("q"+ string n) stop y ((start, BoolLabel(x) , "q"+ string n)::edges,n+1) // runs edges with start node made from 'n', on input 'y'. But stores the boolean with boolLabel into list edges
    | ElseExpr(gc1,gc2) -> edgesGC start stop gc2 (edgesGC start stop gc1 (edges,n)) // runs edgesGC on gc1 with empty list edges and counter n, then uses this result to run edgesGC on gc2.

// last two edges functions, deterministic version, produces start stop and label for commands.
let rec detEdgesC start stop (e:TypeC) (edges,n)= // realized this was the same code as C and GC, the split to determinism only happens at a later time for function printEdgesDet
    match e with
    | Skip(x) -> ((start,SkipLabel,stop)::edges,n)
    | AssExpr(x,y) -> ((start, AssLabel(x,y), stop)::edges,n)
    | IfExpr(x) -> detEdgesGC start stop x (edges,n) 
    | DoExpr(x) -> detEdgesGC start start x ((start,Done(extractBool x),stop)::edges,n)
    | SemiExpr(x,y) -> detEdgesC start ("q"+string n) x (detEdgesC ("q"+ string n) stop y (edges,n)) // all the same but loops within this recursion and not nondet version

and detEdgesGC start stop e (edges,n) = // still same 
    match e with
    | ArrExpr(x,y) -> detEdgesC ("q"+ string n) stop y ((start, BoolLabel(x), "q"+ string n)::edges,n+1)
    | ElseExpr(gc1,gc2) -> detEdgesGC start stop gc2 (detEdgesGC start stop gc1 (edges,n))

// function to convert the newly made edges list to nodes
let rec Node edges node = 
    match edges with 
    | [] -> []
    | (start,label,stop)::tail -> if node = start then (label,stop)::(Node tail node) else Node tail node // arranges a new list with tuples containing label and end node, repeats till all are in this form

//creates a list (actually a set) of all nodes
let rec Nodes edges nodes = 
  match edges with 
  | [] -> nodes 
  | (start,label,stop)::tail when List.exists (fun c -> c = start) nodes -> Nodes tail nodes // if start exists in nodes, then recurse through tail 
  | (start,label,stop)::tail -> Nodes tail (start::nodes) // and if not then add start to nodes and recurse through tail

let Labels edges = List.map (fun l -> (l,Node edges l)) (Nodes edges []) // given input edges, perform a map which produces tuple with itself and after Node has been performed

// uses general label, and produces the deterministic version
let rec makeDet detList label= 
  match detList with
  | [] -> []
  | (x,qf)::ltail -> (UAndExpr(x,NotExpr(label)),qf):: (makeDet ltail (UOrExpr(x,label))) // the first condition becomes label & negation of the rest of the label. The rest of the label depends on how many
                                                                                          // conditions you have, if only one then the rest of the label is "false", as soon as you have more than one condition
                                                                                          // they are placed in a unary or with "false", then when the first condition is reached it is & and negated with the rest

// evaluates complicated booleans to their simple type using evalB, then places them in a unary or with the rest of the booleans.
let rec printDone bls =
  match bls with 
  | [] -> ""
  | b::bl -> evalB(b)+"|"+(printDone bl)

// prints normal labels by using evalC and booleans with evalB. We keep qf inside the tuple to keep track of which node this label is printed to.
let printLabels l = 
  match l with
  | (AssLabel(x,y),qf) -> (evalC(AssExpr(x,y)),qf)
  | (BoolLabel(x),qf) -> (evalB(x),qf)
  | (SkipLabel,qf) -> ("skip",qf) // print skip on edge to qf
  | (Done(bls),qf) -> ("!("+printDone bls + ")",qf) // done in FM4FUN is shown as a negated list of all previous conditions, heading to universal end node

// finishes the done statement with false if empty, or adds all other conditions in a unary or link between them. it is called neg tho it does not negate, this happens later in helper2
let rec neg d = 
  match d with
  | [] -> False("")
  | d::dtail -> UOrExpr(d, neg dtail)

// if the lexpr is a boolean label return it, otherwise if it is a done statement with form as in makeDet comments, then negate the list produced by neg function above. This correctly prints a done label independent of input
let helper2 lexpr = 
  match lexpr with
    | BoolLabel(x) -> x
    | Done(ls) -> NotExpr(neg ls)

// simple helper used later on prolly
let isBool l = 
  match l with 
    | BoolLabel(x) -> true
    | _ -> false

let rec printHelper ls = List.map (fun t -> printLabels t) ls // takes list ls and maps each element to a print labels, simple helper

// finally we split det and nondet behaviour
let printHelperDet (qs,ls) = 
    match ls with 
        | [] -> (qs,[])
        | (BoolLabel(x),qf)::ltail -> (qs,List.map (fun (x,qf) -> (evalB(x),qf)) ((makeDet (List.map (fun (c,qf) -> (helper2 c,qf)) (List.filter (fun (label,qf) -> isBool label) ls)) (False("")))@(List.map (fun (c,qf) -> (helper2 c,qf)) (List.filter (fun(k,qf) -> not(isBool k)) ls))))
        | l::ltail -> (qs,[printLabels l])

// map edges which holds a start node and a list, to the function printHelperDet, which converts it into print ready statements only for det statements.
let printEdgesDet edges =  List.map (fun (qs,ls) -> printHelperDet (qs,ls)) edges;;

// does the same for nondet labels
let printEdges edges = List.map (fun (qs,ls) -> (qs,printHelper ls)) edges

// prints the list into a form which can be understood by graphviz, the red are for "" but as a string and not string operators.
let rec strDet1 qs ls =
  match ls with
    | [] -> ""
    | (label,qf)::ltail -> qs + "->"+qf+"[label =  \" " + label + " \"]\n " + strDet1 qs ltail+" "

// simpler helper that takes, a tuple and performs the previous function recursively on tail
let rec strDet2 ls = 
  match ls with
    | [] -> ""
    |(qs,ls)::ltail -> strDet1 qs ls + strDet2 ltail

// simple function to add syntax needed for graphviz
let graphStr edges = "digraph D { "+strDet2 edges+" }"

// two functions identical, one runs nondet and the other det, could be optimized optional
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

let merge mem1 mem2 = Map.fold (fun acc key value -> Map.add key value acc) mem1 mem2;;

let rec A a memory = 
    match a with
    | Name(x) -> Map.tryFind x memory 
    | Num(x) -> Some x
    | PlusExpr(x,y) -> match A x memory, A y memory with
                            | Some n1, Some n2 -> Some (n1+n2)
                            | _ -> None
    | MinusExpr(x,y) -> match A x memory, A y memory with
                            | Some n1, Some n2 -> Some (n1-n2)
                            | _ -> None
    | TimesExpr(x,y) -> match A x memory, A y memory with
                            | Some n1, Some n2 -> Some (n1*n2)
                            | _ -> None
    | DivExpr(x,y) -> match A x memory, A y memory with
                            | Some n1, Some n2 -> Some (n1/n2)
                            | _ -> None
    | PowExpr(x,y) -> match A x memory, A y memory with
                            | Some n1, Some n2 when n2 >= 0 -> Some (int (double n1 ** double n2))
                            | _ -> None
    | Array(x,y) -> if Map.containsKey x memory then A y memory else None
    // A[3] = 

let rec B b memory = 
    match b with
    | True(x) -> Some true
    | False(x) -> Some false
    | AndExpr(x,y) -> match B x memory, B y memory with
                        | Some b1, Some b2 -> Some (b1 && b2)
                        | _ -> None
    | OrExpr(x,y) ->  match B x memory, B y memory with
                        | Some b1, Some b2 -> Some (b1 || b2)
                        | _ -> None
    //dunno about these
    // | UAndExpr(x,y) -> match B x memory, B y memory with
    //                     | Some b1, Some b2 -> Some (b1 & b2)
    //                     | _ -> None
    // | UOrExpr(x,y) -> match B x memory, B y memory with
    //                     | Some b1, Some b2 -> Some (b1 | b2)
    //                     | _ -> None
    | NotExpr(x) -> match B x memory with
                        | Some b -> Some (not b)
                        | _ -> None
    | GrExpr(x,y) -> match A x memory, A y memory with
                        | Some n1, Some n2 -> Some (n1 > n2)
                        | _ -> None
    | LeExpr(x,y) ->  match A x memory, A y memory with
                        | Some n1, Some n2 -> Some (n1 < n2)
                        | _ -> None
    | GrEqExpr(x,y) ->  match A x memory, A y memory with
                        | Some n1, Some n2 -> Some (n1 >= n2)
                        | _ -> None
    | LeEqExpr(x,y) ->   match A x memory, A y memory with
                            | Some n1, Some n2 -> Some (n1 <= n2)
                            | _ -> None
    | EqualsExpr(x,y) ->  match A x memory, A y memory with
                            | Some n1, Some n2 -> Some (n1 = n2)
                            | _ -> None
    | NotEqualsExpr(x,y) ->  match A x memory, A y memory with
                                | Some n1, Some n2 -> Some (not(n1 = n2))
                                | _ -> None




let rec S action memory = 
    match action with
    | Skip(x) -> Some memory
    | AssExpr(x,y) -> match A x memory with
                        | None -> None
                        | _ -> match x with
                                | Name(str) ->  match A y memory with
                                                    | None -> None
                                                    | Some n -> Some (memory |> Map.add str n) 
                                | _ -> None
    | DoExpr(x) -> SBdo x memory
    | IfExpr(x) -> SBif x memory
and SBdo action memory = 
    match action with
    | ArrExpr(b,C) -> match B b memory with
                        | Some k -> if k then SBdo action (S C memory).Value else Some memory
                        | _ -> None
    //deterministic and non-deterministic is decided here
    | ElseExpr(gc1,gc2) -> match SBdo gc1 memory, SBdo gc2 memory with
                            | Some k, _ -> Some k
                            | None, Some m -> Some m
                            | _ -> None
and SBif action memory = 
    match action with
    | ArrExpr(b,C) -> match B b memory with
                        | Some k -> if k then S C memory else Some memory
                        | _ -> None
    //deterministic and non-deterministic is decided here
    | ElseExpr(gc1,gc2) -> match SBif gc1 memory, SBif gc2 memory with
                            | Some k, Some m -> SBif gc2 m
                            | _ -> None


//initiate memory
let rec Ini map ls = 
    match ls with
    | [] -> map
    | (Name(n),Num(x))::ltail -> Ini (map |> Map.add n x) ltail
    | _ -> map


let ini = [(Name("x"),Num(1));(Name("y"),Num(0))];;

let map = Ini Map.empty ini;;

//examples:
let eif = IfExpr
            (ElseExpr
              (ArrExpr (GrExpr (Name "x", Num 3), AssExpr (Name "y", Num 4)),
                  ArrExpr (LeExpr (Name "x", Num 2), AssExpr (Name "y", Num 2))));;


//if-expressions work
let mapA = S eif map;;
mapA.Value;;

//do-expression works too
let det = DoExpr (ArrExpr (GrExpr (Name "x", Num 3), AssExpr (Name "x", MinusExpr(Name "x",Num 1))));;
let ini2 = [(Name "x",Num 5)];;
let map2 = Ini Map.empty ini2;;
let mapB = S det map2;;

//stuck configuration
let ini3 = [(Name "k",Num 5)];;
let map3 = Ini Map.empty ini3;;
let mapC = S det map3;;

//now idk how to connect that to the program graph 
let rec printMem mem = 
    match mem with
    | [] -> ""
    | (str, n)::ltail -> str+": "+string n+"\n"+printMem ltail

let runInterpreter e memory =
    match S e memory with
    | Some mem -> printMem (Map.toList mem)
    | None -> "stuck"

eif;;
map;;
runInterpreter det map3;;