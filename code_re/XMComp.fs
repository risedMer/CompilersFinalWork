module XMComp

open System.IO
open XMAbsyn
open XMMachine
open Debug
open Backend

type 'data Env = (string * 'data) list

let rec lookup env x =
    match env with
    | [] -> failwith (x + " not found")
    | (y, v) :: yr -> if x = y then v else lookup yr x

type Var =
    | Glovar of int (* absolute address in stack           *)
    | Locvar of int (* address relative to bottom of frame *)

type VarEnv = (Var * typ) Env * int

type Paramdecs = (typ * string) list

type FunEnv = (label * typ option * Paramdecs) Env

let isX86Instr = ref false

let rec allocateWithMsg (kind: int -> Var) (typ, x) (varEnv: VarEnv) =
    let varEnv, instrs =
        allocate (kind: int -> Var) (typ, x) (varEnv: VarEnv)
    msg
    <| "\nalloc\n"
       + sprintf "%A\n" varEnv
       + sprintf "%A\n" instrs
    (varEnv, instrs)

and allocate (kind: int -> Var) (typ, x) (varEnv: VarEnv) : VarEnv * instr list =
    msg $"allocate called!{(x, typ)}"
    let (env, newloc) = varEnv
    match typ with
    | TypA (TypA _, _) -> raise (Failure "allocate: array of arrays not permitted")
    | TypA (t, Some i) ->
        let newEnv =
            ((x, (kind (newloc + i), typ)) :: env, newloc + i + 1) //数组内容占用 i个位置,数组变量占用1个位置
        let code = [ INCSP i; GETSP; OFFSET(i - 1); SUB ]
        (newEnv, code)
    | _ ->
        let newEnv =
            ((x, (kind (newloc), typ)) :: env, newloc + 1)
        let code = [ INCSP 1 ]
        // info (fun () -> printf "new varEnv: %A\n" newEnv) // 调试 显示分配后环境变化
        (newEnv, code)

let bindParam (env, newloc) (typ, x) : VarEnv =
    ((x, (Locvar newloc, typ)) :: env, newloc + 1)

let bindParams paras ((env, newloc): VarEnv) : VarEnv = List.fold bindParam (env, newloc) paras

let makeGlobalEnvs (topdecs: topdec list) : VarEnv * FunEnv * instr list =
    let rec addv decs varEnv funEnv =
        msg $"\nGlobal funEnv:\n{funEnv}\n"
        match decs with
        | [] -> (varEnv, funEnv, [])
        | dec :: decr ->
            match dec with
            | Vardec (typ, var) ->
                let (varEnv1, code1) = allocateWithMsg Glovar (typ, var) varEnv
                let (varEnvr, funEnvr, coder) = addv decr varEnv1 funEnv
                (varEnvr, funEnvr, code1 @ coder)
            | Fundec (tyOpt, f, xs, body) -> addv decr varEnv ((f, ($"{newLabel ()}_{f}", tyOpt, xs)) :: funEnv)
    addv topdecs ([], 0) []

let x86patch code =
    if !isX86Instr then
        code @ [ CSTI -8; MUL ] // x86 偏移地址*8
    else
        code 

(* Compiling XM statements:
   * stmt    is the statement to compile
   * varenv  is the local and global variable environment
   * funEnv  is the global function environment
*)

let rec cStmt stmt (varEnv: VarEnv) (funEnv: FunEnv) : instr list =
    match stmt with
    | If (e, stmt1, stmt2) ->
        let labelse = newLabel ()
        let labend = newLabel ()
        cExpr e varEnv funEnv
        @ [ IFZERO labelse ]
          @ cStmt stmt1 varEnv funEnv
            @ [ GOTO labend ]
              @ [ Label labelse ]
                @ cStmt stmt2 varEnv funEnv @ [ Label labend ]
    | While (e, body) ->
        let labbegin = newLabel ()
        let labtest = newLabel ()
        [ GOTO labtest; Label labbegin ]
        @ cStmt body varEnv funEnv
          @ [ Label labtest ]
            @ cExpr e varEnv funEnv @ [ IFNZRO labbegin ]
    | Expr e -> cExpr e varEnv funEnv @ [ INCSP -1 ]
    | Block stmts ->
        let rec loop stmts varEnv =
            match stmts with
            | [] -> (snd varEnv, [])
            | s1 :: sr ->
                let (varEnv1, code1) = cStmtOrDec s1 varEnv funEnv
                let (fdepthr, coder) = loop sr varEnv1
                (fdepthr, code1 @ coder)
        let (fdepthend, code) = loop stmts varEnv
        code @ [ INCSP(snd varEnv - fdepthend) ]
    | Return None -> [ RET(snd varEnv - 1) ]
    | Return (Some e) -> cExpr e varEnv funEnv @ [ RET(snd varEnv) ]

and cStmtOrDec stmtOrDec (varEnv: VarEnv) (funEnv: FunEnv) : VarEnv * instr list =
    match stmtOrDec with
    | Stmt stmt -> (varEnv, cStmt stmt varEnv funEnv)
    | Dec (typ, x) -> allocateWithMsg Locvar (typ, x) varEnv

(* Compiling XM expressions:
   * e       is the expression to compile
   * varEnv  is the local and gloval variable environment
   * funEnv  is the global function environment

   Net effect principle: if the compilation (cExpr e varEnv funEnv) of
   expression e returns the instruction sequence instrs, then the
   execution of instrs will leave the rvalue of expression e on the
   stack top (and thus extend the current stack frame with one element).
*)



and cExpr (e: expr) (varEnv: VarEnv) (funEnv: FunEnv) : instr list =
    match e with
    | Access acc -> cAccess acc varEnv funEnv @ [ LDI ]
    | Assign (acc, e) ->
        cAccess acc varEnv funEnv
        @ cExpr e varEnv funEnv @ [ STI ]
    | CstI i -> [ CSTI i ]
    | CstF i -> [ CSTFF i ]
    | Addr acc -> cAccess acc varEnv funEnv
    | Prim1 (ope, e1) ->
        cExpr e1 varEnv funEnv
        @ (match ope with
           | "!" -> [ NOT ]
           | "printi" -> [ PRINTI ]
           | "printc" -> [ PRINTC ]
           | _ -> raise (Failure "unknown primitive 1"))
    | Prim2 (ope, e1, e2) ->
        cExpr e1 varEnv funEnv
        @ cExpr e2 varEnv funEnv
          @ (match ope with
             | "*" -> [ MUL ]
             | "+" -> [ ADD ]
             | "-" -> [ SUB ]
             | "/" -> [ DIV ]
             | "%" -> [ MOD ]
             | "==" -> [ EQ ]
             | "!=" -> [ EQ; NOT ]
             | "<" -> [ LT ]
             | ">=" -> [ LT; NOT ]
             | ">" -> [ SWAP; LT ]
             | "<=" -> [ SWAP; LT; NOT ]
             | _ -> raise (Failure "unknown primitive 2"))
    | Andalso (e1, e2) ->
        let labend = newLabel ()
        let labfalse = newLabel ()
        cExpr e1 varEnv funEnv
        @ [ IFZERO labfalse ]
          @ cExpr e2 varEnv funEnv
            @ [ GOTO labend
                Label labfalse
                CSTI 0
                Label labend ]
    | Orelse (e1, e2) ->
        let labend = newLabel ()
        let labtrue = newLabel ()
        cExpr e1 varEnv funEnv
        @ [ IFNZRO labtrue ]
          @ cExpr e2 varEnv funEnv
            @ [ GOTO labend
                Label labtrue
                CSTI 1
                Label labend ]
    | Call (f, es) -> callfun f es varEnv funEnv

and cAccess access varEnv funEnv : instr list =
    match access with
    | AccVar x ->
        match lookup (fst varEnv) x with
        | Glovar addr, _ ->
            if !isX86Instr then
                [ GVAR addr ]
            else
                [ CSTI addr ]
        | Locvar addr, _ -> [ GETBP; OFFSET addr; ADD ]
    | AccDeref e ->
        match e with
        | Access _ -> (cExpr e varEnv funEnv)
        | Addr _ -> (cExpr e varEnv funEnv)
        | _ ->
            printfn "WARN: x86 pointer arithmetic not support!"
            (cExpr e varEnv funEnv)
    | AccIndex (acc, idx) ->
        cAccess acc varEnv funEnv
        @ [ LDI ]
          @ x86patch (cExpr idx varEnv funEnv) @ [ ADD ]

and cExprs es varEnv funEnv : instr list =
    List.concat (List.map (fun e -> cExpr e varEnv funEnv) es)

and callfun f es varEnv funEnv : instr list =
    let (labf, tyOpt, paramdecs) = lookup funEnv f
    let argc = List.length es
    if argc = List.length paramdecs then
        cExprs es varEnv funEnv @ [ CALL(argc, labf) ]
    else
        raise (Failure(f + ": parameter/argument mismatch"))

let argc = ref 0

let cProgram (Prog topdecs) : instr list =
    let _ = resetLabels ()
    let ((globalVarEnv, _), funEnv, globalInit) = makeGlobalEnvs topdecs
    let compilefun (tyOpt, f, xs, body) =
        let (labf, _, paras) = lookup funEnv f
        let paraNums = List.length paras
        let (envf, fdepthf) = bindParams paras (globalVarEnv, 0)
        let code = cStmt body (envf, fdepthf) funEnv
        [ FLabel (paraNums, labf) ]
        @ code @ [ RET(paraNums - 1) ]
    let functions =
        List.choose
            (function
            | Fundec (rTy, name, argTy, body) -> Some(compilefun (rTy, name, argTy, body))
            | Vardec _ -> None)
            topdecs
    let (mainlab, _, mainparams) = lookup funEnv "main"
    argc := List.length mainparams
    globalInit
    @ [ LDARGS !argc
        CALL(!argc, mainlab)
        STOP ]
      @ List.concat functions

let intsToFile (inss: int list) (fname: string) =
    File.WriteAllText(fname, String.concat " " (List.map string inss))

let writeInstr fname instrs =
    let ins =
        String.concat "\n" (List.map string instrs)
    File.WriteAllText(fname, ins)
    printfn $"VM instructions saved in file:\n\t{fname}"

let compileToFile program fname =
    msg <|sprintf "program:\n %A" program
    let instrs = cProgram program
    msg <| sprintf "\nStack VM instrs:\n %A\n" instrs
    writeInstr (fname + ".ins") instrs
    let bytecode = code2ints instrs
    msg <| sprintf "Stack VM numeric code:\n %A\n" bytecode
    isX86Instr := true
    let x86instrs = cProgram program
    writeInstr (fname + ".insx86") x86instrs
    let x86asmlist = List.map emitx86 x86instrs
    let x86asmbody =
        List.fold (fun asm ins -> asm + ins) "" x86asmlist
    let x86asm =
        (x86header + beforeinit !argc + x86asmbody)
    printfn $"x86 assembly saved in file:\n\t{fname}.asm"
    File.WriteAllText(fname + ".asm", x86asm)
    intsToFile bytecode (fname + ".out")
    instrs