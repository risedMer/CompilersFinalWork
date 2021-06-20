module XMMachine

type label = string

// 汇编指令
type instr =
    | Label of label
    | FLabel of int * label
    | CSTI of int
    | CSTC of int                   (* Constant Char    new             *)
    | CSTF of int                   (* Constant Float   new             *)
    | GVAR of int                   (* global variable                  *)
    | OFFSET of int
    | ADD
    | SUB
    | MUL
    | DIV
    | MOD 
    | EQ
    | LT                            (* less then: s[sp-1] < s[sp]       *)
    | NOT                           (* s[sp] != 0                       *)
    | DUP                           (* 复制栈顶                          *)
    | SWAP                          (* swap s[sp-1] and s[sp]           *)
    | LDI                           (* get s[s[sp]]                     *)
    | STI                           (* set s[s[sp-1]]                   *)
    | GETBP                         (* get bp                           *)
    | GETSP
    | INCSP of int                  (* increase stack top by m          *)
    | GOTO of label                 
    | IFZERO of label               (* go to label if s[sp] == 0        *)
    | IFNZRO of label               (* go to label if s[sp] != 0        *)
    | CALL of int * label           (* move m args up 1,push pc,jump    *)
    | TCALL of int * int * label    (* move m args down n,jump          *)
    | RET of int                    (* pop m and return to s[sp]        *)
    | PRINTI                        (* print s[sp] as integer           *)
    | PRINTC                        (* print s[sp] as character         *)
    | PRINTF                        (* print s[sp] as float   new       *)
    | LDARGS of int                 (* load command line args on stack  *)
    | STOP                          (* halt the abstract machine        *)

let (resetLabels, newLabel) =
    let lastlab = ref - 1
    ((fun () -> lastlab := 0),(fun () -> (lastlab := 1 + !lastlab; "L" + (!lastlab).ToString())))

type 'data env = (string * 'data) list

let rec lookup env x =
    match env with
    | []            -> failwith (x + " not found")
    | (y,v) :: yr   -> x = y then v else lookup yr x

(*
    指令列表的发出分为两个阶段：
    1. 构建一个环境 labenv 将标签映射到地址
    2. 将代码发送到文件中，使用环境 labenv 解析标签

    标签码需要与后端一致
*)

// 机器码部分
[<Literal>]
let CODECSTI = 0

[<Literal>]
let CODEADD = 1

[<Literal>]
let CODESUB = 2

[<Literal>]
let CODEMUL = 3

[<Literal>]
let CODEDIV = 4

[<Literal>]
let CODEMOD = 5

[<Literal>]
let CODEEQ = 6

[<Literal>]
let CODELT = 7

[<Literal>]
let CODENOT = 8

[<Literal>]
let CODEUP = 9

[<Literal>]
let CODESWAP = 10

[<Literal>]
let CODELDI = 11

[<Literal>]
let CODESTI = 12

[<Literal>]
let CODEGETBP = 13

[<Literal>]
let CODEGETSP = 14

[<Literal>]
let CODEINCSP = 15

[<Literal>]
let CODEGOTO = 16

[<Literal>]
let CODEIFZERO = 17

[<Literal>]
let CODEIFNZRO = 18

[<Literal>]
let CODECALL = 19

[<Literal>]
let CODETCALL = 20

[<Literal>]
let CODERET = 21

[<Literal>]
let CODEPRINTI = 22

[<Literal>]
let CODEPRINTC = 23

[<Literal>]
let CODELDARGS = 24

[<Literal>]
let CODESTOP = 25

// new 
[<Literal>]
let CODECSTC = 26

[<Literal>]
let CODECSTF = 27

[<Literal>]
let CODEPRINTF = 28

// 获得标签在机器码中的地址
let makelabenv (addr,labenv) instr =
    match instr with
    | Label lab             -> (addr,(lab,addr) :: labenv)
    | FLabel (m,lab)        -> (addr,(lab,addr) :: labenv)
    | CSTI i                -> (addr + 2,labenv)
    | CSTC i                -> (addr + 2,labenv) // new
    | CSTF i                -> (addr + 2,labenv) // new
    | GVAR i                -> (addr + 2,labenv)
    | OFFSET i              -> (addr + 2,labenv)
    | ADD                   -> (addr + 1,labenv)
    | SUB                   -> (addr + 1,labenv)
    | MUL                   -> (addr + 1,labenv)
    | DIV                   -> (addr + 1,labenv)
    | MOD                   -> (addr + 1,labenv)
    | EQ                    -> (addr + 1,labenv)
    | LT                    -> (addr + 1,labenv)
    | NOT                   -> (addr + 1,labenv)
    | DUP                   -> (addr + 1,labenv)
    | SWAP                  -> (addr + 1,labenv)
    | LDI                   -> (addr + 1,labenv)
    | STI                   -> (addr + 1,labenv)
    | GETBP                 -> (addr + 1,labenv)
    | GETSP                 -> (addr + 1,labenv)
    | INCSP m               -> (addr + 2,labenv)
    | GOTO lab              -> (addr + 2,labenv)
    | IFZERO lab            -> (addr + 2,labenv)
    | IFNZRO lab            -> (addr + 2,labenv)
    | CALL(m,lab)           -> (addr + 3,labenv)
    | TCALL(m,n,lab)        -> (addr + 4,labenv)
    | RET m                 -> (addr + 2,labenv)
    | PRINTI                -> (addr + 1,labenv)
    | PRINTC                -> (addr + 1,labenv)
    | PRINTF                -> (addr + 1,labenv) // new
    | LDARGS m              -> (addr + 2,labenv)
    | STOP                  -> (addr + 1,labenv)

// 以整数形式输出字节码
// getlab 得到标签所在地址的函数
// let getlab lab = lookup labenv lab

let rec emitints getlab instr ints =
    match instr with
    | Label lab         -> ints
    | FLabel (m,lab)    -> ints
    | CSTI i            -> CODECSTI     :: i :: ints
    | CSTC i            -> CODECSTC     :: i :: ints // new
    | CSTF i            -> CODECSTF     :: i :: ints // new
    | GVAR i            -> CODECSTI     :: i :: ints
    | OFFSET i          -> CODECSTI     :: i :: ints
    | ADD               -> CODEADD      :: ints
    | SUB               -> CODESUB      :: ints
    | MUL               -> CODEMUL      :: ints
    | DIV               -> CODEDIV      :: ints
    | MOD               -> CODEMOD      :: ints
    | EQ                -> CODEEQ       :: ints
    | LT                -> CODELT       :: ints
    | NOT               -> CODENOT      :: ints
    | DUP               -> CODEDUP      :: ints
    | SWAP              -> CODESWAP     :: ints
    | LDI               -> CODELDI      :: ints
    | STI               -> CODESTI      :: ints
    | GETBP             -> CODEGETBP    :: ints
    | GETSP             -> CODEGETSP    :: ints
    | INCSP m           -> CODEINCSP    :: m :: ints
    | GOTO lab          -> CODEGOTO     :: getlab lab :: ints
    | IFZERO lab        -> CODEIFZERO   :: getlab lab :: ints
    | IFNZRO lab        -> CODEIFNZRO   :: getlab lab :: ints
    | CALL(m,lab)       -> CODECALL     :: m :: getlab lab :: ints
    | TCALL(m,n,lab)    -> CODETCALL    :: m :: n :: getlab lab :: ints
    | RET m             -> CODERET      :: m :: ints
    | PRINTI            -> CODEPRINTI   :: ints
    | PRINTC            -> CODEPRINTC   :: ints
    | PRINTF            -> CODEPRINTF   :: ints // new
    | LDARGS m          -> CODELDARGS   :: m :: ints
    | STOP              -> CODESTOP     :: ints

(*
    将指令列表转换为 int 列表(共2个步骤)：
    1. 构建标签环境
    2. 使用标签环境的输出指令
*)

// 完成汇编指令到机器指令的转换
let code2ints (code : instr list) : int list =
    let (_,labenv) = List.fold makelabenv (0,[]) code
    let getlab lab = lookup labenv lab
    List.foldBack (emitints getlab) code []

let ntolabel (n:int) : label =
    string(n)

// 反编译
let rec decomp ints : instr list = 
    match ints with
    | []                                        -> []
    | CODEADD    :: ints_rest                   -> ADD                     :: decomp ints_rest
    | CODESUB    :: ints_rest                   -> SUB                     :: decomp ints_rest
    | CODEMUL    :: ints_rest                   -> MUL                     :: decomp ints_rest
    | CODEDIV    :: ints_rest                   -> DIV                     :: decomp ints_rest
    | CODEMOD    :: ints_rest                   -> MOD                     :: decomp ints_rest
    | CODEEQ     :: ints_rest                   -> EQ                      :: decomp ints_rest
    | CODELT     :: ints_rest                   -> LT                      :: decomp ints_rest
    | CODENOT    :: ints_rest                   -> NOT                     :: decomp ints_rest
    | CODEDUP    :: ints_rest                   -> DUP                     :: decomp ints_rest
    | CODESWAP   :: ints_rest                   -> SWAP                    :: decomp ints_rest
    | CODELDI    :: ints_rest                   -> LDI                     :: decomp ints_rest
    | CODESTI    :: ints_rest                   -> STI                     :: decomp ints_rest
    | CODEGETBP  :: ints_rest                   -> GETBP                   :: decomp ints_rest
    | CODEGETSP  :: ints_rest                   -> GETSP                   :: decomp ints_rest
    | CODEINCSP  :: m :: ints_rest              -> INCSP m                 :: decomp ints_rest
    | CODEGOTO   :: lab :: ints_rest            -> GOTO (ntolabel lab)     :: decomp ints_rest
    | CODEIFZERO :: lab :: ints_rest            -> IFZERO (ntolabel lab)   :: decomp ints_rest
    | CODEIFNZRO :: lab :: ints_rest            -> IFNZRO (ntolabel lab)   :: decomp ints_rest
    | CODECALL   :: m :: lab :: ints_rest       -> CALL(m,ntolabel lab)    :: decomp ints_rest
    | CODETCALL  :: m :: n :: lab :: ints_rest  -> TCALL(m,n,ntolabel lab) :: decomp ints_rest
    | CODERET    :: m :: ints_rest              -> RET m                   :: decomp ints_rest
    | CODEPRINTI :: ints_rest                   -> PRINTI                  :: decomp ints_rest
    | CODEPRINTC :: ints_rest                   -> PRINTC                  :: decomp ints_rest
    | CODEPRINTF :: ints_rest                   -> PRINTF                  :: decomp ints_rest // new
    | CODELDARGS :: ints_rest                   -> LDARGS 0                :: decomp ints_rest
    | CODESTOP   :: ints_rest                   -> STOP                    :: decomp ints_rest
    | CODECSTI   :: i :: ints_rest              -> CSTI i                  :: decomp ints_rest
    | CODECSTC   :: i :: ints_rest              -> CSTC i                  :: decomp ints_rest // new
    | CODECSTF   :: i :: ints_rest              -> CSTF i                  :: decomp ints_rest // new
    | _                                         -> printf "%A" ints; failwith "unknow code"