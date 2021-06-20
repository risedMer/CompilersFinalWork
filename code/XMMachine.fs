module XMMachine

type label = string
// 汇编指令
type instr =
    | Label of label
    | FLabel of int * label
    | CSTI of int
    | CSTF of int                   (* Constant Float   new             *)
    | CSTC of int                   (* Constant Char    new             *)
    | OFFSET of int
    | GVAR of int                   (* global variable                  *)
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
    | SETSP
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