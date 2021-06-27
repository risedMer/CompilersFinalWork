module XMAbsyn

type typ =                          // 基本类型 Type
    | TypInt                        (* Type int                     *)
    | TypChar                       (* Type Char                    *)
    | TypString                     (* Type String      new         *)
    | TypDouble                     (* Type Double      new         *)
    | TypArray of typ * int option  (* Type Array                   *)
    | TypPoint of typ               (* Type Pointer                 *)

and expr =                          // 表达式右值
    | Access of access              (* x    or  *p      or  a[e]    *)
    | Assign of access * expr       (* x=e  or  *p=e    or  a[e]=e  *)
    | Address of access             (* &x   or  &*p     or  &a[e]   *)
    | CstInt of int                 (* Constant int                 *)
    | CstChar of char               (* Constant char    new         *)
    | CstDouble of double           (* Constant double  new         *)
    | CstString of string           (* Constant string  new         *)
    | Prim1 of string * expr        (* Unary primitive operator     *)
    | Prim2 of string * expr * expr (* Binary primitive operator    *)
    | Andalso of expr * expr        (* Sequential and               *)
    | Orelse of expr * expr         (* Sequential or                *)
    | Call of string * expr list    (* Function call f(...)         *)

and access =                        // 左值存储的位置
    | AccVar of string              (* Variable access        x     *)
    | AccDeref of expr              (* 引用指针指向变量的值     *p    *)
    | AccIndex of access * expr     (* Array indexing         a[e]  *)

and stmt =                          // 表达式功能
    | If of expr * stmt * stmt      (* 条件语句                      *)
    | While of expr * stmt          (* While循环                     *)
    | Expr of expr                  (* 表达式语句               e;   *)
    | Return of expr option         (* 从函数中返回                  *)
    | Block of stmtordec list       (* 语句块                        *)

and stmtordec =                     // 表达式或声明
    | Dec of typ * string           (* 本地变量声明                   *)
    | Stmt of stmt                  (* 表达式                         *)

and topdec =                                                        // 顶级声明
    | Fundec of typ option * string * (typ * string) list * stmt    (* 函数表达式            *)
    | Vardec of typ * string                                        (* 变量声明              *)

and program =                       // 程序：顶级声明的列表
    | Prog of topdec list