module AST

import Idris.Syntax
import Idris.Parser

import Core.FC
import Core.Name

import System
import System.File

import ErrOr

import Deriving.Show
%language ElabReflection

%default covering

-------------------------------------------------------------------------------

mapM : Monad m => (f : a -> m b) -> List a -> m (List b)
mapM f [] = pure []
mapM f (x :: xs) = do
  xs' <- mapM f xs
  x' <- f x
  pure (x' :: xs')

traverseWithSt : Monad m
              => {st : Type}
              -> (f : st -> a -> m (b, st))
              -> st
              -> List a
              -> m (List b, st)
traverseWithSt f st [] = pure ([], st)
traverseWithSt f st (x :: xs) = do
  (x', st') <- f st x
  (xs', st'') <- traverseWithSt f st' xs
  pure (x' :: xs', st'')

-------------------------------------------------------------------------------
-- Erlang IR

public export
data EPat : Type where
  EPVar : String -> EPat
  EPCst : String -> EPat
  EPBracket : List EPat -> EPat
  EPInfixApp : List EPat -> String -> List EPat -> EPat 
  EPPair : List EPat -> List EPat -> EPat 
  EPUnit : EPat 
  EPList : List EPat -> EPat

public export
data EStmt : Type where
  ESVar   : String -> EStmt
  ESCst  : String -> EStmt
  ESApp   : String -> List EStmt -> EStmt
  ESApp2 : EStmt -> List EStmt -> EStmt
  ESInfixApp : EStmt -> String -> EStmt -> EStmt
  ESString : String -> EStmt
  ESMatchOp : List EPat -> EStmt -> EStmt
  ESMatchVar : String -> EStmt -> EStmt
  ESMacMod : EStmt
  ESList : List EStmt -> EStmt
  ESSelf : EStmt
  ESRecv : List (EPat, List EStmt) -> EStmt
  ESSend : EStmt -> EStmt -> EStmt
  EBracket : EStmt -> EStmt
  ESeq : EStmt -> EStmt -> EStmt
  ELam : List EPat -> EStmt -> EStmt
  EList : List EStmt -> EStmt
  EPair : List EStmt -> List EStmt -> EStmt
  EUnit : EStmt
  EComprehension : EStmt -> List EStmt -> EStmt
  ECase : EStmt -> List (List EPat, List EStmt) -> EStmt
  ELet : String -> EStmt -> EStmt -> EStmt
  EIf : EStmt -> EStmt -> EStmt -> EStmt

public export
data EDecl : Type where
  EDNil : EDecl
  EDFun : (dn : String) -> (cs : List (List EPat, List EStmt)) -> EDecl

public export
data EMod : Type where
  EM : (mn : String) -> List EDecl -> EMod


%hint
public export 
showEPat : Show EPat 
showEPat = %runElab derive

%hint
public export 
showEStmt : Show EStmt 
showEStmt = %runElab derive

%hint
public export 
showEDecl : Show EDecl 
showEDecl = %runElab derive

%hint
public export 
showEMod : Show EMod 
showEMod = %runElab derive

-------------------------------------------------------------------------------
-- Pretty Print IR

flatLam : List EPat -> EStmt -> (List EPat, EStmt)
flatLam pats (ELam p2 rhs) = flatLam (pats ++ p2) rhs
flatLam pats body = (pats, body)

strip : EStmt -> EStmt
strip (EBracket x) = strip x
strip x = x

mutual

  eF : List EStmt -> String -> String 
  eF [] s = ""
  eF (s1::s2::ss) s = s 
  eF (s1::[]) s = s

  eP : List EPat -> String -> String 
  eP [] s = ""
  eP (s1::s2::ss) s = s 
  eP (s1::[]) s = s

  public export 
  pFun : String -> String 
  pFun fun = 
    case fun of
      "filter" => "lists:filter"
      "sum" => "lists:sum"
      "map" => "lists:map"
      "length" => "length"
      "spawn" => "spawn"
      "Nat" => "nat"
      "MsgT" => "msgt"
      "fChan" => "utils:fst"
      "sChan" => "utils:snd"
      "chunk" => "utils:n_length_chunks"
      "chunk2" => "utils:unshuffle"
      "fst" => "utils:fst2"
      "snd" => "utils:snd2"
      "zip1" => "lists:zip"
      "foldl" => "lists:foldl"
      "hd" => "hd"
      "tl" => "tl"
      "||" => "or"
      "==" => "=="
      "+" => "+"
      "-" => "-"
      "*" => "*"
      "foldr" => "lists:foldr"
      "takeWhile" => "lists:takewhile"
      "minus" => "utils:minus"
      "not" => "not"
      "zipIt" => "lists:zip"
      "app" => "lists:append"
      "S" => "utils:s"
      "rem" => "rem"
      "procN" => "play2:processN"
      "divLem" => "utils:divLem"
      "<$$>" => "play2:sync_stream2"
      "foldStages" => "play2:foldStages"
      "connectStages" => "play2:comp"
      "Just" => "utils:just"
      "<##>" => "play2:app_stream2"
      "<$$$>" => "play2:sync_stream3"
      "<" => "<"
      ">" => ">"
      "<=" => "=<"
      ">=" => ">="
      "<$>" => "play2:sync_stream"
      "proc" => "play2:process"
      "zip" => "lists:zip"
      "replicate" => "lists:duplicate"
      "farm" => "play2:taskFarm"
      "concatMap" => "lists:flatmap"
      "&&" => "and"
      "`mod`" => "rem"
      "`div`" => "div"
      "/" => "/"
      "splitAt" => "lists:split"

      fn => "?MODULE:" ++ fn

  public export 
  pVar : String -> String 
  pVar fun = 
    case fun of
      "Nat" => "nat"
      "MsgT" => "msgt"
      "True" => "true"
      "False" => "false"
      "andB" => "fun(X,Y) -> X and Y end"
      "Nothing" => "nothing"
      "<$>" => "fun play2:sync_stream/1"
      "head" => "fun hd/1"
      "tail" => "fun tl/1"
      "+" => "fun erlang:'+'/2"
      "PNil" => "pnil"
      fn => fn

  public export 
  pRecvs : Bool -> String -> String -> List (EPat, List EStmt) -> String 
  pRecvs b t e [] = ""
  pRecvs b t e ((p,cs) :: rest) = pPats " , " [p] ++ " -> " 
                        ++ pStmts b t e cs
                        ++ pRecvs b t e rest

  public export 
  pStmts : Bool -> String -> String -> List EStmt -> String 
  pStmts b t e [] = "" 
  pStmts b t e (ESVar x :: ss) = t ++ (pVar x) ++ " " ++ (eF ss e) ++ pStmts b t e ss 
  pStmts b t e (ESCst c :: ss) = t ++ c ++ " " ++ (eF ss e) ++ pStmts b t e ss 
  pStmts b t e (ESApp2 s1 args::ss) = 
    t ++ (pStmts b t e [s1]) ++ "( " ++ pStmts b "" " , " args ++ " ) " ++ (eF ss e) ++ pStmts b t e ss
  pStmts b t e (ESApp fn args::ss) = 
    if fn == "Pure" || fn == "Return" then 
        t ++ pStmts b "" " , " args ++  (eF ss e) ++ pStmts b t e ss    
    else if fn == "<$$$>" then 
        t 
        ++ "play2:sync_stream3("
        ++ pStmts b "" " , " args
        ++ " "
        ++ ","
        ++ " "
        ++ "length(Input)"
        ++ ")"
        ++ (eF ss e)
        ++ pStmts b t e ss
    else if fn == "proc" || fn == "play2:process" then
        case args of
          [ESVar fName] =>
            t ++ "play2:process(fun " ++ fName ++ "/1)"
            ++ (eF ss e) ++ pStmts b t e ss
          _ =>
            t ++ "play2:process(" ++ pStmts b "" " , " args ++ ")"
            ++ (eF ss e) ++ pStmts b t e ss
    else if fn == "believe_me" then
        case args of
          [EUnit] => t ++ "[]" ++ (eF ss e) ++ pStmts b t e ss
          [x] => t ++ pStmts b "" e [x] ++ (eF ss e) ++ pStmts b t e ss
          _ => t ++ pStmts b "" " , " args ++ (eF ss e) ++ pStmts b t e ss
    else if fn == "farm" then
        case args of
          [f, nw, xs, _] =>
            t ++ "play2:taskFarm(" ++ pStmts b "" " , " [f, nw, xs] ++ ")"
            ++ (eF ss e) ++ pStmts b t e ss
          _ =>
            t ++ "play2:taskFarm(" ++ pStmts b "" " , " args ++ ")"
            ++ (eF ss e) ++ pStmts b t e ss
    else if fn == "cast" then -- identity in Erlang
        case args of
          [x] => t ++ pStmts b "" e [x] ++ (eF ss e) ++ pStmts b t e ss
          _ => t ++ pStmts b "" " , " args ++ (eF ss e) ++ pStmts b t e ss
    else if fn == "snd" then -- snd ((<$>) x) -> sync_stream(x)
        case args of
          [ESApp "<$>" innerArgs] =>
            t ++ "play2:sync_stream(" ++ pStmts b "" " , " innerArgs ++ ")"
            ++ (eF ss e) ++ pStmts b t e ss
          [EBracket (ESApp "<$>" innerArgs)] =>
            t ++ "play2:sync_stream(" ++ pStmts b "" " , " innerArgs ++ ")"
            ++ (eF ss e) ++ pStmts b t e ss
          _ =>
            t ++ (pFun "snd") ++ "( " ++ pStmts b "" " , " args ++ " )"
            ++ (eF ss e) ++ pStmts b t e ss
    else if fn == "map" then -- wrap bare fn name in fun ?MODULE:F/1
        case args of
          (ESVar fname :: rest) =>
            let v = pVar fname
                fref = case unpack v of
                  ('f' :: 'u' :: 'n' :: ' ' :: _) => v
                  cs => if any (== ':') cs then v else "fun ?MODULE:" ++ v ++ "/1"
            in t ++ "lists:map(" ++ fref ++ ", " ++ pStmts b "" "," rest ++ ")"
               ++ (eF ss e) ++ pStmts b t e ss
          _ => t ++ "lists:map(" ++ pStmts b "" " , " args ++ ")"
               ++ (eF ss e) ++ pStmts b t e ss
    else if fn == "take" then -- take N (iterate F Start) → lists:seq(Start, Start+N-1)
        case map strip args of
          [n, ESApp "iterate" [_, start]] =>
            t ++ "lists:seq(" ++ pStmts b "" "" [start] ++ ", "
            ++ pStmts b "" "" [start] ++ " + " ++ pStmts b "" "" [n] ++ " - 1)"
            ++ (eF ss e) ++ pStmts b t e ss
          [n, list] =>
            t ++ "lists:sublist(" ++ pStmts b "" "" [list] ++ ", " ++ pStmts b "" "" [n] ++ ")"
            ++ (eF ss e) ++ pStmts b t e ss
          _ =>
            t ++ "lists:sublist(" ++ pStmts b "" " , " args ++ ")"
            ++ (eF ss e) ++ pStmts b t e ss
    else
        t ++ (pFun fn) ++ "( " ++ pStmts b "" " , " args ++ " ) " ++  (eF ss e) ++ pStmts b t e ss
  pStmts b t e (ESInfixApp t1 str t2 :: ss) = 
    if str == "::" then 
      t 
      ++ "["
      ++ pStmts b "" e [t1]
      ++ " "
      ++ "|" 
      ++ " "
      ++ pStmts b "" e [t2]
      ++ "]"
      ++ (eF ss e)
      ++ pStmts b t e ss
       else if str == "++" then 
      t 
      ++ "lists:append("
      ++ pStmts b "" e [t1]
      ++ " "
      ++ "," 
      ++ " "
      ++ pStmts b "" e [t2]
      ++ ")"
      ++ (eF ss e)
      ++ pStmts b t e ss
       else if str == "<###>" then 
        t 
        ++ "play2:distributeL("
        ++ pStmts b "" e [t1]
        ++ " "
        ++ ","
        ++ " "
        ++ pStmts b "" e [t2]
        ++ ")"
        ++ (eF ss e)
        ++ pStmts b t e ss
       else if str == "<##>" then 
        t 
        ++ "play2:app_stream2("
        ++ pStmts b "" e [t1]
        ++ " "
        ++ ","
        ++ " "
        ++ pStmts b "" e [t2]
        ++ ")"
        ++ (eF ss e)
        ++ pStmts b t e ss
       else if str == ">>" then
        t
        ++ "play2:comp("
        ++ pStmts b "" e [t1]
        ++ " "
        ++ ","
        ++ " "
        ++ pStmts b "" e [t2]
        ++ ")"
        ++ (eF ss e)
        ++ pStmts b t e ss
       else if str == "<#>" then
        t
        ++ "play2:app_stream("
        ++ pStmts b "" e [t1]
        ++ ","
        ++ pStmts b "" e [t2]
        ++ ")"
        ++ (eF ss e)
        ++ pStmts b t e ss
       else if str == "<$>" then
        t
        ++ "play2:sync_stream("
        ++ pStmts b "" e [t1]
        ++ ")"
        ++ (eF ss e)
        ++ pStmts b t e ss
       else if str == "." then -- snd . (<$>) -> fun play2:sync_stream/1
        let inner2 = case t2 of
                       EBracket x => x
                       x => x
        in case (t1, inner2) of
             (ESVar "snd", ESVar "<$>") =>
               t ++ "fun play2:sync_stream/1"
               ++ (eF ss e) ++ pStmts b t e ss
             _ =>
               t
               ++ pStmts b "" e [t1]
               ++ " ?MODULE:. "
               ++ pStmts b "" e [t2]
               ++ (eF ss e)
               ++ pStmts b t e ss
       else
         t
      ++ pStmts b "" e [t1]
      ++ " "
      ++ (pFun str)
      ++ " "
      ++ pStmts b "" e [t2]
      ++ (eF ss e)
      ++ pStmts b t e ss
  pStmts b t e (ESString s::ss)  = t ++ "\"" ++ s ++ "\"" ++ (eF ss e) ++ pStmts b t e ss
  pStmts True t e (ESMatchOp pats st :: ss) = t 
                                          ++ pPats " , " pats
                                          ++ " <- "
                                          ++ pStmts True t e [st] 
                                          ++ (eF ss e) ++ pStmts True t e ss
  pStmts False t e (ESMatchOp pats st :: ss) = t 
                                          ++ pPats " , " pats
                                          ++ " = "
                                          ++ pStmts False t e [st] 
                                          ++ (eF ss e) ++ pStmts False t e ss
  pStmts True t e (ESMatchVar n rhs :: ss ) = t 
                                          ++ n
                                          ++ " <- "
                                          ++ pStmts True t e [rhs] 
                                          ++ (eF ss e) ++ pStmts True t e ss
  pStmts b t e (ESMatchVar n rhs :: ss ) = t 
                                          ++ n
                                          ++ " = "
                                          ++ pStmts b t e [rhs] 
                                          ++ (eF ss e) ++ pStmts b t e ss

  pStmts b t e (ESMacMod ::ss) = t ++ "?MODULE" ++ (eF ss e) ++ pStmts b t e ss 
  pStmts b t e (ESList sts :: ss) = t ++  "[ "
                               ++ pStmts b t " , " sts 
                               ++ " ] "
                               ++ (eF ss e) ++ pStmts b t e ss 
  pStmts b t e (ESSelf  :: ss) = t ++ "self() " ++ (eF ss e) ++ pStmts b t e ss 
  pStmts b t e (ESRecv cs :: ss) = t ++ "receive" ++ "\n\t\t"
                                 ++ pRecvs b (t++"\t") e cs
                             ++ (eF ss e) 
                             ++ "\n"
                             ++ t 
                             ++ "end"
                             ++ pStmts b t e ss 
  pStmts b t e (ESSend m s :: ss) = t ++ pStmts b "" "" [m] ++ " ! " ++ pStmts b "" "" [s] ++ (eF ss e) ++ pStmts b t e ss 
  pStmts b t e (EBracket ter :: ss) = t ++ " ( "
                                 ++ (pStmts b t e [ter])
                                 ++ " ) "
                                 ++ (eF ss e)
                                 ++ pStmts b t e ss 
  pStmts b t e (ESeq t1 t2 :: ss) =
           t ++ "lists:seq( "
        ++ (pStmts b t e [t1])
        ++ " , "
        ++ (pStmts b t e [t2])
        ++ " ) "
        ++ (eF ss e)
        ++ pStmts b t e ss  
  pStmts b t e (ELam p rhs :: ss) =
    let (allPats, body) = flatLam p rhs -- flatten curried lambdas
    in   t ++ "fun ( "
      ++ pPats " , " allPats
      ++ " ) -> "
      ++ pStmts b t e [body]
      ++ " end "
      ++ (eF ss e)
      ++ pStmts b t e ss 
  pStmts b t e (EList ts :: ss) = 
            t ++ "["
         ++ pStmts b "" "," ts
         ++ "]"
         ++ (eF ss e) 
         ++ pStmts b t e ss 
  pStmts b t e (EPair t1 t2 :: ss) = 
         t ++ "{"
         ++ pStmts b "" e t1
         ++ ","
         ++ pStmts b "" e t2
         ++ "}"
         ++ (eF ss e) 
         ++ pStmts b t e ss 
  pStmts b t e (EUnit :: ss) =
    "{}"
    ++ (eF ss e)
    ++ pStmts b t e ss
  pStmts b t e (EComprehension term gens :: ss) = 
    "["
    ++ pStmts b "" "" [term]
    ++ " || "
    ++ pStmts True "" "," gens
    ++ "]"
    ++ (eF ss e)
    ++ pStmts b t e ss 
  pStmts b t e (ECase term clauses :: ss) =
    case term of
      ESApp "decEq" [a, bVal] => -- decEq(a,b) → case a of b -> yes; _ -> no end
        case clauses of
          ((_, yesBody) :: (_, noBody) :: _) =>
            t ++ "case " ++ pStmts b "" "" [a] ++ " of\n"
            ++ "\t" ++ pStmts b "" "" [bVal] ++ " -> " ++ pStmts False "" "," yesBody
            ++ ";\n\t_ -> " ++ pStmts False "" "," noBody
            ++ "\n" ++ t ++ "end"
            ++ (eF ss e) ++ pStmts b t e ss
          _ =>
            t ++ "case " ++ pStmts b "" "" [term] ++ " of\n"
            ++ pClauses "\t" False clauses ++ "\n" ++ t ++ "end"
            ++ (eF ss e) ++ pStmts b t e ss
      _ =>
        t ++ "case " ++ pStmts b "" "" [term] ++ " of\n"
        ++ pClauses "\t" False clauses ++ "\n" ++ t ++ "end"
        ++ (eF ss e) ++ pStmts b t e ss
  pStmts b t e (ELet name val body :: ss) =
    t ++ name
    ++ " = "
    ++ pStmts b "" "" [val]
    ++ ",\n"
    ++ pStmts b t e [body]
    ++ (eF ss e)
    ++ pStmts b t e ss
  pStmts b t e (EIf cond thenB elseB :: ss) =
    t ++ "if "
    ++ pStmts b "" "" [cond]
    ++ " ->\n"
    ++ pStmts b (t ++ "\t") "" [thenB]
    ++ ";\n"
    ++ t
    ++ "true ->\n"
    ++ pStmts b (t ++ "\t") "" [elseB]
    ++ "\n"
    ++ t
    ++ "end"
    ++ (eF ss e)
    ++ pStmts b t e ss

  public export
  pPats : String -> List EPat -> String
  pPats e [] = ""
  pPats e (EPVar n :: pats) = n ++ (eP pats e) ++ pPats e pats
  pPats e (EPCst c :: pats) = c ++ (eP pats e) ++ pPats e pats 
  pPats e (EPBracket p :: pats) = "(" ++ (pPats e p) ++ ")" ++ (eP pats e) ++ pPats e pats
  pPats e (EPInfixApp p1 str p2 :: pats) = 
     if str == "::" then 
      "["
      ++ (pPats e p1)
      ++ "|"
      ++ (pPats e p2)
      ++ "]"
      ++ (eP pats e)
      ++ pPats e pats
     else 
      (pPats e p1)
      ++ str
      ++ (pPats e p2)
  pPats e (EPPair t1 t2 :: pats) = 
    "{"
    ++ (pPats e t1)
    ++ ","
    ++ (pPats e t2)
    ++ "}"
    ++ (eP pats e)
    ++ pPats e pats
  pPats e (EPUnit :: pats) =
    "{}"
    ++ (eP pats e)
    ++ pPats e pats
  pPats e (EPList [] :: pats) = 
    "[]"
    ++ (eP pats e)
    ++ pPats e pats
  pPats e (EPList xs :: pats) =
    "["
    ++ pPats "," xs
    ++ "]"
    ++ (eP pats e)
    ++ pPats e pats

  pClauses : String -> Bool -> List (List EPat, List EStmt) -> String
  pClauses n d [] = ""
  pClauses n d ((pats, stmts)::[]) =  n ++ "("
                                 ++ pPats "," pats
                                 ++ ") -> "
                                 ++ pStmts False "" "," stmts
                                 ++ (if d then "." else "")

  pClauses n d ((pats, stmts)::cs) =  n ++ "("
                                 ++ pPats "," pats
                                 ++ ") -> "
                                 ++ pStmts False "" "," stmts
                                 ++ ";\n"
                                 ++ pClauses n d cs

  public export
  pDecs : List EDecl -> String 
  pDecs [] = ""
  pDecs (EDNil :: decs) = pDecs decs 
  pDecs (EDFun n cs :: decs) =  pClauses n True cs ++ "\n\n" ++ pDecs decs

  public export
  pMod : EMod -> String 
  pMod (EM name decs) =  "-module(" ++ name ++ ")." ++ "\n"
                      ++ "-compile(export_all).\n\n"
                      ++ pDecs decs

-------------------------------------------------------------------------------
-- IR Generation -- Names

Env : Type
Env = List (String, String)

prelude : String -> EStmt
prelude "printLn"  = ESVar "io:format" -- N.B. newlines not preserved
prelude "putStrLn" = ESVar "io:format"
prelude "putStr"   = ESVar "io:format"
prelude "print"    = ESVar "io:format"
prelude "Halt"     = ESVar "halt"
prelude "MEnd"     = ESVar "mend"
prelude "Msg"      = ESVar "msg"
prelude "MkStageNil" = ESVar "mkstagesnil"
prelude "MkStages" = ESVar "mkstages"
prelude str        = ESVar str

toEVarName : String -> String
toEVarName str =
  case strM str of
    StrNil => assert_total (idris_crash "toEVarName: impossible empty string")
    StrCons x xs => strCons (toUpper x) xs


lookup : Env -> String -> EStmt
lookup [] str = prelude str
lookup ((x,y) :: xs) str =
  if str == x then ESVar (toEVarName y) else lookup xs str

getNameStrFrmName : Env -> Name.Name -> ErrorOr String
getNameStrFrmName env n =
  case displayName n of
      (Nothing, n') => case lookup env n' of
        ESVar n'' => Just n''
        n'' => error
             $ "getNameStrFrmName: var lookup unexpected expand -- " ++ show n''
      _ => error "getNameStrFrmName -- non-empty namespace"

getNameStmtFrmName : Env -> Name.Name -> ErrorOr EStmt
getNameStmtFrmName env n =
  case displayName n of
      (Nothing, n') => Just (lookup env n')
      _ => error "getNameStmtFrmName -- non-empty namespace"

getNameFrmTerm : Env -> PTerm -> ErrorOr EStmt
getNameFrmTerm env (PRef fc' n) =
  getNameStmtFrmName env n
getNameFrmTerm env (PApp fc' f x) =
  getNameFrmTerm env f
getNameFrmTerm env (PNamedApp fc' f n x) =
  getNameFrmTerm env f
getNameFrmTerm env tm =
  error $ "getNameFrmTerm -- unimplemented -- " ++ show tm

getNameFrmClause : Env -> PClause -> ErrorOr String
getNameFrmClause env (MkPatClause fc lhs _ _) = do
  ESVar n <- getNameFrmTerm env lhs
    | n =>
      error $ "getNameFrmClause: name causes unexpected expansion -- " ++ show n
  pure n
getNameFrmClause env (MkWithClause fc lhs _ _ _) = do
  ESVar n <- getNameFrmTerm env lhs
    | n =>
      error $ "getNameFrmClause: name causes unexpected expansion -- " ++ show n
  pure n
getNameFrmClause env (MkImpossible fc lhs) =
  error "getNameFrmClause -- MkImpossible"

-------------------------------------------------------------------------------
-- IR Generation -- Patterns

genEPats : Env -> PTerm -> ErrorOr (List EPat, Env)
genEPats env (PRef fc n) = do
  "Z" <- getNameStrFrmName env n
    | n' => case n' of 
              "MEnd" => pure ([EPVar "mend"], ("MEnd","mend") :: env)
              n'' => pure ([EPVar (toEVarName n'')], (n'',n'') :: env)
  pure ([EPVar "0"], ("Z","0") :: env)
genEPats env (PApp fc (PRef _ nm) (PRef _ nm2)) = do
  "S" <- getNameStrFrmName env nm
    | n => case n of 
              "mkstagesnil" => error "here"   
              n' => do
                  -- nm1 <- getNameStrFrmName env nm
                    "Z" <- getNameStrFrmName env nm2 
                      | nm2 => case nm2 of 
                                "mkstagesnil" => error (">" ++ show n)
                                nm2' => pure ([EPVar ((toEVarName nm2))], (nm2,nm2) :: env)
                    pure ([EPVar "0"], ("Z","0") :: env)
  nm2' <- getNameStrFrmName env nm2
  pure ([EPVar (toEVarName nm2')], (nm2',(nm2'++"-1")) :: env)
genEPats env (PApp fc (PRef _ nm) x) =  do
   "MkStages" <- getNameStrFrmName env nm 
      | n => genEPats env x 
   error (show x)
genEPats env (PApp fc f x) = do
  (xs, env')  <- genEPats env f
  (ys, env'') <- genEPats env' x
  pure (xs ++ ys, env'')
genEPats env (PPair fc l r) = do
  (xs, env') <- genEPats env l
  (ys, env'') <- genEPats env' r
  pure ([EPPair xs ys], env'')
genEPats env (PPrimVal fc c) = do
    pure ([EPCst (show c)], env)
genEPats env (PBracketed fc t) = do 
  (t', env') <- genEPats env t
  pure ([EPBracket t'], env')
genEPats env (POp fc (MkWithData _ t1) opn t2) = do
    (t1', env') <- genEPats env t1.getLhs
    (t2', env'') <- genEPats env' t2 
    pure ([EPInfixApp t1' (show opn.val) t2'], env'')
genEPats env (PDPair fu op t1 t2 t3) = do
      (t1, env') <- genEPats env t1 
      (t2, env'') <- genEPats env' t3 
      pure ([EPPair t1 t2], env'')
genEPats env (PUnit _) =
  pure ([EPUnit], env)
genEPats env (PImplicit _) =
  pure ([EPVar "_"], env)
genEPats env (PNamedApp fc f n x) =
  genEPats env f
genEPats env (PList _ _ []) =
  pure ([EPList []], env)
genEPats env (PList _ _ items) = do
  (pats, env') <- genEPatsList env items
  pure ([EPList pats], env')
  where
    genEPatsList : Env -> List (Core.FC.FC, PTerm) -> ErrorOr (List EPat, Env)
    genEPatsList env [] = pure ([], env)
    genEPatsList env ((_, t) :: rest) = do
      (pats1, env') <- genEPats env t
      (pats2, env'') <- genEPatsList env' rest
      pure (pats1 ++ pats2, env'')

genEPats env p = error $ "genEPats: unimplemented -- " ++ show p

genEPatsTop : Env -> PTerm -> ErrorOr (List EPat, Env)
genEPatsTop env (PRef fc n) = do
  "mend" <- getNameStrFrmName env n
    | n' => case n' of
              "True"  => pure ([EPCst "true"],  env)
              "False" => pure ([EPCst "false"], env)
              _       => pure ([], env)
  pure ([EPVar "mend"], ("mend","mend") :: env) -- no arguments
genEPatsTop env (PApp fc (PRef _ nm) (PRef _ nm2)) = do 
  "msg" <- getNameStrFrmName env nm 
    | n => do 
                            nm2' <- getNameStrFrmName env nm2

                            pure ([EPVar (toEVarName nm2')], (nm2', nm2') :: env)
           
  nm2 <- getNameStrFrmName env nm2
  pure ([EPPair [EPVar "msg"] [EPVar (toEVarName nm2)] ], (nm2,nm2) :: env)
genEPatsTop env p = genEPats env p

-------------------------------------------------------------------------------
-- IR Generation -- Terms/Expressions/Statements

mutual
  genEStmtsDo : Env
             -> List PDo
             -> ErrorOr (List EStmt, Env)
  genEStmtsDo env [] = pure ([], env)
  genEStmtsDo env ((DoExp fc tm) :: dss) = do
    (es, env') <- genSend env tm
    (rest, env'') <- genEStmtsDo env' dss
    pure (es ++ rest, env'')
  genEStmtsDo env ((DoBindPat fc t ty u cls) :: dss) = do
    ([x,y], env') <- genEPats env t
      | (xs, env') => do
        (e, env'') <- genEStmt env' u
        (rest, env''') <- genEStmtsDo env'' dss
        pure (ESMatchOp xs e :: rest, env''')
    (e@(ESApp "spawn" _), env'') <- genSpawn env' u -- SPAWN
      | (e, env'') => do
        (rest, env''') <- genEStmtsDo env'' dss
        pure (ESMatchOp [x,y] e :: rest, env''')
    (rest, env''') <- genEStmtsDo env'' dss
    pure (ESMatchOp [x] e :: rest, env''')
  genEStmtsDo env (DoBind fc nfc x rig mty rhs@(PApp _ (PRef _ fn) (PRef _ ch)) :: dss) = do
    x' <- getNameStrFrmName env x
    case displayName fn of
      (Nothing, "Recv") => do
        (rest, env') <- genEStmtsDo ((x',x') :: env) dss
        pure ([ESRecv [(EPVar (toEVarName x'), rest)]], env')
      _ => do
        (rhs', env') <- genEStmt ((x',x') :: env) rhs
        (rest, env'') <- genEStmtsDo env' dss
        pure (rhs' :: rest, env'')    
  genEStmtsDo env (DoBind fc nfc x rig mty rhs@(PApp _ (PApp _ (PRef _ fn) (PRef x2 ty)) ch) :: dss) = do
    x' <- getNameStrFrmName env x
    case displayName fn of
      (Nothing, "Recv") => do
        (rest, env') <- genEStmtsDo ((x',x') :: env) dss
        pure ([ESRecv [(EPVar (toEVarName x'), rest)]], env')
      _ => do
        (rhs', env') <- genEStmt env rhs 
        (rest, env'') <- genEStmtsDo ((x',x') :: env') dss
        pure (ESMatchVar (toEVarName x') rhs' :: rest, env'') 
  genEStmtsDo env (DoBind fc nfc x rig mty rhs :: dss) = do
    x' <- getNameStrFrmName env x
    (e@(ESApp "spawn" _), env') <- genSpawn env rhs -- SPAWN
       | (rhs', env') => do -- <- genEStmt ((x',x')::env) rhs
             (rest, env'') <- genEStmtsDo ((x',x') :: env') dss
             pure (ESMatchVar (toEVarName x') rhs' :: rest, env'')
    (rest, env'') <- genEStmtsDo ((x',x') :: env') dss
    pure (ESMatchVar (toEVarName x') e :: rest, env'')
  genEStmtsDo env (DoLet _ _ _ _ _ _ :: dss) =
    error $ "genEStmtsDo: unimplemented -- DoLet"
  genEStmtsDo env (DoLetPat _ _ _ _ _ :: dss) =
    error $ "genEStmtsDo: unimplemented -- DoLetPat"
  genEStmtsDo env (DoLetLocal _ _ :: dss) =
    error $ "genEStmtsDo: unimplemented -- DoLetLocal"
  genEStmtsDo env (DoRewrite _ _ :: ds) =
    error $ "genEStmtsDo: unimplemented -- DoRewrite"

  genEStmtsStr : PStr -> ErrorOr String
  genEStmtsStr (StrLiteral fc str) = Just str
  genEStmtsStr (StrInterp fc tm) =
    error $ "genEStmtsStr: unimplemented -- StrInterp"

  genEStmts : Env -> PTerm -> ErrorOr (List EStmt, Env)
  genEStmts env (PRef fc n) = do
    stmt <- getNameStmtFrmName env n
    pure ([stmt], env)
  genEStmts env (PApp fc f x) = do
    (ESApp fn xs, env')  <- genEStmt env f
      | (ESVar fn, env') => do
        (x', env'') <- genEStmt env' x
        case fn of 
          "msg" => do pure ([EPair [ESVar "msg"] [x']], env')
          _ => do pure ([ESApp fn [x']], env'')
      | (f', env') => do 
                --  (f'', env'') <- genEStmt env f
                  (x', env'') <- genEStmt env' x
                  pure ([ESApp2 f' [x']], env'')
    (x', env') <- genEStmt env x
    pure ([ESApp fn (xs ++ [x'])], env')
  genEStmts env (PString fc ht strs) = do
    strs' <- mapM genEStmtsStr strs
    pure ([ESString (concat strs')], env)
  genEStmts env (PDoBlock fc mns dss) =
    genEStmtsDo env dss
  genEStmts env (PPrimVal fc c) = do
    pure ([ESCst (show c)], env)

  genEStmts env (PPair fc t1 t2) = do
    (t1, env') <- genEStmts env t1 
    (t2, env'') <- genEStmts env' t2
    pure ([EPair t1 t2], env'')

  genEStmts env (PBracketed fc t1) = do 
    (t1', env') <- genEStmt env t1 
    pure ([EBracket t1'], env')
  genEStmts env (PNamedApp fc p1 name p2) = error $ "genEStmts: PNamedApp unimplemented -- "  ++ show name
  genEStmts env (POp fc (MkWithData _ t1) opn t2) = do
    (t1', env') <- genEStmt env t1.getLhs
    (t2', env'') <- genEStmt env' t2 
    pure ([ESInfixApp t1' (show opn.val) t2'], env'')
  genEStmts env (PRange fc t1 mt2 t3) = do
     (t1', env') <- genEStmt env t1 
     (t3', env'') <- genEStmt env' t3
     pure ([ESeq t1' t3'], env'') 
  genEStmts env (PLam fc r pinf pat args scope) = do
     (pat', env') <- genEPats env pat 
     (rhs, env'') <- genEStmt env' scope 
     pure ([ELam pat' rhs], env'') 
  genEStmts env (PList fu ni li) = do 
    ts <- mapM (\x => genEStmt env x) (map snd li)
    pure ([EList (map fst ts)], env)
  genEStmts env (PDPair fu op t1 t2 t3) = do
      (t1, env') <- genEStmts env t1 
      (t2, env'') <- genEStmts env' t3 
      pure ([EPair t1 t2], env'')
  genEStmts env (PUnit _) =
    pure ([EUnit], env)
  genEStmts env (PComprehension fc t gens) = do
      (gens', env') <- genEStmtsDo env gens 
      (t', env'') <- genEStmt env' t 
      pure ([EComprehension t' gens'], env'')
  genEStmts env (PCase fc opts t cls) = do
      (t1, env') <- genEStmt env t 
      cls <- mapM (genEClause env') cls
      pure ([ECase t1 (trim cls)], env')
  genEStmts env (PIfThenElse fc cond t f) = do
      (cond', env') <- genEStmt env cond
      (t', env'') <- genEStmt env' t
      (f', env''') <- genEStmt env'' f
      pure ([EIf cond' t' f'], env''')
  genEStmts env (PLet fc rig pat ty val scope alts) = do
      (pats, env') <- genEPats env pat
      (val', env'') <- genEStmt env' val
      (scope', env''') <- genEStmt env'' scope
      case pats of
        [EPVar n] => pure ([ELet n val' scope'], env''')
        _ => pure ([ELet (pPats "," pats) val' scope'], env''')

  genEStmts env (PImplicit _) = pure ([ESVar "_"], env)
  genEStmts env (PSectionL fc opn rhs) = do -- (+1) → fun(X) -> X + 1 end
    (rhs', env') <- genEStmt env rhs
    pure ([ELam [EPVar "X"] (ESInfixApp (ESVar "X") (show opn.val) rhs')], env')
  genEStmts env (PSectionR fc lhs opn) = do -- (1+) → fun(X) -> 1 + X end
    (lhs', env') <- genEStmt env lhs
    pure ([ELam [EPVar "X"] (ESInfixApp lhs' (show opn.val) (ESVar "X"))], env')
  genEStmts env tm = error $ "genEStmts: unimplemented -- "  ++ show tm

  genEStmt : Env -> PTerm -> ErrorOr (EStmt, Env)
  genEStmt env tm = do
    ((stmt :: _), env') <- genEStmts env tm
      | ([], _) => error $ "genEStmt: no statements generated"
    pure (stmt, env')

  genSpawn : Env -> PTerm -> ErrorOr (EStmt, Env)
  genSpawn env tm@(PApp _ (PApp _ (PApp _ (PRef _ fn) (PRef _ ty1)) (PRef _ ty2)) (PRef _ p)) =
    case displayName fn of
      (Nothing, "Spawn") => do
        pStr <- getNameStmtFrmName env p
        Just (ESApp "spawn" [ESMacMod, pStr, ESList [ESVar "chan", ESSelf]], env)
      (Nothing, "rem") => error $ "rem!!"
      _ => genEStmt env tm
  genSpawn env tm = genEStmt env tm

  genSend : Env -> PTerm -> ErrorOr (List EStmt, Env)
  genSend env tm@(PApp _ (PApp _ (PRef _ fn) t) msg) = do
    "Send" <- getNameStrFrmName env fn
      | fn' => genEStmts env tm
    (ch', env') <- genEStmt env t
    (msg', env'') <- genEStmt env' msg
    pure ([ESSend ch' msg'], env'')
  genSend env tm = genEStmts env tm


-------------------------------------------------------------------------------
-- IR Generation -- Clauses/Statements
  genEClause : Env -> PClause -> ErrorOr (List EPat, List EStmt, Env)
  genEClause env (MkPatClause fc lhs rhs _) = do
    (pats, env') <- genEPatsTop env lhs
    (stmts, env'') <- genEStmts env' rhs
    Just (pats, stmts, env'')
  genEClause env (MkWithClause fc lhs _ flags clauses) = do
    (pats, env') <- genEPatsTop env lhs
    results <- traverse (genEClause env') clauses
    let nestedStmts = map (\(_, s, _) => s) results
    let allStmts = concat nestedStmts
    Just (pats, allStmts, env')
  genEClause e c = error $ "genEClause: unimplemented -- " ++ show c

-------------------------------------------------------------------------------
-- IR Generation -- Declarations
  trim :  List (List EPat, (List EStmt, Env)) ->  List (List EPat, List EStmt)
  trim [] = []
  trim ((pats, (stmts, env)) :: rest) = (pats, stmts) :: trim rest

public export
genEDecl : PDecl -> ErrorOr EDecl
genEDecl (MkWithData fc decl) = case decl of
  PDef cs@(c :: _) => do
    dn <- getNameFrmClause [] c
    body <- mapM (genEClause []) cs
    pure (EDFun dn (trim body))
  PClaim _ =>
    pure EDNil 
  PData _ _ _ _ =>
    pure EDNil
  PDirective _ =>
    pure EDNil
  d => error $ "genEDecl: unimplemented -- " ++ show d
-------------------------------------------------------------------------------
-- IR Generation -- Modules
public export
genIR : Module -> ErrorOr EMod
genIR m = do
  ds <- mapM genEDecl m.decls
  Just (EM "example" ds)
