module Main

import Idris.Syntax
import Idris.Parser
import Core.FC
import Core.Name

import System
import System.File
import System.Directory
import Data.String

import ErrOr
import AST
import Idris.Pretty

import Deriving.Show
%language ElabReflection

Show a => Show (Core.WithData.WithFC a) where
  show x = show x.val

getModuleName : Module -> String
getModuleName mod = let name = show mod.moduleNS in
  case strM name of
    StrNil => name
    StrCons x xs => strCons (toLower x) xs

resolveInput : String -> String
resolveInput f =
  if isInfixOf "/" f then f
  else "programs/" ++ f

main : IO ()
main = do
  args <- getArgs
  let rawName = case args of
                  (_ :: f :: _) => f
                  _ => "ParMatMul.idr"
  let fName = resolveInput rawName

  let srcLoc = PhysicalIdrSrc (mkModuleIdent Nothing "idris_to_erlang")

  Right rawSrc <- readFile fName
    | Left err => do
        putStrLn $ "Error reading file: " ++ show err
        exitFailure

  let Right (ws, st, mod) = runParser srcLoc Nothing rawSrc (prog srcLoc)
    | Left err => do
      putStrLn $ "Parse error: " ++ show err
      exitFailure

  putStrLn $ "Parsing successful"
  putStrLn $ "Module name: " ++ show mod.moduleNS
  putStrLn $ "Number of declarations: " ++ show (length mod.decls)

  let Just ir = genIR mod
    | Err (StdErr err) => do
                putStrLn $ "IR generation error: " ++ err
                exitFailure

  let actualModName = getModuleName mod
  let finalIR = case ir of
                      EM _ decls => EM actualModName decls

  let erlangCode = pMod finalIR

  _ <- createDir "generated"  -- ensure dir exists, ignore if already there
  let outFile = "generated/" ++ actualModName ++ ".erl"

  result <- writeFile outFile erlangCode

  case result of
      Left err => do
            putStrLn $ "Error writing output file: " ++ show err
            exitFailure
      Right () => putStrLn $ "Written: " ++ outFile

  putStrLn erlangCode

