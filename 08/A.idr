module Main

import Data.String

data Instruction = Nop | Jmp | Acc

Show Instruction where
  show Nop = "nop"
  show Jmp = "jmp"
  show Acc = "acc"

data Command = Do Integer Instruction Integer

Show Command where
  show (Do line inst arg) = (show line) ++ ": " ++ (show inst) ++ " " ++ (show arg)

data HaltReason = IpOutOfRange | RepeatVisit

Show HaltReason where
  show IpOutOfRange = "ip out of range"
  show RepeatVisit = "line visited repeatedly"

data MachineState = Run Integer Integer (List Integer) | Halt Integer Integer (List Integer) HaltReason

Show MachineState where
  show (Run ip acc visited) = 
    "ip: " ++ (show ip) 
    ++ ", acc: " ++ (show acc) 
    ++ ", visited: " ++ (show visited)
  show (Halt ip acc visited reason) = 
    "HALT -- " ++ (show reason) 
    ++ " -- ip: " ++ (show ip) 
    ++ ", acc: " ++ (show acc) 
    ++ ", visited: " ++ (show visited)

parseCommand : (Integer, String) -> Command
parseCommand (line, cmd) =
  case words cmd of
    (inst :: arg :: _) => 
      case parseInteger arg of
        Nothing => Do line Nop 0
        Just arg' => case inst of
                       "nop" => Do line Nop arg'
                       "acc" => Do line Acc arg'
                       "jmp" => Do line Jmp arg'
                       _ => Do line Nop 0
    _ => Do line Nop 0

executeCommand : MachineState -> Command -> MachineState
executeCommand state@(Halt _ _ _ _) _ = state
executeCommand (Run ip acc visited) (Do line inst arg) = 
  if line `elem` visited
    then Halt ip acc visited RepeatVisit
    else case inst of
              Nop => Run (ip+1) acc (line :: visited)
              Acc => Run (ip+1) (acc+arg) (line :: visited)
              Jmp => Run (ip+arg) acc (line :: visited)

runTillHalt' : MachineState -> List Command -> MachineState
runTillHalt' state@(Halt _ _ _ _) _ = state
runTillHalt' state@(Run ip acc visited) cmds = 
  case index' (fromIntegerNat ip) cmds of
    Nothing => Halt ip acc visited IpOutOfRange
    Just cmd => runTillHalt' (executeCommand state cmd) cmds

runTillHalt : List Command -> MachineState
runTillHalt cmds = runTillHalt' (Run 0 0 []) cmds

main : IO ()
main = do file <- readFile "input.txt"
          case file of
              Right content =>
                let cmds = lines content 
                    len = cast (length cmds) in
                    printLn $ runTillHalt $ map parseCommand $ zip [0..len] cmds
              Left err => printLn err
