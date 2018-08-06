module Game.Texts where

convertNumRobot :: String -> Maybe Int
convertNumRobot num = case num of
  "" -> Nothing
  _  -> Just (read num :: Int)

convertMoves :: String -> Maybe String
convertMoves s = case s of
  "" -> Nothing
  _ -> Just s

getMovesAndRobots :: IO (Maybe String, Maybe Int)
getMovesAndRobots = do
  putStrLn $ "\nPlease enter a list of moves you would like the Robots to take. \n"
          ++ "The moves available are '^' 'V' '<' '>' if you don't use these they will be ignored \n"
          ++ "(press enter for default list of moves):"
  moves <- getLine
  putStrLn $ "How many Robots would you like (max 10)? \n"
          ++ "(press enter for default number of Robots):"
  numRobot <- getLine
  return (convertMoves moves, convertNumRobot numRobot)

robotTitle :: String
robotTitle =
  "\n\n"
  ++ "$$$$$$$\\            $$\\                  $$\\ \n"
  ++ "$$  __$$\\           $$ |                 $$ | \n"
  ++ "$$ |  $$ | $$$$$$\\  $$$$$$$\\   $$$$$$\\ $$$$$$\\    $$$$$$$\\ \n"
  ++ "$$$$$$$  |$$  __$$\\ $$  __$$\\ $$  __$$\\\\_$$  _|  $$  _____| \n"
  ++ "$$  __$$< $$ /  $$ |$$ |  $$ |$$ /  $$ | $$ |    \\$$$$$$\\ \n"
  ++ "$$ |  $$ |$$ |  $$ |$$ |  $$ |$$ |  $$ | $$ |$$\\  \\____$$\\ \n"
  ++ "$$ |  $$ |\\$$$$$$  |$$$$$$$  |\\$$$$$$  | \\$$$$  |$$$$$$$  | \n"
  ++ "\\__|  \\__| \\______/ \\_______/  \\______/   \\____/ \\_______/ \n"

byeBye :: String
byeBye = "👋 Thanks for playing see you soon!!!"

selectOption :: String
selectOption = "\n" ++ "λ - select a command use 'help' to view all commands:"

showHelp :: String
showHelp =
     "\n"
  ++ "---------- HELP ------------\n"
  ++ "run:all:  run the entire simulation \n"
  ++ "run:one:  run one one turn of the siulation \n"
  ++ "robots:   current status of all the robot \n"
  ++ "presents: total number of presents \n"
  ++ "houses:   show houses with 'x' or more presents delivered \n"
  ++ "viewTurn: display the state give 'x' move number"

successSim :: String
successSim = "🚀 Sucess the simulation was run 🚀"

successRobot :: String
successRobot = "The state of your robots after simulation "
