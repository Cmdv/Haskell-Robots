module Game.Texts where

getMovesAndRobots :: IO (String, Int)
getMovesAndRobots = do
  putStrLn $ "\nPlease enter a list of moves you would like the Robots to take. \n"
          ++ "The moves available are '^' 'V' '<' '>' if you don't use these they will be ignored \n"
          ++ "(press enter for default list of moves):"
  moves <- getLine
  putStrLn $ "How many Robots would you like (max 10)? \n"
          ++ "(press enter for default number of Robots):"
  numRobot <- getLine
  return (moves, read numRobot :: Int)

robotTitle :: String
robotTitle =
  "\n\n"
  ++ "8888888b.           888               888             \n"
  ++ "888   Y88b          888               888             \n"
  ++ "888    888          888               888             \n"
  ++ "888   d88P  .d88b.  88888b.   .d88b.  888888 .d8888b  \n"
  ++ "8888888P\"  d88\"\"88b 888 \"88b d88\"\"88b 888    88K      \n"
  ++ "888 T88b   888  888 888  888 888  888 888    \"Y8888b. \n"
  ++ "888  T88b  Y88..88P 888 d88P Y88..88P Y88b.       X88 \n"
  ++ "888   T88b  \"Y88P\"  88888P\"   \"Y88P\"   \"Y888  88888P'\n"

byeBye :: String
byeBye = "Thanks for playing see you soon!!!"

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

success :: String
success = "🚀 Sucess the simulation was run 🚀"
