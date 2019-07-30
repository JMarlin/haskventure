import Data.Maybe
import Data.List
import System.IO

main = do
    hSetBuffering stdout NoBuffering
    core init_state 

data GameState = GameState {
        room :: Room
    }

currentRoom = room
setRoom state new_room = state { room = new_room }

(#) a b = b a

try_looking state command = do
    new_state <- (lookCallback ( currentRoom state)) state
    return new_state 

try_going state command = do
    let room_name = intercalate " " (tail . words $ command) in
      case (state # currentRoom) # getRoomByName $ room_name of
        Nothing -> do
          putStrLn "I don't know that room"
          return state
        (Just new_room) -> do
          looked_state <- new_room # lookCallback $ state
          return $ looked_state # setRoom $ new_room

cmd_exit state command = do
    putStrLn "Bye!"
    return state

data Room = Room {
            name           :: String,
            lookCallback   :: GameState -> IO GameState,
            actionCallback :: GameState-> String -> IO GameState,
            customActions  :: [String],
            rooms          :: [Room]
    }
    

data Action = Action { 
              command   :: GameState -> String -> IO GameState, 
              exitsRepl :: Bool
    }

commands = 
    [
        ("look", Action try_looking False),
        ("go",   Action try_going   False),
        ("exit", Action cmd_exit    True)
    ]  

living_room = Room "Living Room"
    (\gs -> do 
        putStrLn "You're in a living room"
        return gs)
    (\gs action -> do
        case action of
            "kick butt"  -> putStrLn "You kick some butt"
            "take names" -> putStrLn "You take a notepad out of your inner jacket pocket and jot down the first names that come to your mind"
            _            -> return ()
        return gs)
    ["kick butt", "take names"]
    [kitchen_room]

kitchen_room = Room "Kitchen"
    (\gs -> do
        putStrLn "You're in the kitchen"
        return gs)
    (\gs action -> do
        case action of 
            "cook dinner" -> putStrLn "You make a delicious dinner, eat it all, and even clean the dishes when you're done"
            _             -> return ()
        return gs)
    ["cook dinner"]
    [living_room]

getRoomByName :: Room -> String -> Maybe Room
getRoomByName parent_room search_name = foldr find_match Nothing (parent_room # rooms)
    where find_match _ found_item@(Just x) = found_item
          find_match current_item Nothing  = 
              if (current_item # name) == search_name
                then Just current_item
                else Nothing

init_state = GameState { 
    room = living_room 
}

core :: GameState -> IO GameState
core game_state = do
    putStr "> "
    value <- getLine
    let action = (lookup (head . words $ value) commands) in
        if action # isNothing 
          then if value `elem` ((game_state # currentRoom) # customActions)
              then do
                  new_state <- ((game_state # currentRoom) # actionCallback) game_state value
                  core new_state
              else do
                  putStrLn "Unknown command"
                  core game_state
          else let (Just action_value) = action in do
              new_state <- (action_value # command) game_state value
              if action_value # exitsRepl
                  then return new_state 
                  else core new_state 


