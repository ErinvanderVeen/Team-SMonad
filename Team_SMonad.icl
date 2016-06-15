implementation module Team_SMonad

import StdEnv, StdIO, Footballer, FootballerFunctions

// Define the Monad
class Monad m where
  pure           :: a -> m a
  (>>=) infixl 1 :: (m a) (a -> m b) -> m b

:: Either e a =  BrainO e | BrainI a

instance Monad (Either e) where
  (>>=) (BrainO e) _ = BrainO e
  (>>=) (BrainI a) next = next a

  pure x = BrainI x

// To make sure we can always return a (BrainOutput, Memory)
fromBrainIO :: (Either (BrainOutput, Memory) (BrainInput, Memory)) -> (BrainOutput, Memory)
fromBrainIO (BrainO e) = e
fromBrainIO (BrainI (_, mem)) = (GainBall, mem)

// Create the team
Team_SMonad :: SMonad_NumPlayers SMonad_Difficulty Home FootballField -> Team
Team_SMonad nr_players difficulty home field
|   home==West              = westTeam
|   otherwise               = eastTeam
where
    eastTeam                = mirror field westTeam
    westTeam                = [keeper : fielders]
    clubname                = base_TeamName_SMonad +++ if (home == West) " West" " East"
    keeper                  = SMonad_keeper clubname home field {zero & px=scale -0.5 field.flength} 1
    fielders                = [  SMonad_player clubname home field {px=scale (-0.5*dx) field.flength,py=scale (dy - 0.5) field.fwidth} nr
                              \\ (dx,dy) <- generate_positions (nr_players - 1)
                               & nr      <- [2..]
                              ]
    where
        generate_positions :: Int -> [(Real, Real)]
        generate_positions nr_players = zip2 (halton_sequence 3 nr_players) (halton_sequence 2 nr_players)
        where
            halton_sequence :: Int Int -> [Real]
            halton_sequence base nr
            # powers = map ((^) base) [1..]
            # seq = flatten (map halton_sequence` powers)
            = until_len nr seq []
            where
                halton_sequence` :: Int -> [Real]
                halton_sequence` base = map (\x -> (toReal x) / (toReal base)) [1 .. base - 1]
                
                until_len :: Int [Real] [Real] -> [Real]
                until_len n [x:xs] now
                | n == 0 = now
                | isMember x now = until_len n xs now
                | otherwise = until_len (n-1) xs [x : now]
                              
base_TeamName_SMonad        :: String
base_TeamName_SMonad        = "Soccer Monad"                              

:: Memory               = { home :: !Home }

SMonad_player :: !ClubName !Home !FootballField !Position !PlayersNumber -> Footballer
SMonad_player club home field position nr
    = { playerID            = {clubName = club,playerNr=nr}
      , name                = "Monad " <+++ nr
      , length              = min_length
      , pos                 = position
      , nose                = zero
      , speed               = zero
      , skills              = (Running, Kicking, Rotating)
      , effect              = Nothing
      , stamina             = max_stamina
      , health              = max_health
      , brain               = { memory = {home=home}, ai = player_brain }
      }

SMonad_keeper :: !ClubName !Home !FootballField !Position !PlayersNumber -> Footballer
SMonad_keeper club home field position nr =
    { playerID            = {clubName = club,playerNr=nr}
    , name                = "Monad " <+++ nr
    , length              = max_length
    , pos                 = position
    , nose                = zero
    , speed               = zero
    , skills              = (Catching, Kicking, Gaining)
    , effect              = Nothing
    , stamina             = max_stamina
    , health              = max_health
    , brain               = { memory = {home=home}, ai = keeper_brain }
    }

// The Brains
player_brain :: (BrainInput, Memory) -> (BrainOutput, Memory)
player_brain input = fromBrainIO (pure input >>= try_to_score)
    where
        try_to_score :: (BrainInput, Memory) -> Either (BrainOutput, Memory) (BrainInput, Memory)
        try_to_score ({me, referee}, memory) = BrainO (KickBall {vxy={direction=me.nose,velocity=(ms 299792458.0)}, vz=ms 1.0}, memory)
        
keeper_brain :: (BrainInput, Memory) -> (BrainOutput, Memory)
keeper_brain input = fromBrainIO (pure input >>= try_to_score)
    where
        try_to_score :: (BrainInput, Memory) -> Either (BrainOutput, Memory) (BrainInput, Memory)
        try_to_score ({me, referee}, memory) = BrainO (KickBall {vxy={direction=me.nose,velocity=(ms 299792458.0)}, vz=ms 1.0}, memory)
