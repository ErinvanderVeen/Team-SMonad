implementation module Team_SMonad

import StdEnv, StdIO, Footballer, FootballerFunctions, StdDebug

// Define the Monad
class Monad m where
  pure           :: a -> m a
  (>>=) infixl 1 :: (m a) (a -> m b) -> m b

:: Either e a =  BrainO e | BrainI a

// I know this is not a real Monad. But... It works
instance Monad (Either e) where
  (>>=) (BrainO e) _ = BrainO e
  (>>=) (BrainI a) next = next a

  pure x = BrainI x

// To make sure we can always return a (BrainOutput, Memory)
fromBrainIO :: (Either (BrainOutput, Memory) (BrainInput, Memory)) -> (BrainOutput, Memory)
fromBrainIO (BrainO e) = e
fromBrainIO (BrainI (inp, mem)) = (halt` inp, mem)

// Create the team
Team_SMonad :: SMonad_NumPlayers Home FootballField -> Team
Team_SMonad nr_players home field
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

:: Memory               = { home :: !Home, base_pos :: Position, can_touch_ball :: Bool, penalty :: Bool, goalkick :: Bool, shot :: Bool}

SMonad_player :: !ClubName !Home !FootballField !Position !PlayersNumber -> Footballer
SMonad_player club home field position nr
    # bp = if (home == West) (position) ({px= ~position.px, py=position.py})
    # ctb = home == West
    = 
    { playerID            = {clubName = club,playerNr=nr}
    , name                = "Monad " <+++ nr
    , length              = max_length
    , pos                 = position
    , nose                = zero
    , speed               = zero
    , skills              = (Running, Kicking, Gaining)
    , effect              = Nothing
    , stamina             = max_stamina
    , health              = max_health
    , brain               = { memory = {home=home, base_pos=bp, can_touch_ball=ctb, penalty=False, goalkick=False, shot=False}, ai = player_brain field}
    }

SMonad_keeper :: !ClubName !Home !FootballField !Position !PlayersNumber -> Footballer
SMonad_keeper club home field position nr
    # bp = if (home == West) (position) ({px= ~position.px, py=position.py})
    # ctb = home == West
    = 
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
    , brain               = { memory = {home=home, base_pos=bp, can_touch_ball=ctb, penalty=False, goalkick=False, shot=False}, ai = keeper_brain field}
    }

// The Brains
player_brain :: FootballField (BrainInput, Memory) -> (BrainOutput, Memory)
player_brain field input = fromBrainIO (pure input >>= update_mem >>= try_to_score >>= advance >>= take_ball >>= pass_ball >>= move_to_position)
    where
        try_to_score :: (BrainInput, Memory) -> Either (BrainOutput, Memory) (BrainInput, Memory)
        try_to_score a=:(inp, mem) = BrainI a

        advance :: (BrainInput, Memory) -> Either (BrainOutput, Memory) (BrainInput, Memory)
        advance a=:(inp, mem) = BrainI a

        pass_ball :: (BrainInput, Memory) -> Either (BrainOutput, Memory) (BrainInput, Memory)
        pass_ball a=:(inp=:{others, me, football}, mem)
        # have_ball = i_have_ball inp
        | have_ball = trace_n "pass" (BrainO (KickBall {vxy = {direction = bearing zero (toPosition me) (toPosition (best_pass_option a)), velocity = maxVelocityBallKick me}, vz = ms 1.0}, {mem & shot = True}))
        | otherwise = BrainI a

        take_ball :: (BrainInput, Memory) -> Either (BrainOutput, Memory) (BrainInput, Memory)
        take_ball (inp=:{me, football}, mem=:{shot})
        # canGain = maxGainReach me >= dist me (getBall inp)
        # whb = we_have_ball inp
        # ball_direction = bearing (ball_direction (getBall inp)) (get_ball_location (getBall inp)) me
        | canGain && ~whb && ~shot = trace_n "take" (BrainO (GainBall, mem))
        | otherwise = BrainI (inp, mem)

        move_to_position :: (BrainInput, Memory) -> Either (BrainOutput, Memory) (BrainInput, Memory)
        move_to_position (inp=:{me, football}, mem)
        # is_free = ballIsFree football
        # closest = am_closest_to_ball (getBall inp) me (allies inp)
        | is_free && closest = BrainO (fix (get_ball_location (getBall inp)) (maxGainReach me) (inp, mem))
        | otherwise = BrainI (inp, mem)
        
keeper_brain :: FootballField (BrainInput, Memory) -> (BrainOutput, Memory)
keeper_brain field input = fromBrainIO (pure input >>= update_mem >>= penalty >>= goalkick >>= take_ball >>= pass_ball >>= move_to_position field)
    where
        penalty :: (BrainInput, Memory) -> Either (BrainOutput, Memory) (BrainInput, Memory)
        penalty (inp=:{me, football}, mem=:{penalty})
        | penalty && ~(ballIsFree football) = BrainO (afterfix (rotate (bearing me.nose (toPosition me) (toPosition (m 0.0, me.pos.py)))) mem.base_pos (m 2.0) (inp, mem))
        | penalty = BrainI (inp, {mem & penalty = ~penalty})
        | otherwise = BrainI (inp, mem)

        goalkick :: (BrainInput, Memory) -> Either (BrainOutput, Memory) (BrainInput, Memory)
        goalkick (inp=:{me}, mem=:{goalkick})
        | goalkick = BrainO (afterfix (returnAI (KickBall {vxy = {direction = bearing zero (toPosition me) (toPosition (m 0.0, m 0.0)), velocity = maxVelocityBallKick me}, vz = maxVelocityBallKick me})) (getBall inp).ballPos.pxy (maxGainReach me) (inp, {mem & goalkick = ~goalkick}))
        | otherwise = BrainI (inp, mem)

        take_ball :: (BrainInput, Memory) -> Either (BrainOutput, Memory) (BrainInput, Memory)
        take_ball (inp=:{me, football}, mem)
        # canCatch = maxCatchReach me >= get_ball_height (getBall inp)
        # canGain = maxGainReach me >= dist me (getBall inp)
        # whb = we_have_ball inp
        # ball_direction = bearing (ball_direction (getBall inp)) (get_ball_location (getBall inp)) me
        # should_catch = ball_direction <= (degree 90) && ball_direction >= (degree 270)
        | should_catch && canCatch && canGain && ~whb = BrainO (CatchBall, mem)
        | otherwise = BrainI (inp, mem)

        pass_ball :: (BrainInput, Memory) -> Either (BrainOutput, Memory) (BrainInput, Memory)
        pass_ball (inp=:{me}, mem)
        | i_have_ball inp = BrainO (KickBall {vxy={direction=bearing zero (toPosition me) (toPosition (m 0.0, m 0.0)), velocity=maxVelocityBallKick me}, vz=ms 1.5}, mem)
        | otherwise = BrainI (inp, mem)

        move_to_position :: FootballField (BrainInput, Memory) -> Either (BrainOutput, Memory) (BrainInput, Memory)
        move_to_position field (inp=:{me, football, others}, mem=:{home})
        # closest = am_closest_to_ball (getBall inp) me (axis inp)
        | inPenaltyArea field home (get_ball_location (getBall inp)) && closest = BrainO (Move {direction = (bearing zero me (getBall inp)), velocity = max_velocity} (bearing me.nose me (getBall inp)), mem)
        | otherwise = BrainO (afterfix (rotate (bearing me.nose (toPosition me) (toPosition (m 0.0, me.pos.py)))) mem.base_pos (m 2.0) (inp, mem))

update_mem :: (BrainInput, Memory) -> Either (BrainOutput, Memory) (BrainInput, Memory)
update_mem (inp=:{me, referee}, mem=:{home, base_pos, penalty, goalkick, shot})
# new_memory = {mem & home = if (any isEndHalf referee) (other home) home,
                base_pos = if (any isEndHalf referee) ({px = ~base_pos.px, py = base_pos.py}) base_pos,
                penalty = any isPenalty referee || penalty,
                goalkick = any isGoalKick referee || goalkick,
                shot = dist (getBall inp) me > (m 2.0) && shot
            }
= BrainI (inp, new_memory)

afterfix :: (FootballerAI Memory) Position Metre (BrainInput, Memory) -> (BrainOutput, Memory)
afterfix after point diff (input=:{me}, m)
| d < diff = after (input, m)
| otherwise = (move, m)
where
    d = dist me point
    a = bearing zero me point
    r = bearing me.nose me point
    v = ms (max 6.0 (toReal d))
    move = Move {direction = a, velocity = v} r

get_ball_height :: Football -> Metre
get_ball_height fb = fb.ballPos.pz

get_ball_location :: Football -> Position
get_ball_location fb = fb.ballPos.pxy

allies :: BrainInput -> [Footballer]
allies {others, me} = [x \\ x <- others | sameClub me x]

axis :: BrainInput -> [Footballer]
axis inp=:{others} = removeMembers others (allies inp)

we_have_ball :: BrainInput -> Bool
we_have_ball inp=:{football, me} = or [ballIsGainedBy x.playerID football \\ x <- (allies inp) ++ [me]]

i_have_ball :: BrainInput -> Bool
i_have_ball {me, football} = ballIsGainedBy me.playerID football

max_velocity :: Velocity
max_velocity = ms 299792458.0

ball_direction :: Football -> Angle
ball_direction football = football.ballSpeed.vxy.direction

best_pass_option :: (BrainInput, Memory) -> Footballer
best_pass_option (inp=:{others, me}, {home})
// Add a weight to every footballer
// The footballers will eventually be sorted on their weight
# footballers = allies inp
# footballers = map (\x -> (if (home == West) (~(toReal x.pos.px)) (toReal x.pos.px), x)) footballers
// We prefer footballers who are more near the center
# footballers = map (\x -> (fst x - 0.5 * (abs (toReal ((snd x).pos.py))), snd x)) footballers
// We prefer footballers who are closer to the footballer
# footballers = map (\x -> (fst x - 0.25 * (toReal (dist (snd x) me)), snd x)) footballers
// Sort them based on the heuristic
# footballers = sortBy (\x -> \y -> (fst x < fst y)) footballers
= snd (hd footballers)

am_closest_to_ball :: Football Footballer [Footballer] -> Bool
am_closest_to_ball fb me rest = all ((<) (dist me.pos (get_ball_location fb))) [dist footballer.pos (get_ball_location fb)\\ footballer <- rest]
