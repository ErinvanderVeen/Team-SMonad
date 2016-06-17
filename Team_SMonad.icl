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

:: Memory               = { home :: !Home, base_pos :: Position, can_touch_ball :: Bool, penalty :: Bool, goalkick :: Bool, shot :: Bool, field :: FootballField}

SMonad_player :: !ClubName !Home !FootballField !Position !PlayersNumber -> Footballer
SMonad_player club home footballfield position nr
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
    , brain               = { memory = {home=home, base_pos=bp, can_touch_ball=ctb, penalty=False, goalkick=False, shot=False, field=footballfield }, ai = player_brain}
    }

SMonad_keeper :: !ClubName !Home !FootballField !Position !PlayersNumber -> Footballer
SMonad_keeper club home footballfield position nr
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
    , brain               = { memory = {home=home, base_pos=bp, can_touch_ball=ctb, penalty=False, goalkick=False, shot=False, field = footballfield }, ai = keeper_brain}
    }

// The Brains
player_brain :: (BrainInput, Memory) -> (BrainOutput, Memory)
player_brain input = fromBrainIO (pure input >>= update_mem >>= try_to_score >>= advance >>= take_ball >>= pass_ball >>= move_to_position)
    where
        try_to_score :: (BrainInput, Memory) -> Either (BrainOutput, Memory) (BrainInput, Memory)
        try_to_score a=:(inp=:{me}, mem=:{home, field})
        # close = dist_to_goal a <= m 15.0
        | close = BrainO (KickBall {vxy = {direction = bearing zero (toPosition me) (centerOfGoal (other home) field), velocity = maxVelocityBallKick me}, vz = ms 1.0}, {mem & shot=True})
        | otherwise = BrainI a

        advance :: (BrainInput, Memory) -> Either (BrainOutput, Memory) (BrainInput, Memory)
        advance a=:(inp, mem=:{home, field})
        # axis_close = toReal (dist_to_closest_axis inp) <= 5.0
        # have_ball = i_have_ball inp
        | ~axis_close && have_ball = BrainO (fix (centerOfGoal (other home) field) (m 9.0) (inp, mem))
        | otherwise = BrainI a

        pass_ball :: (BrainInput, Memory) -> Either (BrainOutput, Memory) (BrainInput, Memory)
        pass_ball a=:(inp=:{others, me, football}, mem)
        # have_ball = i_have_ball inp
        # target = best_pass_option a
        | have_ball = trace_n "pass" (BrainO (KickBall {vxy = {direction = bearing zero (toPosition me) (toPosition (target)), velocity = ms (1.5 * (toReal (dist me.pos target.pos)))}, vz = ms 1.0}, {mem & shot = True}))
        | otherwise = BrainI a

        take_ball :: (BrainInput, Memory) -> Either (BrainOutput, Memory) (BrainInput, Memory)
        take_ball (inp=:{me, football}, mem=:{shot})
        # canGain = maxGainReach me >= dist me (getBall inp)
        # whb = we_have_ball inp
        # ball_direction = bearing (ball_direction (getBall inp)) (get_ball_location (getBall inp)) me
        | canGain && ~whb && ~shot = trace_n "take" (BrainO (GainBall, mem))
        | otherwise = BrainI (inp, mem)

        move_to_position :: (BrainInput, Memory) -> Either (BrainOutput, Memory) (BrainInput, Memory)
        move_to_position a=:(inp=:{me, football}, mem)
        # closest = am_closest_to_ball (getBall inp) me (allies inp)
        | closest = trace_n "move_to_position" (BrainO (fix (get_ball_location (getBall inp)) (maxGainReach me) (inp, mem)))
        | otherwise = BrainO (fix (scale_base_pos a) (m 2.0) (inp, mem))
        
keeper_brain :: (BrainInput, Memory) -> (BrainOutput, Memory)
keeper_brain input = fromBrainIO (pure input >>= update_mem >>= penalty >>= goalkick >>= take_ball >>= pass_ball >>= move_to_position)
    where
        penalty :: (BrainInput, Memory) -> Either (BrainOutput, Memory) (BrainInput, Memory)
        penalty (inp=:{me, football}, mem=:{penalty})
        | penalty && ~(ballIsFree football) = BrainO (afterfix (rotate (bearing me.nose (toPosition me) (toPosition (m 0.0, me.pos.py)))) mem.base_pos (m 2.0) (inp, mem))
        | penalty = BrainI (inp, {mem & penalty = ~penalty})
        | otherwise = BrainI (inp, mem)

        goalkick :: (BrainInput, Memory) -> Either (BrainOutput, Memory) (BrainInput, Memory)
        goalkick a=:(inp=:{me}, mem=:{goalkick})
        | goalkick = BrainO (afterfix (returnAI (KickBall {vxy = {direction = bearing zero (toPosition me) (toPosition (best_pass_option a)), velocity = maxVelocityBallKick me}, vz = maxVelocityBallKick me})) (getBall inp).ballPos.pxy (maxGainReach me) (inp, {mem & goalkick = ~goalkick}))
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
        pass_ball a=:(inp=:{me}, mem)
        | i_have_ball inp = BrainO (KickBall {vxy={direction=bearing zero (toPosition me) (toPosition (best_pass_option a)), velocity=maxVelocityBallKick me}, vz=ms 1.5}, mem)
        | otherwise = BrainI (inp, mem)

        move_to_position :: (BrainInput, Memory) -> Either (BrainOutput, Memory) (BrainInput, Memory)
        move_to_position (inp=:{me, football, others}, mem=:{home, field})
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
// We prefer players who are closer the the oponents goal
# footballers = map (\x -> (if (home == West) (toReal x.pos.px) (~(toReal x.pos.px)), x)) footballers
// We prefer free players
# footballers = map (\x -> (fst x + 1.5 * (toReal (dist_to_closest_axis inp)), snd x)) footballers
// We prefer footballers who are closer to the footballer
# footballers = map (\x -> (fst x - 0.5 * (toReal (dist (snd x) me)), snd x)) footballers
// Sort them based on the heuristic
# footballer = maxListBy (\x -> \y -> (fst x < fst y)) footballers
= snd footballer

am_closest_to_ball :: Football Footballer [Footballer] -> Bool
am_closest_to_ball fb me rest = all ((<) (dist me.pos (get_ball_location fb))) [dist footballer.pos (get_ball_location fb)\\ footballer <- rest]

dist_to_closest_axis :: BrainInput -> Metre
dist_to_closest_axis inp=:{me} = minList [dist footballer.pos (me.pos)\\ footballer <- axis inp]

dist_to_goal :: (BrainInput, Memory) -> Metre
dist_to_goal (inp=:{me}, mem=:{home, field}) = dist (centerOfGoal (other home) field) (me.pos)

scale_base_pos :: (BrainInput, Memory) -> Position
scale_base_pos (inp=:{me}, mem=:{home, field, base_pos})
# ball_x = (get_ball_location (getBall inp)).px
# ball_scale = (toReal (ball_x)) / (toReal (field.flength))
# ball_scale = if (home == East) (~(ball_scale)) ball_scale
# ball_scale = ball_scale + 0.7
# ball_scale = 2.0 * ball_scale
# offset = if (home == East) ((scale 0.5 field.flength) - base_pos.px) ((scale 0.5 field.flength) + base_pos.px)
# offset = scale ball_scale offset
//# offset = scale 1.5 offset
# x = if (home == East) ((scale 0.5 field.flength) - offset) (~((scale 0.5 field.flength) - offset))
= {px=x, py=base_pos.py}

