definition module Team_SMonad

/*
  Team Soccer Monad.
  Author: Erin van der Veen
          s4431200
*/

import Team

// The number of players
// Prefix to prevent collision with Team_Harmless
:: SMonad_NumPlayers :== Int

Team_SMonad :: SMonad_NumPlayers Home FootballField -> Team

Team_SMonad_Pro :== Team_SMonad 11
