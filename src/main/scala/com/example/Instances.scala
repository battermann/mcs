package com.example

import cats.data.State
import cats.effect.IO
import cats.effect.concurrent.Ref
import com.example.samegame.SameGame
import cats.implicits._

object Instances {
  def createState()
    : Game[State[SearchState[samegame.Position, samegame.Game, Int], ?], samegame.Position, samegame.Game, Int] =
    new Game[State[SearchState[samegame.Position, samegame.Game, Int], ?], samegame.Position, samegame.Game, Int] {

      def applyMove(move: samegame.Position): State[S, Unit] =
        State.modify[S] { searchState =>
          val nextPosition = SameGame.applyMove(move, searchState.gameState.position)
          val gameState = searchState.gameState.copy(
            position = nextPosition,
            score = SameGame.score(nextPosition),
            playedMoves = move :: searchState.gameState.playedMoves
          )
          searchState.copy(gameState = gameState)
        }

      def legalMoves: State[S, List[samegame.Position]] =
        State.inspect[S, List[samegame.Position]] { searchState =>
          SameGame.legalMoves(searchState.gameState.position)
        }

      def rndInt(bound: Int): State[S, Int] = State[S, Int] { searchState =>
        val (nextSeed, i) = searchState.seed.nextInt(bound)
        (searchState.copy(seed = nextSeed), i)
      }

      def rndSimulation(): State[S, Unit] = {
        val playRndLegalMove = for {
          moves <- legalMoves
          isTerminalPosition <- moves match {
            case Nil => State.pure[S, Boolean](true)
            case ms  => rndInt(ms.length).flatMap(i => applyMove(ms(i))).as(false)
          }
        } yield isTerminalPosition

        playRndLegalMove.iterateUntil(isTerminalPosition => isTerminalPosition).void
      }

      def set(searchState: S): State[S, Unit] = State.set[S](searchState)
      def get(): State[S, S]                  = State.get[S]
      def inspect[A](f: S => A): State[S, A]  = State.inspect[S, A](f)
      def modify(f: S => S): State[S, Unit]   = State.modify[S](f)
      def pure[A](a: A): State[S, A]          = State.pure[S, A](a)
    }

  def createIORef(initial: SearchState[samegame.Position, samegame.Game, Int]): IO[Game[IO, samegame.Position, samegame.Game, Int]] =
    for {
      ref <- Ref.of[IO, SearchState[samegame.Position, samegame.Game, Int]](initial)
    } yield
      new Game[IO, samegame.Position, samegame.Game, Int] {
        def applyMove(move: samegame.Position): IO[Unit] =
          ref.update { searchState =>
            val nextPosition = SameGame.applyMove(move, searchState.gameState.position)
            val gameState = searchState.gameState.copy(
              position = nextPosition,
              score = SameGame.score(nextPosition),
              playedMoves = move :: searchState.gameState.playedMoves
            )
            searchState.copy(gameState = gameState)
          }

        def legalMoves: IO[List[samegame.Position]] =
          ref.get.map(searchState => SameGame.legalMoves(searchState.gameState.position))

        def rndInt(bound: Int): IO[Int] =
          IO(scala.util.Random.nextInt(bound))

        def rndSimulation(): IO[Unit] = {
          val playRndLegalMove = for {
            moves <- legalMoves
            isTerminalPosition <- moves match {
              case Nil => true.pure[IO]
              case ms  => rndInt(ms.length).flatMap(i => applyMove(ms(i))).as(false)
            }
          } yield isTerminalPosition

          playRndLegalMove.iterateUntil(isTerminalPosition => isTerminalPosition).void
        }

        def set(searchState: S): IO[Unit] = ref.set(searchState)
        def get(): IO[S]                  = ref.get
        def inspect[A](f: S => A): IO[A]  = ref.get.map(f)
        def modify(f: S => S): IO[Unit]   = ref.update(f)
        def pure[A](a: A): IO[A]          = a.pure[IO]
      }

}
