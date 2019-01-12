package com.example

import cats.Show
import cats.data.StateT
import cats.effect.IO
import cats.effect.concurrent.Ref
import cats.implicits._
import com.example.samegame._

object Interpreters {
  def withState(): Game[StateT[IO, SearchState[samegame.Position, samegame.Game, Int], ?], samegame.Position, samegame.Game, Int] =
    new Game[StateT[IO, SearchState[samegame.Position, samegame.Game, Int], ?], samegame.Position, samegame.Game, Int] {

      def applyMove(move: samegame.Position): StateT[IO, S, Unit] =
        StateT.modify[IO, S] { searchState =>
          val nextPosition = SameGame.applyMove(move, searchState.gameState.position)
          val gameState = searchState.gameState.copy(
            position = nextPosition,
            score = SameGame.score(nextPosition),
            playedMoves = move :: searchState.gameState.playedMoves
          )
          searchState.copy(gameState = gameState)
        }

      def legalMoves: StateT[IO, S, List[samegame.Position]] =
        StateT.inspect[IO, S, List[samegame.Position]] { searchState =>
          SameGame.legalMoves(searchState.gameState.position)
        }

      def rndInt(bound: Int): StateT[IO, S, Int] = StateT[IO, S, Int] { searchState =>
        val (nextSeed, i) = searchState.seed.nextInt(bound)
        IO { (searchState.copy(seed = nextSeed), i) }
      }

      def simulation: StateT[IO, S, Unit] = {
        val playRndLegalMove = for {
          moves <- legalMoves
          isTerminalPosition <- moves match {
            case Nil => StateT.pure[IO, S, Boolean](true)
            case ms  => rndInt(ms.length).flatMap(i => applyMove(ms(i))).as(false)
          }
        } yield isTerminalPosition

        playRndLegalMove.iterateUntil(isTerminalPosition => isTerminalPosition).void
      }

      def gameState: StateT[IO, SearchState[Position, samegame.Game, Int], GameState[Position, samegame.Game, Int]] =
        StateT.inspect(_.gameState)

      def bestSequence: StateT[IO, SearchState[Position, samegame.Game, Int], Option[Result[Position, Int]]] =
        StateT.inspect(_.bestSequence)

      def bestTotal: StateT[IO, SearchState[Position, samegame.Game, Int], Option[Result[Position, Int]]] =
        StateT.inspect(_.bestTotal)

      def update(f: S => S): StateT[IO, S, Unit] = StateT.modify[IO, S](f)

      def log(msg: String): StateT[IO, S, Unit] =
        StateT[IO, S, Unit](s => IO(println(msg)).map((s, _)))
    }

  def withIORef(initial: SearchState[samegame.Position, samegame.Game, Int]): IO[Game[IO, samegame.Position, samegame.Game, Int]] =
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

        def simulation: IO[Unit] = {
          val playRndLegalMove = for {
            moves <- legalMoves
            isTerminalPosition <- moves match {
              case Nil => true.pure[IO]
              case ms  => rndInt(ms.length).flatMap(i => applyMove(ms(i))).as(false)
            }
          } yield isTerminalPosition

          playRndLegalMove.iterateUntil(isTerminalPosition => isTerminalPosition).void
        }

        def gameState: IO[GameState[Position, samegame.Game, Int]] =
          ref.get.map(_.gameState)

        def bestSequence: IO[Option[Result[Position, Int]]] =
          ref.get.map(_.bestSequence)

        def bestTotal: IO[Option[Result[Position, Int]]] =
          ref.get.map(_.bestTotal)

        def update(f: S => S): IO[Unit] = ref.update(f)

        def log(msg: String): IO[Unit] = IO(println(msg))
      }

  implicit val showCell: Show[CellState] = (cellState: CellState) =>
    cellState match {
      case Empty         => "-"
      case Filled(Green) => "0"
      case Filled(Blue)  => "1"
      case Filled(Red)   => "2"
      case Filled(Brown) => "3"
      case Filled(Gray)  => "4"
  }

  implicit val showBoard: Show[Board] = (board: Board) =>
    board.columns.map(col => col.cells.map(c => show"$c").reverse).transpose.map(_.mkString("[", ",", "]")).mkString("\n")

  implicit val showGame: Show[samegame.Game] = (game: samegame.Game) =>
    game match {
      case InProgress(board, score) => show"$board\n\nScore: $score (game in progress)"
      case Finished(board, score)   => show"$board\n\nScore: $score (game finished)"
  }

  val showResult: Show[GameState[samegame.Position, samegame.Game, Int]] = (t: GameState[samegame.Position, samegame.Game, Int]) => show"""
       |${t.position}
       |
       |Moves: ${t.playedMoves.reverse.map(p => s"(${p.col}, ${p.row})").mkString("[", ", ", "]")}
       |""".stripMargin

  val showResultAsQueryParams: Show[GameState[samegame.Position, samegame.Game, Int]] = (t: GameState[samegame.Position, samegame.Game, Int]) =>
    show"""${t.playedMoves.reverse.map(p => s"(${p.col}, ${p.row})").mkString("[", ", ", "]")}
       |Score: ${t.score}
       |""".stripMargin
}
