package mcs

import cats.Show
import cats.data.StateT
import cats.effect.IO
import cats.effect.concurrent.Ref
import cats.implicits._
import mcs.Prng.Seed
import mcs.samegame._

object Interpreters {
  def gameInstanceStateT(): Game[StateT[IO, SearchState[samegame.Position, samegame.Game, Int, Seed], ?], samegame.Position, samegame.Game, Int, Seed] =
    new Game[StateT[IO, SearchState[samegame.Position, samegame.Game, Int, Seed], ?], samegame.Position, samegame.Game, Int, Seed] {

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

      def gameState: StateT[IO, S, GameState[Position, samegame.Game, Int]] =
        StateT.inspect(_.gameState)

      def bestSequence: StateT[IO, S, Option[Result[Position, Int]]] =
        StateT.inspect(_.bestSequence)

      def bestTotal: StateT[IO, S, Option[Result[Position, Int]]] =
        StateT.inspect(_.bestTotal)

      def update(f: S => S): StateT[IO, S, Unit] = StateT.modify[IO, S](f)
    }

  def gameInstanceIORef(initial: SearchState[samegame.Position, samegame.Game, Int, Unit]): IO[Game[IO, samegame.Position, samegame.Game, Int, Unit]] =
    for {
      ref <- Ref.of[IO, SearchState[samegame.Position, samegame.Game, Int, Unit]](initial)
    } yield
      new Game[IO, samegame.Position, samegame.Game, Int, Unit] {
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
      }

  val loggerState: Logger[StateT[IO, SearchState[samegame.Position, samegame.Game, Int, Seed], ?]] =
    new Logger[StateT[IO, SearchState[samegame.Position, samegame.Game, Int, Seed], ?]] {
      def log[T: Show](t: T): StateT[IO, SearchState[samegame.Position, samegame.Game, Int, Seed], Unit] =
        StateT[IO, SearchState[samegame.Position, samegame.Game, Int, Seed], Unit](s => IO(println(t.show)).map((s, _)))
    }

  val loggerIORef: Logger[IO] = new Logger[IO] {
    def log[T: Show](t: T): IO[Unit] = IO(println(t.show))
  }

  implicit val showCell: Show[CellState] = Show.show {
    case Empty         => "-"
    case Filled(Green) => "0"
    case Filled(Blue)  => "1"
    case Filled(Red)   => "2"
    case Filled(Brown) => "3"
    case Filled(Gray)  => "4"
  }

  implicit val showMove: Show[samegame.Position] =
    Show.show(p => show"(${p.col}, ${p.row})")

  implicit val showList: Show[List[samegame.Position]] =
    Show.show(_.map(_.show).mkString("[", ", ", "]"))

  implicit val showResult: Show[Result[samegame.Position, Int]] =
    Show.show(result => show">>> Improved sequence found\n>>> Score: ${result.score.show}, Moves: ${result.moves.reverse.show}")

  implicit val showBoard: Show[Board] =
    Show.show(_.columns.map(col => col.cells.map(_.show).reverse).transpose.map(_.mkString("[", ",", "]")).mkString("\n"))

  implicit val showGame: Show[samegame.Game] = Show.show {
    case InProgress(board, score) => show"$board\n\nScore: $score (game in progress)"
    case Finished(board, score)   => show"$board\n\nScore: $score (game finished)"
  }

  implicit val showGameState: Show[GameState[samegame.Position, samegame.Game, Int]] = Show.show(t => show"""
       |${t.position}
       |
       |Moves: ${t.playedMoves.reverse}
       |""".stripMargin)

  implicit val showGameStateAsQueryParams: Show[GameState[samegame.Position, samegame.Game, Int]] =
    Show.show(t => show"""Moves: ${t.playedMoves.reverse.map(p => s"move=${p.col}%2C${p.row}").mkString("&")}
                   |
                   |Score: ${t.score}
                   |""".stripMargin)

  implicit val showGameStateAsJsFunctionCalls: Show[GameState[samegame.Position, samegame.Game, Int]] =
    Show.show(t => show"""Moves: ${t.playedMoves.reverse.map(p => s"sg_remove(${p.col},${14 - p.row})").mkString(";")}
                  |
                  |Score: ${t.score}
                  |""".stripMargin)

}
