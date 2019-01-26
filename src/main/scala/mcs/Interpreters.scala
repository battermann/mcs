package mcs

import cats.{Eq, Show}
import cats.data.StateT
import cats.effect.IO
import cats.effect.concurrent.Ref
import cats.implicits._
import mcs.Prng.Seed
import mcs.samegame._
import mcs.util.ListUtils

object Interpreters {

  type Move          = samegame.Position
  type BoardPosition = samegame.Game

  final case class SearchState[S](
      seed: S,
      gameState: GameState[Move, BoardPosition, Int],
      bestSequence: Option[Result[Move, Int]],
      /** `bestTotal` is not driving the search. But tracks overall best result for logging. */
      bestTotal: Option[Result[Move, Int]]
  )

  type StateIO[A] = StateT[IO, SearchState[Seed], A]

  val gameInterpreterStateT: Game[StateIO, Move, BoardPosition, Int, Seed] =
    new Game[StateIO, Move, BoardPosition, Int, Seed] {

      def applyMove(move: Move): StateIO[Unit] =
        StateT.modify[IO, SearchState[Seed]] { searchState =>
          val nextPosition = SameGame.applyMove(move, searchState.gameState.position)
          val gameState = searchState.gameState.copy(
            position = nextPosition,
            score = SameGame.score(nextPosition),
            playedMoves = move :: searchState.gameState.playedMoves
          )
          searchState.copy(gameState = gameState)
        }

      def legalMoves: StateIO[List[Move]] =
        StateT.inspect[IO, SearchState[Seed], List[Move]] { searchState =>
          SameGame.legalMoves(searchState.gameState.position)
        }

      private def rndInt(bound: Int): StateIO[Int] = StateT[IO, SearchState[Seed], Int] { searchState =>
        val (nextSeed, i) = searchState.seed.nextInt(bound)
        IO { (searchState.copy(seed = nextSeed), i) }
      }

      def simulation: StateIO[Unit] = {
        val playRndLegalMove = for {
          moves <- legalMoves
          isTerminalPosition <- moves match {
            case Nil => StateT.pure[IO, SearchState[Seed], Boolean](true)
            case ms  => rndInt(ms.length).flatMap(i => applyMove(ms(i))).as(false)
          }
        } yield isTerminalPosition

        playRndLegalMove.iterateUntil(identity).void
      }

      def gameState: StateIO[GameState[Position, BoardPosition, Int]] =
        StateT.inspect(_.gameState)

      def bestSequence: StateIO[Option[Result[Position, Int]]] =
        StateT.inspect(_.bestSequence)

      def bestTotal: StateIO[Option[Result[Position, Int]]] =
        StateT.inspect(_.bestTotal)

      def updateGameState(gameState: GameState[Position, BoardPosition, Int]): StateT[IO, SearchState[Seed], Unit] =
        StateT.modify[IO, SearchState[Seed]](_.copy(gameState = gameState))

      def updateBestTotal(bestTotal: Result[Position, Int]): StateT[IO, SearchState[Seed], Unit] =
        StateT.modify[IO, SearchState[Seed]](_.copy(bestTotal = bestTotal.some))

      def updateBestSequence(bestSequence: Option[Result[Position, Int]]): StateT[IO, SearchState[Seed], Unit] =
        StateT.modify[IO, SearchState[Seed]](_.copy(bestSequence = bestSequence))

      def isPrefixOf(gameState: GameState[Position, BoardPosition, Int])(result: Result[Position, Int]): Boolean =
        ListUtils.isSuffixOf(gameState.playedMoves, result.moves)(Eq.fromUniversalEquals)

      def next(gameState: GameState[Position, BoardPosition, Int], result: Result[Position, Int]): Option[Position] =
        if (isPrefixOf(gameState)(result) && gameState.playedMoves.length < result.moves.length) {
          result.moves(result.moves.length - 1 - gameState.playedMoves.length).some
        } else {
          None
        }
    }

  def gameInterpreterIORef(initial: SearchState[Unit]): IO[Game[IO, Move, BoardPosition, Int, Unit]] =
    for {
      ref <- Ref.of[IO, SearchState[Unit]](initial)
    } yield
      new Game[IO, Move, BoardPosition, Int, Unit] {
        def applyMove(move: Move): IO[Unit] =
          ref.update { searchState =>
            val nextPosition = SameGame.applyMove(move, searchState.gameState.position)
            val gameState = searchState.gameState.copy(
              position = nextPosition,
              score = SameGame.score(nextPosition),
              playedMoves = move :: searchState.gameState.playedMoves
            )
            searchState.copy(gameState = gameState)
          }

        def legalMoves: IO[List[Move]] =
          ref.get.map(searchState => SameGame.legalMoves(searchState.gameState.position))

        private def rndInt(bound: Int): IO[Int] =
          IO(scala.util.Random.nextInt(bound))

        def simulation: IO[Unit] = {
          val playRndLegalMove = for {
            moves <- legalMoves
            isTerminalPosition <- moves match {
              case Nil => true.pure[IO]
              case ms  => rndInt(ms.length).flatMap(i => applyMove(ms(i))).as(false)
            }
          } yield isTerminalPosition

          playRndLegalMove.iterateUntil(identity).void
        }

        def gameState: IO[GameState[Position, BoardPosition, Int]] =
          ref.get.map(_.gameState)

        def bestSequence: IO[Option[Result[Position, Int]]] =
          ref.get.map(_.bestSequence)

        def bestTotal: IO[Option[Result[Position, Int]]] =
          ref.get.map(_.bestTotal)

        def updateGameState(gameState: GameState[Position, BoardPosition, Int]): IO[Unit] =
          ref.update(_.copy(gameState = gameState))

        def updateBestTotal(bestTotal: Result[Position, Int]): IO[Unit] =
          ref.update(_.copy(bestTotal = bestTotal.some))

        def updateBestSequence(bestSequence: Option[Result[Position, Int]]): IO[Unit] =
          ref.update(_.copy(bestSequence = bestSequence))

        def isPrefixOf(gameState: GameState[Position, BoardPosition, Int])(result: Result[Position, Int]): Boolean =
          ListUtils.isSuffixOf(gameState.playedMoves, result.moves)(Eq.fromUniversalEquals)

        def next(gameState: GameState[Position, BoardPosition, Int], result: Result[Position, Int]): Option[Position] =
          if (isPrefixOf(gameState)(result) && gameState.playedMoves.length < result.moves.length) {
            result.moves(result.moves.length - 1 - gameState.playedMoves.length).some
          } else {
            None
          }

      }

  def loggerState: Logger[StateIO] =
    new Logger[StateIO] {
      def log[T: Show](t: T): StateT[IO, SearchState[Seed], Unit] =
        StateT[IO, SearchState[Seed], Unit](s => IO(println(t.show)).map((s, _)))
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

  implicit val showMove: Show[Move] =
    Show.show(p => show"(${p.col}, ${p.row})")

  implicit val showList: Show[List[Move]] =
    Show.show(_.map(_.show).mkString("[", ", ", "]"))

  implicit val showResult: Show[Result[Move, Int]] =
    Show.show(result => show">>> Improved sequence found\n>>> Score: ${result.score.show}, Moves: ${result.moves.reverse.show}")

  implicit val showBoard: Show[Board] =
    Show.show(_.columns.map(col => col.cells.map(_.show).reverse).transpose.map(_.mkString("[", ",", "]")).mkString("\n"))

  implicit val showGame: Show[BoardPosition] = Show.show {
    case InProgress(board, score) => show"$board\n\nScore: $score (game in progress)"
    case Finished(board, score)   => show"$board\n\nScore: $score (game finished)"
  }

  implicit val showGameState: Show[GameState[Move, BoardPosition, Int]] = Show.show(t => show"""
       |${t.position}
       |
       |Moves: ${t.playedMoves.reverse}
       |""".stripMargin)

  val showGameStateAsQueryParams: Show[GameState[Move, BoardPosition, Int]] =
    Show.show(t => show"""Moves: ${t.playedMoves.reverse.map(p => s"move=${p.col}%2C${p.row}").mkString("&")}
                   |
                   |Score: ${t.score}
                   |""".stripMargin)

  val showGameStateAsJsFunctionCalls: Show[GameState[Move, BoardPosition, Int]] =
    Show.show(t => show"""Moves: ${t.playedMoves.reverse.map(p => s"sg_remove(${p.col},${14 - p.row})").mkString(";")}
                  |
                  |Score: ${t.score}
                  |""".stripMargin)

  implicit val positionEq: Eq[Move] = Eq.fromUniversalEquals
}
