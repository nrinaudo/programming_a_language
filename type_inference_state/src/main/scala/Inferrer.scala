package type_inference

case class Inferrer[A](run: InfState => Either[String, Inferrer.Result[A]]):
  def map[B](f: A => B): Inferrer[B] = Inferrer: state =>
    run(state).map: result =>
      result.map(f)

  def flatMap[B](f: A => Inferrer[B]): Inferrer[B] = Inferrer: state =>
    run(state).flatMap: result =>
      f(result.value).run(result.state)

object Inferrer:
  case class Result[A](state: InfState, value: A):
    def map[B](f: A => B): Result[B] = copy(value = f(value))

  def fail[A](err: String): Inferrer[A] = Inferrer: _ =>
    Left(err)

  def freshVar: Inferrer[TypeInf] = Inferrer: state =>
    Right(
      Result(
        state = state.copy(curr = state.curr + 1),
        value = TypeInf.Var(state.curr)
      )
    )

  def get(i: Int): Inferrer[Option[TypeInf]] = Inferrer: state =>
    Right(
      Result(
        state = state,
        value = state.get(i)
      )
    )

  def set(i: Int, t: TypeInf): Inferrer[Unit] = Inferrer: state =>
    Right(
      Result(
        state = state.set(i, t),
        value = ()
      )
    )

  def pure[A](a: A): Inferrer[A] = Inferrer: state =>
    Right(
      Result(
        state = state,
        value = a
      )
    )

  def from[A](e: Either[String, A]): Inferrer[A] = Inferrer: state =>
    e.map(a => Result(state, a))
