package scalaz.zio

class RefMSpec extends AbstractRTSSpec {

  def is = "RefMSpec".title ^ s2"""
   Create a new RefM with a specified value and check if:
      `read` returns the current value.                                                        $e1
      `write` puts the new value correctly.                                                    $e2
      `modify` changes the value and returns the updated value.                                $e3
      `modifyFold` changes the value and returns another value computed from the modification. $e4
    """

  val (current, update) = ("value", "new value")

  val modReturnValue  = "hello"
  val effectFulUpdate = IO.sync({ println(s"new refM value = $update"); update })
  val effectFulMod    = IO.sync({ println(s"new refM value = $update"); (modReturnValue, update) })

  def e1 =
    unsafeRun(
      for {
        ref   <- RefM(current)
        value <- ref.get
      } yield value must beTheSameAs(current)
    )

  def e2 =
    unsafeRun(
      for {
        ref   <- RefM(current)
        _     <- ref.set(update)
        value <- ref.get
      } yield value must beTheSameAs(update)
    )

  def e3 =
    unsafeRun(
      for {
        ref   <- RefM(current)
        value <- ref.update(_ => effectFulUpdate)
      } yield value must beTheSameAs(update)
    )

  def e4 =
    unsafeRun(
      for {
        ref   <- RefM(current)
        r     <- ref.modify[String](_ => effectFulMod)
        value <- ref.get
      } yield (r must beTheSameAs(modReturnValue)) and (value must beTheSameAs(update))
    )
}
