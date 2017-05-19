import scala.util.Try

case class Message[A](m: A, b: Long)

case class Incoming(i: String)

type Pipeline = Message[Incoming] => Option[Long]

implicit def liftToM[A, B](f: A => B): Message[A] => Message[B] = x => x.copy(m = f(x.m))
implicit def optionLift[A, B](f: A => B): Option[A] => Option[B] = o => o.map(f)
implicit def flip[A,B](x: A => Message[Option[B]] ): A => Option[Message[B]] = 
  a => {
    val res = x(a)
    res.m match {
      case Some(m) => Some(Message(m, res.b))
      case _ => None
    }
  }

val decode: Incoming => Option[String] = in => Try(in.i.toLong).toOption.map(_ => in.i)
val serialize: Message[String] => String = x => x.m + x.b 
val publish: String => Long = _.toLong

val pip: Pipeline = 
    flip(decode) andThen
    serialize andThen
    publish


pip(Message(Incoming("2"), 2)) //results in Some(22)
pip(Message(Incoming(""), 3))  //results in None
