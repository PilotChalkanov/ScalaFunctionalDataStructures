
import Either.{Left, Right}

import scala.util.control.NonFatal

def mean(xs: Seq[Double]): Either[String, Double] =
  if xs.isEmpty then
    Left("mean of empty list!")
  else
    Right(xs.sum / xs.length)

def safeDiv(x: Int, y: Int): Either[Throwable, Int] =
  try Right(x/y)
  catch case NonFatal(t) => Left(t)

def catchNonFatal[A](a: => A): Either[Throwable, A] =
  try Right(a)
  catch case NonFatal(t) => Left(t)