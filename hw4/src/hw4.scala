/**
 * Created by kate on 12.12.14.
 */
object hw4 extends App {

  object HListExample {

    trait Fold[Elem, Value] {
      type Apply <: Value

      def apply[N <: Elem, Acc <: Value](n: N, acc: Acc): Apply
    }

    sealed trait HList {
      def ::[E](v: E): HList

      def ++[T <: HList](v: T): HList

      type Foldr[Value, F <: Fold[Any, Value], I <: Value] <: Value

      def foldr[Value, F <: Fold[Any, Value], I <: Value](f: F, i: I): Foldr[Value, F, I]

      type Foldl[Value, F <: Fold[Any, Value], I <: Value] <: Value

      def foldl[Value, F <: Fold[Any, Value], I <: Value](f: F, i: I): Foldl[Value, F, I]

      def lengthR = foldr(Length, 0)

      def lengthL = foldl(Length, 0)
    }


    case class HCons[H, T <: HList](head: H, tail: T) extends HList {
      def ::[E](v: E) = HCons(v, this)

      def ++[L <: HList](v: L) = HCons(head, tail ++ v)

      type Foldr[Value, F <: Fold[Any, Value], I <: Value] = F#Apply

      def foldr[Value, F <: Fold[Any, Value], I <: Value](f: F, i: I): Foldr[Value, F, I] =
        f(head, tail.foldr[Value, F, I](f, i))

      type Foldl[Value, F <: Fold[Any, Value], I <: Value] = tail.Foldl[Value, F, F#Apply]

      def foldl[Value, F <: Fold[Any, Value], I <: Value](f: F, i: I): Foldl[Value, F, I] =
        tail.foldl[Value, F, F#Apply](f, f(head, i))
    }

    class HNil extends HList {
      def ::[T](v: T) = HCons(v, this)

      def ++[T <: HList](v: T) = v

      type Foldr[Value, F <: Fold[Any, Value], I <: Value] = I

      def foldr[Value, F <: Fold[Any, Value], I <: Value](f: F, i: I) = i


      type Foldl[Value, F <: Fold[Any, Value], I <: Value] = I

      def foldl[Value, F <: Fold[Any, Value], I <: Value](f: F, i: I) = i

      override def toString = "Nil"
    }

    case object HNil extends HNil

    case object Length extends Fold[Any, Int] {
      override type Apply = Int

      override def apply[N <: Any, Acc <: Int](n: N, acc: Acc): Apply = acc + 1
    }

  }

  //task4_1
  import hw4.HListExample._

  val list4 = 1 :: 2 :: 3 :: HNil
  val list5 = "a" :: "b" :: "c" :: "m" :: HNil

  var result = list4 ++ list5
  println("Prepnd list:\n" + result + "\n")

  //task4_2
  println("list length with foldr = " + result.lengthR)
  println("list length with foldl = " + result.lengthL)
}
