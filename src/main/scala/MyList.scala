package com.tkroman.kpi.y2022.l1
import scala.annotation.tailrec
import scala.collection.mutable

enum MyList[+A]:
  case MyNil
  case MyCons(hd: A, tl: MyList[A])

  override def toString: String =
    @scala.annotation.tailrec
    def go(sb: StringBuilder, as: MyList[A]): String = {
      as match {
        case MyNil =>
          sb.result
        case MyCons(h, t) =>
          go(
            sb
              .append(h)
              .append(if t == MyNil then "]" else ", "),
            t
          )
      }
    }
    go(new StringBuilder("["), this)

object MyList:
  def apply[A](xs: A*): MyList[A] = of(xs*)
  def of[A](xs: A*): MyList[A] =
    xs.foldRight(MyNil: MyList[A]) { case (x, acc) => MyCons(x, acc) }

import MyList.*

def maxBy[A](xs: MyList[A], f: A => Int): Option[A] =
  @tailrec
  def go(xs: MyList[A], f: A => Int, maxN: Int, repeated: Boolean, max: A): Option[A] =
    xs match
      case MyNil =>
        if (repeated)
          None
        else
          Some(max)
      case MyCons(hd, tl) =>
        if (f(hd) == maxN)
          go(tl, f, maxN, true, hd)
        else
          if (f(hd) > maxN)
            go(tl, f, f(hd), false, hd)
          else
            go(tl, f, maxN, repeated, max)
  xs match
    case MyNil => None
    case MyCons(hd, tl) => go(tl, f, f(hd), false, hd)

@tailrec
def move[A](xs: MyList[A], ys: MyList[A]): MyList[A] =
  xs match
    case MyNil => ys
    case MyCons(hd, tl) => move(tl, MyCons(hd, ys))

def reverse[A](xs: MyList[A]): MyList[A] =
  move(xs, MyNil)

def prefixes[A](xs: MyList[A]): MyList[MyList[A]] =
  @tailrec
  def go[A](xs: MyList[A], buffer: MyList[MyList[A]]): MyList[MyList[A]] =
    xs match
      case MyNil => buffer
      case MyCons(hd, tl) => go(tl, MyCons(move(xs, MyNil), buffer))
  go(reverse(xs), MyNil)

def repeat[A](n: Int, a: A): MyList[A] =
  @tailrec
  def go(n: Int, a: A, buffer: MyList[A]): MyList[A] =
    if (n == 0)
      buffer
    else
      go(n - 1, a, MyCons(a, buffer))
  go(n, a, MyNil)

def cycle[A](n: Int, xs: MyList[A]): MyList[A] =
  @tailrec
  def go(n: Int, xs: MyList[A], buffer: MyList[A]): MyList[A] =
    if (n == 0)
      buffer
    else
      go(n - 1, xs, move(xs, buffer))
  go(n, move(xs, MyNil), MyNil)

@main
def main(): Unit = {
  println("Hello world!")
}