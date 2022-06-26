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
  def go(xs: MyList[A], f: A => Int, maxN: Int, max: Option[A]): Option[A] =
    xs match
      case MyNil => max
      case MyCons(hd, tl) =>
        if (f(hd) >= maxN)
          if (f(hd) > maxN)
            go(tl, f, f(hd), Some(hd))
          else
            go(tl, f, maxN, None)
        else
          go(tl, f, maxN, max)
  xs match
    case MyNil => None
    case MyCons(hd, tl) => go(tl, f, f(hd), Some(hd))

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
      case MyCons(hd, tl) => go(tl, MyCons(reverse(xs), buffer))
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
  go(n, reverse(xs), MyNil)

def traverse[A, B](xs: MyList[A], f: A => Either[String, B]): Either[String, MyList[B]] =
  xs match
    case MyNil => Right(MyNil)
    case MyCons(hd, tl) =>
      f(hd) match
        case Left(str) => Left(str)
        case Right(b) =>
          traverse(tl, f) match
            case Left(str) => Left(str)
            case Right(list) => Right(MyCons(b, list))

@main
def main(): Unit = {
  println("Hello world!")
}