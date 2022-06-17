package com.tkroman.kpi.y2022.l1

import munit.FunSuite
import com.tkroman.kpi.y2022.l1.*
import MyList.*

class MyListSuite extends FunSuite {
  test("maxBy on Nil") {
    val expected = None
    val actual = maxBy(MyNil: MyList[Int], x => x)
    assertEquals(actual, expected)
  }
  test("maxBy on Cons 1") {
    val expected = Some(9)
    val actual = maxBy(MyList(5,4,2,6,3,4,9,3,4), x => x)
    assertEquals(actual, expected)
  }
  test("maxBy on Cons 2") {
    val expected = None
    val actual = maxBy(MyList(5,4,2,9,3,4,9,3,4), x => x)
    assertEquals(actual, expected)
  }
  test("reverse on Nil") {
    val expected = MyNil
    val actual = reverse(MyNil)
    assertEquals(actual, expected)
  }
  test("reverse on Cons") {
    val expected = MyList(5,4,3,2,1)
    val actual = reverse(MyList(1,2,3,4,5))
    assertEquals(actual, expected)
  }
  test("prefixes on Nil") {
    val expected = MyNil
    val actual = prefixes(MyNil)
    assertEquals(actual, expected)
  }
  test("prefixes on Cons") {
    val expected = MyList(MyList(1),MyList(1,2),MyList(1,2,3))
    val actual = prefixes(MyList(1,2,3))
    assertEquals(actual, expected)
  }
  test("repeat on 0") {
    val expected = MyNil
    val actual = repeat(0, 1)
    assertEquals(actual, expected)
  }
  test("repeat on 5") {
    val expected = MyList(1,1,1,1,1)
    val actual = repeat(5, 1)
    assertEquals(actual, expected)
  }
  test("cycle on Nil 1") {
    val expected = MyNil
    val actual = cycle(5, MyNil)
    assertEquals(actual, expected)
  }
  test("cycle on Nil 2") {
    val expected = MyNil
    val actual = cycle(0, MyNil)
    assertEquals(actual, expected)
  }
  test("cycle on Cons 1") {
    val expected = MyList(1,3,1,3,1,3)
    val actual = cycle(3, MyList(1,3))
    assertEquals(actual, expected)
  }
  test("cycle on Cons 2") {
    val expected = MyNil
    val actual = cycle(0, MyList(1,3))
    assertEquals(actual, expected)
  }
}