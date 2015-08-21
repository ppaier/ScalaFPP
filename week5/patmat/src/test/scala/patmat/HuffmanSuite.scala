package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }

    test("count list") {
      val list1 = List('a')
      val list2 = list1.union(List('b'))
      val list3 = list2.union(List('a'))
      val list4 = list3.union(List('c'))
      val list5 = list4.union(List('b'))
      val listTimes = times(list5)
      assert(listTimes === List(('a',2),('b',2),('c',1)))
  }
  
  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("makeOrderedLeafList for some frequency table") {
    val l = makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3)))
    println(l)
    assert(l === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
    val l2 = until(singleton,combine)(l)    
    println(l2)
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }
  
 test("decode french code") {
    new TestTrees {
      println(decodedSecret)
    }
  }
 
 
  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("abbabbaabbbabbbbabba".toList)) === "abbabbaabbbabbbbabba".toList)
      assert(decode(frenchCode, encode(frenchCode)("huffmanestcool".toList)) === "huffmanestcool".toList)
      assert(encode(frenchCode)("huffmanestcool".toList) === secret)
    }
  }
  
    test("good encode") {
    new TestTrees {
      println(convert(frenchCode))
    }
  }
    
    test("good encode2") {
    new TestTrees {
      val msg = "The Huffman encoding of this message should be three hundred and fifty-two bits long".toList
      println(createCodeTree(msg))
      assert(encode(createCodeTree(msg))(msg).size === 352)
    }
    }
}
