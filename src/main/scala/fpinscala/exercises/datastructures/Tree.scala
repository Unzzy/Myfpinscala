package fpinscala.exercises.datastructures

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

  def size: Int = this match
    case Leaf(_) => 1
    case Branch(l, r) => 1 + l.size + r.size

  def depth: Int = this match
    case Leaf(_) => 0
    case Branch(l, r) => Math.max(1 + l.depth, 1 + r.depth)

  def map[B](f: A => B): Tree[B] = this match
    case Leaf(i) => Leaf(f(i))
    case Branch(l, r) => Branch(l.map(f), r.map(f))

  def fold[B](f: A => B, g: (B,B) => B): B = this match
    case Leaf(i) => f(i)
    case Branch(l, r) => g(l.fold(f, g), r.fold(f, g))

  def sizeViaFold: Int = fold(_ => 1, 1 + _ + _)
  
  def depthViaFold: Int = fold(_ => 0, 1 + _ max _ + 1)
  
  def mapViaFold[B](f: A => B): Tree[B] = fold(i => Leaf(f(i)), (l, r) => Branch(l, r))

object Tree:

  def size[A](t: Tree[A]): Int = t match
    case Leaf(_) => 1
    case Branch(l,r) => 1 + size(l) + size(r)

  extension (t: Tree[Int]) def firstPositive: Int = t match
    case Leaf(i) => i
    case Branch(l, r) => 
      val lPos = l.firstPositive
      if lPos > 0 then lPos else r.firstPositive

    case _ => sys.error("This tree dont have positive numbers")

  extension (t: Tree[Int]) def maximum: Int = t match
    case Leaf(i) => i
    case Branch(l, r) => Math.max(l.maximum, r.maximum)

  extension (t: Tree[Int]) def maximumViaFold: Int = t.fold(i => i,  (l, r) => l max r)
