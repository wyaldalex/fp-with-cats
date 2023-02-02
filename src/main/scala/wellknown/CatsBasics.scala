package wellknown

import cats._
import cats.implicits._


object CatsBasics extends App {
  case class Account(id: Long, number: String, balance: Double, owner: String)

  object Account {

    implicit val universalEq: Eq[Account] = Eq.fromUniversalEquals
    implicit def orderById(implicit orderLong: Order[Long]) : Order[Account] = Order.from((a1,a2) => orderLong.compare(a1.id,a2.id))

    object Instances {

      implicit val eQbyIdEq: Eq[Account] = Eq.instance((a1,a2) => Eq[Long].eqv(a1.id,a2.id))
      implicit val eQbyIdEq2: Eq[Account] = Eq.by(account => account.id)
      implicit val eQbyNumber: Eq[Account] = Eq.instance((a1,a2) => Eq[String].eqv(a1.number,a2.number))

      implicit val orderByNumber: Order[Account] = Order.by(account => account.id)
      //implicit val orderByBalance2: Order[Account] = Order.by(account => account.balance)
      implicit def orderByBalance(implicit orderDouble: Order[Double]): Order[Account] = Order.by(account => account.balance)
    }
  }

  val account1 = Account(1,"123-567",1000,"Leandro")
  val account2 = Account(2,"123-567",1900,"John")
  val account3 = Account(3,"123-567",50,"John")

  println(Account.Instances.eQbyNumber.eqv(account1,account2))
  println(Account.Instances.eQbyIdEq.eqv(account1,account2))
//
//
//  //shorthand
//  import Account.Instances.eQbyNumber
//   println(account2 === account1)
//
//
  def sort[A](list: List[A])(implicit orderA: Order[A]) = {
    list.sorted(orderA.toOrdering)
  }
//
  val unOrderedList = List(account1,account2,account3)
//  import Account.Instances.orderByBalance
//  println(sort[Account](unOrderedList))
//
//  println(account1 compare account3)
//  println(account1 min account3)
//  println(account1 max account3)

  implicit val orderByBalanceDesc: Order[Account] = Order.reverse(Account.Instances.orderByBalance)
  println(sort[Account](unOrderedList))


}

