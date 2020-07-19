package part2actors

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import part2actors.TheFirstExercise.Person.LiveTheLife

object TheFirstExercise extends App {
  /**
   * Exercise
   * 1. a Counter actor
   *   - Increment
   *   - Decrement
   *   - Print
   **/

  val actorSystem = ActorSystem("actorSystem")

  // Domain of the counter
  object Counter {

    case object Increment

    case object Decrement

    case object Print

  }

  class Counter extends Actor {

    import Counter._

    var count = 0

    override def receive: Receive = {
      case Increment => count += 1
      case Decrement => count -= 1
      case Print => println(s"[counter] My current count is $count")
    }
  }

  import Counter._

  val counter = actorSystem.actorOf(Props[Counter], "myCounter")

  (1 to 5).foreach(_ => counter ! Increment)
  (1 to 3).foreach(_ => counter ! Decrement)
  counter ! Print

  /**
   *  2. a Bank account as an actor
   * receives
   *    - Deposit an amount
   *    - WithDraw an amount
   *    - Statement
   * replies with
   *    - Success
   *    - Failure
   *
   * interact with some other kind of actor
   **/

  object BankAccount {

    case class Deposit(amount: Int)

    case class Withdraw(amount: Int)

    case class Statement(amount: Int)

    case class TransactionSuccess(message: String)

    case class TransactionFailure(reason: String)

  }

  class BankAccount extends Actor {

    import BankAccount._

    var fund = 0

    override def receive: Receive = {
      case Deposit(amount) =>
        if (amount < 0) sender() ! TransactionFailure("invalid deposit amount")
        else {
          fund += amount
          sender() ! TransactionSuccess(s"successfully deposited $amount")
        }
      case Withdraw(amount) =>
        if (amount < 0) sender() ! TransactionFailure("invalid withdraw amount")
        else if (amount > fund) sender() ! TransactionFailure(s"insufficient funds")
        else {
          fund -= amount
          sender() ! TransactionSuccess(s"successfully withdrew $amount")
        }
      case Statement => sender() ! s"Your balance is $fund"
    }
  }

  object Person {

    case class LiveTheLife(account: ActorRef)

  }

  class Person extends Actor {

    import BankAccount._
    import Person._

    override def receive: Receive = {
      case LiveTheLife(account) =>
        account ! Deposit(10000)
        account ! Withdraw(90000)
        account ! Withdraw(500)
        account ! Statement
      case message => println(message.toString)
    }
  }

  val account = actorSystem.actorOf(Props[BankAccount], "bankAccount")
  val person = actorSystem.actorOf(Props[Person], "billionaire")

  person ! LiveTheLife(account)
}
