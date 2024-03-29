/*
Exercise 3.1: Many ways to model failure

The exercise was to rewrite the declaration of `AccountService` to use `Option` rather
than `Try` to model failure. The declaration of `transfer` can still compose the `credit`
and `debit` functions as `Option` implements the flatMap function.
 */

package fr_ddd.chapter3.ex1

import java.util.Date


trait AccountService[Account, Amount, Balance]{
  def open(no: String, name: String, openDate: Option[Date]): Option[Account]
  def close(account: Account, closeDate: Option[Date]): Option[Account]
  def debit(accpunt: Account, amount: Amount): Option[Account]
  def credit(account: Account, amount: Amount): Option[Account]
  def balance(account: Account): Option[Account]

  def transfer(from: Account, to: Account, amount: Amount): Option[(Account, Account, Amount)] = for {
    a <- debit(from, amount)
    b <- credit(to, amount)
  } yield (a, b, amount)
}